/*
 * Copyright (c) 2022 xvanc and contributors
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

use super::{
    queue::{CompletionQueueEntry, CompletionStatus, QueuePair, SubmissionQueueEntry},
    Queue,
};
use crate::{
    io, size_of,
    time::Timeout,
    vmm::{PAGE_SHIFT, PAGE_SIZE},
};
use core::time::Duration;
use libsa::{
    endian::{u32_le, u64_le},
    volatile::Volatile,
};

bitflags::bitflags! {
    /// Controller Capabilities
    #[repr(transparent)]
    pub(super) struct Capabilities : u64 {
        /// NVM Command Set is supported
        ///
        /// This command set should be implemented by all (I/O) controllers and provides
        /// basic read and write functionality.
        const NVM_COMMAND_SET   = 1 << 37;
        const BOOT_PARTITIONS   = 1 << 45;
    }
}

impl Capabilities {
    fn new(bits: u64) -> Capabilities {
        // SAFETY: We want to keep undefined fields.
        unsafe { Self::from_bits_unchecked(bits) }
    }

    /// Returns the controller timeout
    ///
    /// This is the maximum amount of time the driver should wait for [`Status::RDY`] to
    /// change state after `CC.EN` changes state.
    fn timeout(self) -> Duration {
        let ms = self.bits >> 24 & 0xff;
        // The value reported is in units of 500ms.
        Duration::from_millis(ms * 500)
    }

    fn doorbell_stride(self) -> usize {
        let dstrd = (self.bits >> 32) & 0xf;
        4 << dstrd
    }

    /// Returns the minimum host page size supported by the controller
    pub(super) fn min_page_size(self) -> usize {
        let mpsmin = (self.bits >> 48) & 0xf;
        0x1000 << mpsmin
    }

    /// Returns the maximum host page size supported by the controller
    fn max_page_size(self) -> usize {
        let mpsmax = (self.bits >> 52) & 0xf;
        0x1000 << mpsmax
    }
}

bitflags::bitflags! {
    /// Controller Status
    #[repr(transparent)]
    struct Status : u32 {
        /// Controller Ready
        ///
        /// This flag is set when the controller is ready to process commands.
        const RDY   = 1 << 0;
        /// Controller Fatal Status
        ///
        /// This flag is set when a fatal controller error occurs which cannot be reported
        /// in the appropriate completion queue.
        const CFS   = 1 << 1;
    }
}

impl Status {
    const fn new(bits: u32) -> Status {
        // SAFETY: We want to keep undefined fields.
        unsafe { Self::from_bits_unchecked(bits) }
    }
}

/// Memory-mapped registers exposed by a controller
#[repr(C)]
struct RegisterBlock {
    /// Controller Capabilities
    cap: Volatile<u64_le>,
    /// Version
    vs: Volatile<u32_le>,
    /// Interrupt Mask Set
    intms: Volatile<u32_le>,
    /// Interrupt Mask Clear
    intmc: Volatile<u32_le>,
    /// Controller Configuration
    cc: Volatile<u32_le>,
    rsvd0: Volatile<u32_le>,
    /// Controller Status
    csts: Volatile<u32_le>,
    /// NVM Subsystem Reset
    nssr: Volatile<u32_le>,
    /// Admin Queue Attributes
    aqa: Volatile<u32_le>,
    /// Admin Submission Queue Base Address
    asq: Volatile<u64_le>,
    /// Admin Completion Queue Base Address
    acq: Volatile<u64_le>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DataPtr {
    Prp(u64, u64),
    // Sgl(SglDataBlock),
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct SglDataBlock {
    pub addr: u64,
    pub len: u32,
    pub rsvd: [u8; 3],
    pub ident: u8,
}

/// An NVM Express Host Controller
#[derive(Debug)]
pub struct Controller {
    caps: Capabilities,
    reg_base: *mut u32_le,
    doorbell_base: *mut Volatile<u32_le>,
    admin_queue: QueuePair,
    io_queue: QueuePair,
}

unsafe impl Send for Controller {}

impl Controller {
    fn status(&self) -> Status {
        let bits = unsafe { self.reg_base.byte_add(0x1c).read_volatile().get() };
        Status::new(bits)
    }

    fn wait_ready(&mut self) -> io::Result<()> {
        let timeout = crate::time::Timeout::start(self.caps.timeout());

        loop {
            let status = self.status();

            if status.contains(Status::RDY) {
                return Ok(());
            }

            if status.contains(Status::CFS) {
                return Err(io::Error::DeviceError);
            }

            if timeout.expired() {
                return Err(io::Error::TimedOut);
            }
        }
    }

    pub(super) fn capabilities(&self) -> Capabilities {
        self.caps
    }

    pub fn initialize(base: *mut u32_le) -> Option<Controller> {
        let regs = unsafe { &mut *base.cast::<RegisterBlock>() };

        let caps = Capabilities::new(regs.cap.read().get());

        // Make sure the controller supports the page size and the NVM command set;
        // otherwise, there's nothing we're interested in here.
        if !caps.contains(Capabilities::NVM_COMMAND_SET)
            || caps.min_page_size() > PAGE_SIZE
            || caps.max_page_size() < PAGE_SIZE
        {
            return None;
        }

        // Disable the controller for configuration.
        regs.cc.write(0.into());

        // Options set with value 0:
        //  - Controller Ready Independent of Media: no
        //      CSTS.RDY will not be set until the connected devices are also ready.
        //  - Shutdown Notification: None
        //  - Arbitration Mechanism: Round Robin
        //      Anything else may not be supported and offers no benefit to a single queue.
        //  - I/O Command Set Selected: NVM Command Set
        let mut conf = 0u32;

        // Set Memory Page and I/O Queue Entry sizes.
        conf |= (PAGE_SHIFT - 12) << 7;
        conf |= size_of!(SubmissionQueueEntry).ilog2() << 16;
        conf |= size_of!(CompletionQueueEntry).ilog2() << 20;

        // Allocate the queues
        let admin_queue = QueuePair::new()?;
        let io_queue = QueuePair::new()?;

        regs.aqa
            .write(((admin_queue.len() - 1) as u32 * 0x00010001).into());
        regs.asq.write((admin_queue.subq.addr() as u64).into());
        regs.acq.write((admin_queue.comq.addr() as u64).into());

        let mut ctlr = Controller {
            caps,
            reg_base: base.cast(),
            doorbell_base: unsafe { base.byte_add(0x1000).cast() },
            admin_queue,
            io_queue,
        };

        // Enable the controller and wait for it to be ready.
        conf |= 0x1;
        regs.cc.write(conf.into());
        ctlr.wait_ready().ok()?;

        // Mask all interrupts.
        regs.intms.write((!0).into());

        // Query the Identify command for the controller and the NVM command set
        // determine optimal block size

        let ioq_len = ctlr.io_queue.len() as u32 - 1;
        let comq_addr = ctlr.io_queue.comq.addr() as u64;
        ctlr.admin_command(AdminCommand::CreateIoCompletionQueue)
            .data_ptr(DataPtr::Prp(comq_addr, 0))
            .cdw10((ioq_len << 16) | 0x0001)
            .cdw11(0x1)
            .execute()
            .inspect_err(|status| log::error!("failed to create i/o completion queue: {status:x}"))
            .ok()?;

        let subq_addr = ctlr.io_queue.subq.addr() as u64;
        ctlr.admin_command(AdminCommand::CreateIoSubmissionQueue)
            .data_ptr(DataPtr::Prp(subq_addr, 0))
            .cdw10((ioq_len << 16) | 0x0001)
            .cdw11(0x00010001)
            .execute()
            .inspect_err(|status| log::error!("failed to create i/o submission queue: {status:x}"))
            .ok()?;

        Some(ctlr)
    }

    /// Update the head pointer for Submission Queue `queue`.
    ///
    /// This notifies the controller that new requests have been submitted.
    fn submission_doorbell(&self, queue: Queue, new_head: u16) {
        unsafe {
            self.doorbell_base
                .byte_add((2 * queue as usize) * self.caps.doorbell_stride())
                .cast::<u32_le>()
                .write_volatile((new_head as u32).into());
        };
    }

    /// Update the head pointer for Completion Queue `queue`.
    ///
    /// This notifies the controller that completed requests have been acknowledged.
    fn completion_doorbell(&self, queue: Queue, new_head: u16) {
        unsafe {
            self.doorbell_base
                .byte_add((2 * queue as usize + 1) * self.caps.doorbell_stride())
                .cast::<u32_le>()
                .write_volatile((new_head as u32).into());
        }
    }

    fn queue(&self, queue: Queue) -> &QueuePair {
        match queue {
            Queue::Admin => &self.admin_queue,
            Queue::Io => &self.io_queue,
        }
    }

    fn queue_mut(&mut self, queue: Queue) -> &mut QueuePair {
        match queue {
            Queue::Admin => &mut self.admin_queue,
            Queue::Io => &mut self.io_queue,
        }
    }

    pub fn admin_command(&mut self, cmd: AdminCommand) -> CommandBuilder<'_> {
        CommandBuilder {
            ctlr: self,
            queue: Queue::Admin,
            cmd: SubmissionQueueEntry {
                cdw0: (cmd as u32).into(),
                ..Default::default()
            },
        }
    }

    pub fn identify<I: super::identify::Identify>(
        &mut self,
        nsid: Option<u32>,
    ) -> crate::Result<Box<I>> {
        super::identify::identify(self, nsid).map_err(|_| {
            anyhow::anyhow!(
                "NVMe Identify command failed: `{}`",
                core::any::type_name::<I>()
            )
        })
    }

    pub fn io_command(&mut self, cmd: IoCommand) -> CommandBuilder<'_> {
        CommandBuilder {
            ctlr: self,
            queue: Queue::Io,
            cmd: SubmissionQueueEntry {
                cdw0: (cmd as u32).into(),
                ..Default::default()
            },
        }
    }
}

impl Drop for Controller {
    fn drop(&mut self) {
        // Delete the I/O queues.
        self.admin_command(AdminCommand::DeleteIoSubmissionQueue)
            .cdw10(0x2)
            .execute()
            .ok();
        self.admin_command(AdminCommand::DeleteIoCompletionQueue)
            .cdw10(0x1)
            .execute()
            .ok();
    }
}

pub enum AdminCommand {
    DeleteIoSubmissionQueue = 0x00,
    CreateIoSubmissionQueue = 0x01,
    DeleteIoCompletionQueue = 0x04,
    CreateIoCompletionQueue = 0x05,
    Identify = 0x06,
}

pub enum IoCommand {
    Read = 0x02,
}

pub struct CommandBuilder<'ctlr> {
    ctlr: &'ctlr mut Controller,
    cmd: SubmissionQueueEntry,
    queue: Queue,
}

impl<'ctlr> CommandBuilder<'ctlr> {
    pub fn namespace_id(&mut self, nsid: u32) -> &mut Self {
        self.cmd.nsid = nsid.into();
        self
    }

    pub fn data_ptr(&mut self, data_ptr: DataPtr) -> &mut Self {
        self.cmd.dptr = match data_ptr {
            DataPtr::Prp(a, b) => [a.into(), b.into()],
            // DataPtr::Sgl(desc) => unsafe { core::mem::transmute(desc) },
        };
        self
    }

    pub fn cdw10(&mut self, cdw: u32) -> &mut Self {
        self.cmd.cdw10 = cdw.into();
        self
    }

    pub fn cdw11(&mut self, cdw: u32) -> &mut Self {
        self.cmd.cdw11 = cdw.into();
        self
    }

    pub fn cdw12(&mut self, cdw: u32) -> &mut Self {
        self.cmd.cdw12 = cdw.into();
        self
    }

    pub fn cdw14(&mut self, cdw: u32) -> &mut Self {
        self.cmd.cdw14 = cdw.into();
        self
    }

    pub fn execute(&mut self) -> Result<&'ctlr CompletionQueueEntry, CompletionStatus> {
        // write the command to the submission queue
        let (com_idx, new_head, phase) = self.ctlr.queue_mut(self.queue).submit(self.cmd.clone());
        // ring the doorbell
        self.ctlr.submission_doorbell(self.queue, new_head);

        let timeout = Timeout::start(Duration::from_secs(30));

        // wait for completion
        let status = loop {
            let status = self.ctlr.queue(self.queue).completion_status(com_idx);

            if status.contains(CompletionStatus::PHASE) != phase {
                break status;
            }

            if timeout.expired() {
                // TODO: Better errors, this is meaningless.
                return Err(status);
            }

            core::hint::spin_loop();
        };

        // Ring the completion doorbell.
        let (com_entry, new_head) = self.ctlr.queue_mut(self.queue).complete();
        self.ctlr.completion_doorbell(self.queue, new_head);

        if status.code() != 0 {
            log::debug!(
                "status.type = {}, status.code = {}\n{:#x?}",
                status.code_type(),
                status.code(),
                self.cmd,
            );

            return Err(status);
        }

        Ok(com_entry)
    }
}
