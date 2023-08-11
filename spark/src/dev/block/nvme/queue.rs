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

use crate::{pages_for, pmm, size_of, vmm::PAGE_SIZE};
use core::ptr;
use libsa::endian::{u16_le, u32_le, u64_le};

#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct SubmissionQueueEntry {
    pub cdw0: u32_le,
    pub nsid: u32_le,
    pub cdw2: u32_le,
    pub cdw3: u32_le,
    pub mptr: u64_le,
    pub dptr: [u64_le; 2],
    pub cdw10: u32_le,
    pub cdw11: u32_le,
    pub cdw12: u32_le,
    pub cdw13: u32_le,
    pub cdw14: u32_le,
    pub cdw15: u32_le,
}

#[repr(C)]
pub struct CompletionQueueEntry {
    pub dw0: u32_le,
    pub dw1: u32_le,
    pub sq_head: u16_le,
    pub sq_ident: u16_le,
    pub cmd_ident: u16_le,
    pub status: u16_le,
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Clone, Copy, Debug)]
    pub struct CompletionStatus : u16 {
        const PHASE = 1 << 0;
    }
}

impl CompletionStatus {
    pub fn code_type(self) -> u8 {
        (self.bits() >> 9 & 7) as u8
    }

    pub fn code(self) -> u8 {
        (self.bits() >> 1) as u8
    }
}

/// A pair of Submission and Completion Queues
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub struct QueuePair {
    pub subq: *mut SubmissionQueueEntry,
    pub comq: *mut CompletionQueueEntry,
    len: u32,
    sub_idx: u32,
    com_idx: u32,
}

impl QueuePair {
    /// Allocate a new Queue Pair.
    ///
    /// Since all requests are synchronous, the minimum number of queue entries are allocated.
    pub fn new() -> Option<QueuePair> {
        let subq_size = size_of!(SubmissionQueueEntry);
        let comq_size = size_of!(CompletionQueueEntry);
        let subq_frames = pages_for!(subq_size);
        let comq_frames = pages_for!(comq_size);
        let qpair_frames = subq_frames + comq_frames;

        pmm::alloc_frames(qpair_frames).map(|base_addr| {
            let subq_base = base_addr;
            let comq_base = base_addr + subq_frames * PAGE_SIZE;

            // We need to ensure all PHASE bits are cleared to `0`.
            unsafe { (base_addr as *mut u8).write_bytes(0, qpair_frames * PAGE_SIZE) };

            QueuePair {
                subq: subq_base as *mut SubmissionQueueEntry,
                comq: comq_base as *mut CompletionQueueEntry,
                // NOTE: Apparently some controllers will misbehave if the submission
                // and completion queues do not contain the same number of entries.
                len: ((subq_frames * PAGE_SIZE) / subq_size) as _,
                sub_idx: 0,
                com_idx: 0,
            }
        })
    }

    pub const fn len(&self) -> usize {
        self.len as _
    }

    pub fn submit(&mut self, cmd: SubmissionQueueEntry) -> (u16, u16, bool) {
        unsafe { self.subq.add(self.sub_idx as usize).write_volatile(cmd) };
        self.sub_idx = (self.sub_idx + 1) % self.len;

        let com_idx = self.com_idx;
        let phase = unsafe { self.comq.add(com_idx as _).read_volatile().status.get() };
        (self.com_idx as _, self.sub_idx as _, phase & 0x1 != 0)
    }

    pub fn complete<'a>(&mut self) -> (&'a CompletionQueueEntry, u16) {
        let old_head = self.com_idx;
        self.com_idx = (self.com_idx + 1) % self.len;
        (unsafe { &*self.comq.add(old_head as _) }, self.com_idx as _)
    }

    pub fn completion_status(&self, com_idx: u16) -> CompletionStatus {
        unsafe {
            let bits = ptr::addr_of!((*self.comq.add(com_idx as _)).status).read_volatile();
            CompletionStatus::from_bits_retain(bits.get())
        }
    }
}

impl Drop for QueuePair {
    fn drop(&mut self) {
        let subq_size = size_of!(SubmissionQueueEntry);
        let comq_size = size_of!(CompletionQueueEntry);
        let subq_frames = pages_for!(subq_size);
        let comq_frames = pages_for!(comq_size);
        let qpair_frames = subq_frames + comq_frames;

        unsafe { pmm::free_frames(self.subq.addr(), qpair_frames) };
    }
}
