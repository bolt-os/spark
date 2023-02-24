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

//! NVM Express

use crate::{
    dev::{
        block::{self, BlockIo},
        pcie, DeviceDriver,
    },
    io,
    vmm::PAGE_SIZE,
};
use alloc::sync::Arc;
use anyhow::anyhow;
use core::cmp;
use libsa::endian::u32_le;
use spin::Mutex;

mod controller;
mod identify;
mod queue;

use self::{
    controller::{Controller, DataPtr, IoCommand},
    identify::{IdentifyController, NvmCommandSet, NvmIdentifyNamespace},
};

#[used]
#[link_section = "device_drivers"]
static NVME_DRIVER: DeviceDriver = DeviceDriver {
    name: "nvme",
    probe_fdt: None,
    probe_pci: Some(pci_init),
};

/// Queue Select
///
/// The driver implements two sets of Submission and Completion queues: the required Admin queue
/// and, since all requests in the bootloader are synchronous, a single I/O queue.
#[derive(Clone, Copy, Debug)]
enum Queue {
    Admin = 0,
    Io = 1,
}

fn pci_init(dev: &pcie::Device) -> crate::Result<()> {
    if dev.ident.class != 1 || dev.ident.subclass != 8 {
        return Ok(());
    }

    let mmio_base = dev
        .bars()
        .next()
        .ok_or_else(|| anyhow!("missing BAR0"))
        .map(|bar| bar.read_addr() as *mut u32_le)?;

    dev.enable_bus_master();

    let Some(ctlr) = controller::Controller::initialize(mmio_base) else {
        return Err(anyhow!("failed to initialize controller"))
    };

    init_common(ctlr)
}

/// Common controller initialization
///
/// This function is called after transport-specific initialization is complete.
fn init_common(mut ctlr: Controller) -> crate::Result<()> {
    let ctlr_info = ctlr.identify::<IdentifyController>(None)?;

    let max_tx_size = ((1 << ctlr_info.mdts) * ctlr.capabilities().min_page_size()) as u64;

    let active_namespaces =
        ctlr.identify::<identify::IoCommandSetActiveNamespaceIdList<NvmCommandSet>>(None)?;

    let ctlr_arc = Arc::new(Mutex::new(ctlr));
    for nsid in active_namespaces.iter() {
        let mut ctlr = ctlr_arc.lock();

        let ns_info = match ctlr.identify::<NvmIdentifyNamespace>(Some(nsid.get())) {
            Ok(info) => info,
            Err(error) => {
                log::warn!("failed to identify namespace #{nsid}: {error}");
                continue;
            }
        };
        let lba_format = ns_info.lbaf[ns_info.flbas as usize];
        if lba_format.metadata_size() != 0 {
            log::warn!(
                "skipping namespace #{nsid} with metdata size {}",
                lba_format.metadata_size()
            );
            continue;
        }
        let block_size = lba_format.lba_data_size();

        drop(ctlr);
        let device = Box::new(Namespace {
            nsid: nsid.get(),
            controller: Arc::clone(&ctlr_arc),
            block_size,
            capacity: ns_info.nsze.get(),
            max_tx_blocks: max_tx_size / block_size as u64,
        });
        if let Err(error) = block::register(device) {
            log::error!("failed to register namespace #{nsid}: {error}");
            continue;
        }
    }

    Ok(())
}

/// A namespace in an NVM subsystem
#[derive(Debug)]
struct Namespace {
    controller: Arc<Mutex<controller::Controller>>,
    nsid: u32,
    block_size: usize,
    capacity: u64,
    max_tx_blocks: u64,
}

struct PrpList {
    lists: Vec<[u64; 512]>,
    count: usize,
}

impl PrpList {
    fn new() -> Self {
        Self {
            lists: vec![],
            count: 0,
        }
    }

    fn addr(&self) -> usize {
        self.lists[0].as_ptr().addr()
    }

    fn push_addr(&mut self, addr: u64) {
        let block = self.count / 512;
        let mut index = self.count % 512;
        if index == 0 {
            // need new block
            self.lists.push([0; 512]);
            if self.lists.len() > 1 {
                self.lists[block][index] = self.lists[block - 1][511];
                self.lists[block - 1][511] = self.lists[block].as_ptr().addr() as u64;
                index += 1;
                self.count += 1;
            }
        }
        self.lists[block][index] = addr;
        self.count += 1;
    }
}

impl BlockIo for Namespace {
    fn block_size(&self) -> u64 {
        self.block_size as u64
    }

    fn capacity(&self) -> u64 {
        self.capacity
    }

    fn read_blocks(&self, mut addr: u64, buf: &mut [u8]) -> crate::io::Result<()> {
        // Length of buffer must be a multiple of the block size.
        if buf.len() & (self.block_size - 1) != 0 {
            return Err(io::Error::InvalidArgument);
        }

        let mut blocks = buf.len() / self.block_size;
        let mut buf_offset = 0;
        while blocks > 0 {
            let buf_ptr = buf[buf_offset..].as_mut_ptr();
            let r_blocks = cmp::min(cmp::min(blocks, self.max_tx_blocks as usize), 0x10000);
            let r_len = r_blocks * self.block_size;

            let mut prp = [0u64; 2];
            prp[0] = buf_ptr.addr() as u64;
            let align_space = match buf_ptr.align_offset(PAGE_SIZE) {
                0 => PAGE_SIZE,
                x => x,
            };
            let _list = if align_space < r_len {
                // The transfer crosses at least 1 page boundary
                let start = buf_ptr.addr() + align_space;

                let remaning_len = r_len - align_space;
                if remaning_len > PAGE_SIZE {
                    // The tranfer crosses >= 2 pages, a PRP List is needed.
                    let num_prps = remaning_len / PAGE_SIZE;
                    let mut prp_list = PrpList::new();
                    for i in 0..num_prps {
                        prp_list.push_addr((start + i * PAGE_SIZE) as u64);
                    }
                    prp[1] = prp_list.addr() as u64;
                    Some(prp_list)
                } else {
                    if align_space > 0 {
                        prp[1] = start as u64;
                    }
                    None
                }
            } else {
                None
            };

            let mut ctlr = self.controller.lock();
            ctlr.io_command(IoCommand::Read)
                .namespace_id(self.nsid)
                // .data_ptr(DataPtr::Prp(buf.as_mut_ptr().addr() as u64, 0))
                .data_ptr(DataPtr::Prp(prp[0], prp[1]))
                .cdw10(addr as u32)
                .cdw11((addr >> 32) as u32)
                .cdw12(r_blocks as u32 - 1)
                .execute()
                .map(|_| ())
                .map_err(|err| {
                    log::error!("error: {err:?}");
                    io::Error::DeviceError
                })?;

            blocks -= r_blocks;
            addr += r_blocks as u64;
            buf_offset += r_len;
        }

        Ok(())
    }
}
