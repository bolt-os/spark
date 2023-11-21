// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

#![cfg(all(sbi, feature = "dev-nvme"))]

//! NVM Express

use crate::{
    dev::{
        block::{self, BlockIo},
        pcie, DeviceDriver,
    },
    io, page_align_up,
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
    #[cfg(feature = "dev-pcie")]
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
        return Err(anyhow!("failed to initialize controller"));
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
                "skipping namespace #{nsid} with metadata size {}",
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

struct ChainedPrpLists {
    lists: Vec<Box<PrpList>>,
}

#[repr(align(4096))]
struct PrpList {
    prps: [u64; 512],
    count: usize,
}

impl PrpList {
    fn new(addr: u64) -> Box<Self> {
        Box::new(Self {
            prps: {
                let mut prps = [0; 512];
                prps[0] = addr;
                prps
            },
            count: 1,
        })
    }

    fn push(&mut self, addr: u64) -> Option<Box<Self>> {
        match self.count {
            index @ ..=510 => {
                self.prps[index] = addr;
                self.count += 1;
                None
            }
            index @ 511 => {
                let new_list = Self::new(addr);
                self.prps[index] = new_list.prps.as_ptr().addr() as u64;
                self.count += 1;
                Some(new_list)
            }
            _ => panic!(),
        }
    }
}

impl ChainedPrpLists {
    fn new() -> Self {
        Self { lists: vec![] }
    }

    fn addr(&self) -> usize {
        self.lists[0].prps.as_ptr().addr()
    }

    fn push_addr(&mut self, addr: u64) {
        if let Some(list) = self.lists.last_mut() {
            if let Some(new_list) = list.push(addr) {
                self.lists.push(new_list);
            }
        } else {
            self.lists.push(PrpList::new(addr));
        }
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
                let remaining_len = r_len - align_space;
                if remaining_len > PAGE_SIZE {
                    // The transfer crosses >= 2 pages, a PRP List is needed.
                    let num_prps = page_align_up!(remaining_len) / PAGE_SIZE;
                    let mut prp_list = ChainedPrpLists::new();
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
