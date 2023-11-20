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

pub mod acpi;
pub mod block;
pub mod fdt;
pub mod fw_cfg;
pub mod pcie;
pub mod uart;

#[cfg(sbi)]
use {::fdt as libfdt, core::mem::size_of, libsa::extern_sym};
#[cfg(uefi)]
use {core::fmt, spin::Mutex, uefi::proto::Proto};

#[cfg(sbi)]
pub struct DeviceDriver {
    pub name: &'static str,
    #[cfg(all(sbi, feature = "dev-pcie"))]
    pub probe_pci: Option<fn(&pcie::Device) -> crate::Result<()>>,
    pub probe_fdt: Option<fn(&libfdt::node::FdtNode) -> crate::Result<()>>,
}

#[cfg(sbi)]
pub fn device_drivers() -> &'static [DeviceDriver] {
    let drivers_start = extern_sym!(__start_device_drivers as DeviceDriver);
    let drivers_end = extern_sym!(__stop_device_drivers as DeviceDriver);
    let len = (drivers_end.addr() - drivers_start.addr()) / size_of::<DeviceDriver>();

    unsafe { core::slice::from_raw_parts(drivers_start, len) }
}

#[cfg(sbi)]
pub fn match_fdt_node(node: &libfdt::node::FdtNode, matches: &[&str]) -> bool {
    if let Some(compat) = node.compatible() {
        compat.all().any(|c| matches.contains(&c))
    } else {
        false
    }
}

#[cfg(sbi)]
pub fn init() {
    let Some(fdt) = fdt::get_fdt() else { return };
    log::debug!("scanning device tree");
    for node in fdt.all_nodes() {
        for driver in device_drivers() {
            if let Some(init) = driver.probe_fdt {
                if let Err(error) = init(&node) {
                    log::error!("{}: {error}", driver.name);
                }
            }
        }
    }
}

#[cfg(uefi)]
pub fn init() {
    use uefi::proto::media::block_io::BlockIo as BlockIoProto;
    let bs = uefi::boot_services();

    let handles = bs
        .handles_by_protocol::<uefi::proto::media::block_io::BlockIo>()
        .unwrap();

    for handle in &*handles {
        let proto = bs.protocol_for_handle::<BlockIoProto>(*handle).unwrap();
        let media = proto.media();

        // Skip partitions.
        if media.logical_partition {
            continue;
        }

        let dev = Box::new(UefiBlockDevice {
            media_id: media.media_id,
            capacity: media.last_block + 1,
            block_size: media.block_size as u64,
            proto: Mutex::new(proto),
        });
        block::register(dev).unwrap();
    }
}

#[cfg(uefi)]
struct UefiBlockDevice {
    proto: Mutex<Proto<uefi::proto::media::block_io::BlockIo>>,
    media_id: u32,
    capacity: u64,
    block_size: u64,
}

#[cfg(uefi)]
unsafe impl Send for UefiBlockDevice {}
#[cfg(uefi)]
unsafe impl Sync for UefiBlockDevice {}

#[cfg(uefi)]
impl fmt::Debug for UefiBlockDevice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UefiBlockDevice")
            .field("media_id", &self.media_id)
            .field("capacity", &self.capacity)
            .field("block_size", &self.block_size)
            .finish()
    }
}

#[cfg(uefi)]
impl block::BlockIo for UefiBlockDevice {
    fn block_size(&self) -> u64 {
        self.block_size
    }

    fn capacity(&self) -> u64 {
        self.capacity
    }

    fn read_blocks(&self, lba: u64, buf: &mut [u8]) -> crate::io::Result<()> {
        let mut proto = self.proto.lock();
        proto.read_blocks(self.media_id, lba, buf)?;
        Ok(())
    }
}
