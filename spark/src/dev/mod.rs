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

pub mod block;
pub mod fw_cfg;
pub mod pcie;

use core::mem::size_of;
use libsa::extern_sym;

pub struct DeviceDriver {
    pub name: &'static str,
    pub fdt_compat: Option<&'static [&'static str]>,
    pub fdt_init: Option<fn(&fdt::Fdt, &fdt::node::FdtNode)>,
    pub pci_compat: Option<&'static [&'static pcie::DriverCompat]>,
    pub pci_init: Option<fn(&pcie::Device)>,
}

pub fn device_drivers() -> &'static [DeviceDriver] {
    let drivers_start = extern_sym!(__device_drivers as DeviceDriver);
    let drivers_end = extern_sym!(__end_device_drivers as DeviceDriver);
    let len = (drivers_end.addr() - drivers_start.addr()) / size_of::<DeviceDriver>();

    unsafe { core::slice::from_raw_parts(drivers_start, len) }
}

fn match_fdt_driver(node: &fdt::node::FdtNode) -> Option<&'static DeviceDriver> {
    let node_compat = node.compatible()?;

    device_drivers()
        .iter()
        .find(|driver| match driver.fdt_compat {
            Some(driver_compat) => node_compat
                .all()
                .any(|compat| driver_compat.contains(&compat)),
            None => false,
        })
}

pub fn init(fdt: &fdt::Fdt) {
    log::debug!("scanning device tree");
    for node in fdt.all_nodes() {
        if let Some(driver) = match_fdt_driver(&node) {
            log::debug!("found driver: {} for node `{}`", driver.name, node.name);
            if let Some(init_fn) = driver.fdt_init {
                init_fn(fdt, &node);
            } else {
                log::debug!("driver missing fdt init fn");
            }
        }
    }
}
