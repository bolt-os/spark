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
pub mod fdt;
pub mod fw_cfg;
pub mod pcie;

use ::fdt as libfdt;

use core::mem::size_of;
use libsa::extern_sym;

pub struct DeviceDriver {
    pub name: &'static str,
    pub probe_pci: Option<fn(&pcie::Device) -> crate::Result<()>>,
    pub probe_fdt: Option<fn(&libfdt::node::FdtNode) -> crate::Result<()>>,
}

pub fn device_drivers() -> &'static [DeviceDriver] {
    let drivers_start = extern_sym!(__start_device_drivers as DeviceDriver);
    let drivers_end = extern_sym!(__stop_device_drivers as DeviceDriver);
    let len = (drivers_end.addr() - drivers_start.addr()) / size_of::<DeviceDriver>();

    unsafe { core::slice::from_raw_parts(drivers_start, len) }
}

pub fn match_fdt_node(node: &libfdt::node::FdtNode, matches: &[&str]) -> bool {
    if let Some(compat) = node.compatible() {
        compat.all().any(|c| matches.contains(&c))
    } else {
        false
    }
}

pub fn init(fdt: &libfdt::Fdt) {
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
