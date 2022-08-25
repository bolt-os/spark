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

use crate::{pages_for, pmm};
use core::{alloc::Layout, cmp};

use bar::{Bar, BarKind};

mod bar {
    use super::Ecam;

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum BarKind {
        None,
        Memory32,
        Memory64,
        Io,
    }

    /// Base Address Register (BAR)
    pub struct Bar<'ecam, K = BarKind> {
        ecam: &'ecam Ecam,
        bar_index: usize,
        kind: K,
        layout: usize,
    }

    impl<'ecam> Bar<'ecam> {
        pub(super) fn new(ecam: &'ecam Ecam, bar_index: usize) -> Bar<'ecam> {
            debug_assert!(bar_index <= 5);
            let (kind, layout) = get_bar_layout(ecam, bar_index);
            Self {
                ecam,
                bar_index,
                kind,
                layout,
            }
        }

        pub fn bar_index(&self) -> usize {
            self.bar_index
        }

        pub fn kind(&self) -> BarKind {
            self.kind
        }

        pub fn layout(&self) -> usize {
            self.layout
        }

        /// Set the address for this BAR
        ///
        /// # Safety
        ///
        /// The caller must ensure that the provided address is appropriate for this BAR.
        /// Specifically, it must conform to the BAR's alignment and, if a 32-bit BAR, the
        /// value must not exceed [`u32::MAX`].
        pub unsafe fn set_address(&self, addr: usize) {
            let bar_offset = 4 * (4 + self.bar_index);
            match self.kind {
                BarKind::None => panic!(),
                BarKind::Memory64 => self.ecam.write_u64(bar_offset, addr as u64),
                BarKind::Io | BarKind::Memory32 => self.ecam.write_u32(bar_offset, addr as u32),
            }
        }
    }

    fn get_bar_layout(ecam: &Ecam, bar_index: usize) -> (BarKind, usize) {
        let bar_offset = 4 * (4 + bar_index);
        let prev = ecam.read_u32(bar_offset);

        if prev == 0 {
            // Unimplemented BARs are hardwired zero
            (BarKind::None, 0)
        } else if prev & 0x1 == 1 {
            ecam.write_u32(bar_offset, !0);
            let size = !(ecam.read_u32(bar_offset) & !0x3) + 1;
            ecam.write_u32(bar_offset, prev);
            if size > 0 {
                (BarKind::Io, size as usize)
            } else {
                panic!();
            }
        } else {
            match (prev >> 1) & 0x3 {
                0x0 => {
                    ecam.write_u32(bar_offset, !0);
                    let size = !(ecam.read_u32(bar_offset) & !0xf) + 1;
                    ecam.write_u32(bar_offset, prev);
                    if size > 0 {
                        (BarKind::Memory32, size as usize)
                    } else {
                        panic!();
                    }
                }
                0x2 => {
                    let prev = ecam.read_u64(bar_offset);
                    ecam.write_u64(bar_offset, !0);
                    let size = !(ecam.read_u64(bar_offset) & !0xf) + 1;
                    ecam.write_u64(bar_offset, prev);

                    if size > 0 {
                        (BarKind::Memory64, size as usize)
                    } else {
                        panic!();
                    }
                }
                _ => panic!(),
            }
        }
    }

}

pub struct Ecam {
    mmio_base: *mut u32,
    addr: (u8, u8, u8),
}

impl Ecam {
    pub unsafe fn new(mmio_base: *mut u8, addr: (u8, u8, u8)) -> Ecam {
        Self {
            mmio_base: mmio_base.cast(),
            addr,
        }
    }

    fn read_register(&self, reg: usize) -> u32 {
        unsafe { self.mmio_base.add(reg).read_volatile() }
    }

    fn write_register(&self, reg: usize, val: u32) {
        unsafe { self.mmio_base.add(reg).write_volatile(val) }
    }

    fn read_u8(&self, offset: usize) -> u8 {
        let reg = offset / 4;
        let shift = (offset % 4) * 8;

        (self.read_register(reg) >> shift) as _
    }

    fn read_u16(&self, offset: usize) -> u16 {
        assert!(offset % 2 == 0);
        let reg = offset / 4;
        let offset = offset / 2;
        let shift = (offset % 4) * 16;

        (self.read_register(reg) >> shift) as _
    }

    fn read_u32(&self, offset: usize) -> u32 {
        assert!(offset % 4 == 0);
        self.read_register(offset / 4)
    }

    fn write_u32(&self, offset: usize, val: u32) {
        assert!(offset % 4 == 0);
        self.write_register(offset / 4, val);
    }

    fn read_u64(&self, offset: usize) -> u64 {
        assert!(offset % 4 == 0);
        let low = self.read_register(offset / 4) as u64;
        let high = self.read_register((offset / 4) + 1) as u64;

        (high << 32) | low
    }

    fn write_u64(&self, offset: usize, val: u64) {
        assert!(offset % 4 == 0);
        let reg = offset / 4;

        let low = val as u32;
        let high = (val >> 32) as u32;

        self.write_register(reg, low);
        self.write_register(reg + 1, high);
    }

    fn bars(&self) -> impl Iterator<Item = Bar<'_>> {
        let mut bar_index = 0;
        let max_bar_index = 5; // This depends on the type!!!!!

        core::iter::from_fn(move || {
            if bar_index > max_bar_index {
                return None;
            }

            let bar = Bar::new(self, bar_index);

            bar_index += if bar.kind() == BarKind::Memory64 { 2 } else { 1 };

            Some(bar)
        })
    }
}

fn device_kind(class: u8, subclass: u8, prog_if: u8) -> &'static str {
    let mut s = "<unknown>";

    #[allow(clippy::single_match)]
    match class {
        0x1 => match subclass {
            0x8 => match prog_if {
                0x2 => s = "NVMe Controller",
                _ => {}
            },
            _ => {}
        },
        0x6 => match subclass {
            0x0 => s = "Host Bridge Controller",
            _ => {}
        },
        _ => {}
    }

    s
}

pub struct HostBridge {
    start_bus: u8,
    end_bus: u8,
    mmio_base: usize,
}

impl HostBridge {
    fn get_ecam(&self, bus: u8, dev: u8, func: u8) -> Option<Ecam> {
        if bus < self.start_bus || self.end_bus < bus {
            return None;
        }

        let ecam_offset = (((bus - self.start_bus) as usize) << 20)
            | ((dev as usize) << 15)
            | ((func as usize) << 12);

        let ecam = unsafe {
            Ecam::new(
                (self.mmio_base as *mut u8).add(ecam_offset),
                (bus, dev, func),
            )
        };

        let dev_vendor = ecam.read_register(0x0);
        if (dev_vendor >> 16) == 0xffff || (dev_vendor & 0xffff) == 0xffff {
            None
        } else {
            Some(ecam)
        }
    }
}

fn busdev_iter() -> impl Iterator<Item = (u8, u8)> {
    let mut bus = 0;
    let mut dev = 0;
    core::iter::from_fn(move || {
        if bus == 255 {
            return None;
        }

        let r = (bus, dev);

        dev += 1;
        if dev == 32 {
            dev = 0;
            bus += 1;
        }

        Some(r)
    })
}

fn alloc_resources(_host: &HostBridge, dev_ecam: &Ecam) {
    for bar in dev_ecam.bars() {
        if bar.layout() == 0 {
            continue;
        }

        if bar.kind() != BarKind::Memory64 {
            todo!();
        }

        let mmio_base = pmm::alloc_frames_aligned(pages_for!(bar.layout()), bar.layout()).unwrap();
        println!(
            "pcie: allocting BAR{} for device {:02x}:{:02x}:{:02x}: mmio_base={:#018x}",
            bar.bar_index(), dev_ecam.addr.0, dev_ecam.addr.1, dev_ecam.addr.2, mmio_base,
        );
        unsafe { bar.set_address(mmio_base) };
    }
}

fn probe(host: &HostBridge, bus: u8, dev: u8, func: u8) {
    let Some(dev_ecam) = host.get_ecam(bus, dev, func) else { return };
    let vendor_id = dev_ecam.read_u16(0x0);
    let device_id = dev_ecam.read_u16(0x2);
    if vendor_id == !0 || device_id == !0 {
        return;
    }

    let header_type = dev_ecam.read_u8(0xe);
    let class = dev_ecam.read_u8(0xb);
    let subclass = dev_ecam.read_u8(0xa);
    let prog_if = dev_ecam.read_u8(0x9);
    println!(
        "  [{bus:02x}:{dev:02x}:{func:02x}]:[{vendor_id:#06x}:{device_id:#06x}]: {}",
        device_kind(class, subclass, prog_if)
    );

    if header_type & 0x7f != 0 {
        return;
    }

    alloc_resources(host, &dev_ecam);
}

pub fn init_from_fdt(fdt: &fdt::Fdt) {
    println!("pcie: enumerating buses");
    for pci_node in fdt.find_all_nodes("/soc/pci") {
        assert!(pci_node
            .compatible()
            .unwrap()
            .all()
            .any(|c| c == "pci-host-ecam-generic"));

        struct IrqMap {
            dev: (u8, u8, u8),
            controller_phandle: u32,
            irq: usize,
            vector: u32,
        }

        let mut irq_map = vec![];
        let mut chunks = pci_node
            .property("interrupt-map")
            .unwrap()
            .value
            .chunks_exact(4)
            .map(|c| u32::from_be_bytes(c.try_into().unwrap()));
        while let Some(x) = chunks.next() {
            let y = chunks.next().unwrap();
            let z = chunks.next().unwrap();
            let intn = chunks.next().unwrap();
            let controller_phandle = chunks.next().unwrap();
            let vector = chunks.next().unwrap();

            let bus = (x >> 16) & 0xff;
            let dev = (x >> 11) & 0x1f;
            let func = (x >> 8) & 0x7;

            irq_map.push(IrqMap {
                dev: (bus as u8, dev as u8, func as u8),
                controller_phandle,
                vector,
                irq: intn as usize,
            });
        }

        let Some(mmio_window) = pci_node.reg().unwrap().next() else { continue };

        let bus_range = pci_node.property("bus-range").unwrap().value;
        assert_eq!(bus_range.len(), 2 * 4);

        println!("pcie: host bridge at {:p}", mmio_window.starting_address);

        let host_bridge = HostBridge {
            mmio_base: mmio_window.starting_address as _,
            start_bus: u32::from_be_bytes(bus_range[..4].try_into().unwrap()) as _,
            end_bus: u32::from_be_bytes(bus_range[4..].try_into().unwrap()) as _,
        };

        for (bus, dev) in busdev_iter() {
            if let Some(dev_ecam) = host_bridge.get_ecam(bus, dev, 0) {
                let func_max = cmp::max(1, (dev_ecam.read_u8(0xe) & 0x80) >> 4);
                for func in 0..func_max {
                    probe(&host_bridge, bus, dev, func);
                }
            }
        }
    }
    println!("pcie: done");
}
