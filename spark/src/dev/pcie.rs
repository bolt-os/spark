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

#![allow(clippy::cast_possible_truncation)]

pub use device::{CommandRegister, Device, DeviceIdent};

use super::{device_drivers, DeviceDriver};
use crate::{pages_for, pmm};
use bar::BarKind;
use core::{fmt, ops::RangeInclusive};
use device::DeviceKind;
use ecam::Ecam;

mod bar {
    use super::device::Device;

    #[allow(clippy::module_name_repetitions)]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum BarKind {
        None,
        Memory32,
        Memory64,
        Io,
    }

    /// Base Address Register (BAR)
    pub struct Bar<'dev, K = BarKind> {
        dev: &'dev Device,
        bar_index: usize,
        kind: K,
        layout: usize,
    }

    impl<'dev> Bar<'dev> {
        pub(super) unsafe fn new(dev: &'dev Device, bar_index: usize) -> Bar<'dev> {
            debug_assert!(bar_index <= 5);

            let bar_offset = 4 * (4 + bar_index);
            let bar_value = unsafe { dev.ecam.read_u32(bar_offset) };

            let kind = if bar_value == 0 {
                // Unimplemented BARs are hardwired to zero.
                BarKind::None
            } else if bar_value & 0x1 == 1 {
                // If bit 0 is 1, the BAR is an I/O space BAR.
                BarKind::Io
            } else {
                // Otherwise, it is a memory space BAR.
                // The width of the register is specified in bits 2:1.
                match (bar_value >> 1) & 0x3 {
                    0x0 => BarKind::Memory32,
                    0x2 => BarKind::Memory64,
                    _ => panic!(),
                }
            };

            let layout = unsafe {
                match kind {
                    BarKind::None => 0,
                    BarKind::Memory64 => {
                        let bar_value = dev.ecam.read_u64(bar_offset);
                        dev.ecam.write_u64(bar_offset, !0);
                        let mask = dev.ecam.read_u64(bar_offset);
                        dev.ecam.write_u64(bar_offset, bar_value);

                        !(mask & !0xf) as usize + 1
                    }
                    BarKind::Memory32 | BarKind::Io => {
                        let ctrl_mask = if kind == BarKind::Io { 0x3 } else { 0xf };
                        dev.ecam.write_u32(bar_offset, !0);
                        let mask = dev.ecam.read_u32(bar_offset);
                        dev.ecam.write_u32(bar_offset, bar_value);

                        !(mask & !ctrl_mask) as usize + 1
                    }
                }
            };

            Self {
                dev,
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

        /// Read the BAR's value
        pub fn read(&self) -> usize {
            let bar_offset = 4 * (4 + self.bar_index);

            // SAFETY: It is `unsafe` to create a `Bar`, so we can assume that this is
            // a valid BAR for the associated device.
            unsafe {
                match self.kind() {
                    BarKind::None => {
                        // We shouldn't be trying to access this BAR if we already
                        // know it's not implemented.
                        panic!("attempt to call read on an unimplemented BAR");
                    }
                    BarKind::Memory32 | BarKind::Io => self.dev.ecam.read_u32(bar_offset) as usize,
                    BarKind::Memory64 => self.dev.ecam.read_u64(bar_offset) as usize,
                }
            }
        }

        /// Reads BAR's value with control bits masked
        pub fn read_addr(&self) -> usize {
            let mask = match self.kind() {
                BarKind::None => 0,
                BarKind::Memory32 | BarKind::Memory64 => 0xf,
                BarKind::Io => 0x3,
            };

            self.read() & !mask
        }

        /// Set the address for this BAR
        ///
        /// # Safety
        ///
        /// The caller must ensure that the provided address is appropriate for this BAR.
        /// Specifically, it must conform to the BAR's alignment and, if a 32-bit BAR, the
        /// value must not exceed [`u32::MAX`].
        pub unsafe fn write(&self, val: usize) {
            let bar_offset = 4 * (4 + self.bar_index);

            match self.kind() {
                BarKind::None => {
                    // We shouldn't be trying to access this BAR if we already
                    // know it's not implemented.
                    panic!("attempt to call read on an unimplemented BAR");
                }
                BarKind::Memory32 | BarKind::Io => {
                    debug_assert!(u32::try_from(val).is_ok(), "write to BAR truncates value");
                    unsafe { self.dev.ecam.write_u32(bar_offset, val as u32) };
                }
                BarKind::Memory64 => unsafe { self.dev.ecam.write_u64(bar_offset, val as u64) },
            }
        }
    }
}

mod ecam {
    use super::DeviceIdent;

    pub struct Ecam {
        mmio_base: *mut u32,
    }

    impl Ecam {
        /// Create a new `Ecam`
        ///
        /// # Safety
        ///
        /// The pointer must be non-null and aligned to at least 4 bytes.
        pub unsafe fn new(mmio_base: *mut u8) -> Ecam {
            Self {
                mmio_base: mmio_base.cast(),
            }
        }

        pub fn read_ident(&self) -> DeviceIdent {
            // SAFETY: Register 0 and 2 are always implemented.
            let (id_reg, class_reg) = unsafe { (self.read_register(0), self.read_register(2)) };

            let vendor_id = id_reg as u16;
            let device_id = (id_reg >> 16) as u16;
            let revision = class_reg as u8;
            let prog_if = (class_reg >> 8) as u8;
            let subclass = (class_reg >> 16) as u8;
            let class = (class_reg >> 24) as u8;

            DeviceIdent {
                vendor_id,
                device_id,
                class,
                subclass,
                prog_if,
                revision,
            }
        }

        /// Read a config space register
        ///
        /// # Panics
        ///
        /// This function will panic if the register index exceeds the bounds of the
        /// configuration space.
        pub unsafe fn read_register(&self, reg: usize) -> u32 {
            assert!(reg < 1024, "register access out of bounds");
            u32::from_le(self.mmio_base.add(reg).read_volatile())
        }

        /// Write a config space register
        ///
        /// # Panics
        ///
        /// This function will panic if the register index exceeds the bounds of the
        /// configuration space.
        pub unsafe fn write_register(&self, reg: usize, val: u32) {
            assert!(reg < 1024, "register access out of bounds");
            self.mmio_base.add(reg).write_volatile(val.to_le());
        }

        /// Read a 32-bit value from the configuration space
        ///
        /// # Safety
        ///
        /// `offset` must not exceed the bounds of the configuration space.
        pub unsafe fn read_u8(&self, offset: usize) -> u8 {
            (self.read_register(offset / 4) >> ((offset % 4) * 8)) as u8
        }

        /// Read a 32-bit value from the configuration space
        ///
        /// # Safety
        ///
        /// `offset` must be aligned to a multiple of 2 bytes and must not exceed the bounds
        /// of the configuration space.
        pub unsafe fn read_u16(&self, offset: usize) -> u16 {
            (self.read_register(offset / 4) >> ((offset % 4) * 8)) as u16
        }

        /// Read a 32-bit value from the configuration space
        ///
        /// # Safety
        ///
        /// `offset` must be aligned to a multiple of 4 bytes and must not exceed the bounds
        /// of the configuration space.
        pub unsafe fn read_u32(&self, offset: usize) -> u32 {
            self.read_register(offset / 4)
        }

        /// Read a 64-bit value from the configuration space
        ///
        /// # Safety
        ///
        /// `offset` must be aligned to a multiple of 4 bytes and must not exceed the bounds
        /// of the configuration space.
        pub unsafe fn read_u64(&self, offset: usize) -> u64 {
            let low = self.read_register(offset / 4) as u64;
            let high = self.read_register(offset / 4 + 1) as u64;

            (high << 32) | low
        }

        pub unsafe fn write_u8(&self, offset: usize, val: u8) {
            let reg = offset / 4;
            let shift = (offset % 4) * 8;
            let mask = !((!0u8 as u32) << shift);
            let val = (val as u32) << shift;
            self.write_register(reg, (self.read_register(reg) & mask) | val);
        }

        pub unsafe fn write_u16(&self, offset: usize, val: u16) {
            let reg = offset / 4;
            let shift = (offset % 4) * 8;
            let mask = !((!0u16 as u32) << shift);
            let val = (val as u32) << shift;
            self.write_register(reg, (self.read_register(reg) & mask) | val);
        }

        pub unsafe fn write_u32(&self, offset: usize, val: u32) {
            self.write_register(offset / 4, val);
        }

        pub unsafe fn write_u64(&self, offset: usize, val: u64) {
            self.write_register(offset / 4, val as u32);
            self.write_register(offset / 4 + 1, (val >> 32) as u32);
        }
    }
}

mod device {
    use super::{
        bar::{Bar, BarKind},
        ecam::Ecam,
        BusAddr,
    };
    use core::fmt;

    #[allow(clippy::module_name_repetitions)]
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub enum DeviceKind {
        Regular,
        PciBridge,
        CardbusBridge,
    }

    impl DeviceKind {
        fn num_bars(self) -> usize {
            match self {
                DeviceKind::Regular => 6,
                DeviceKind::PciBridge => 2,
                DeviceKind::CardbusBridge => todo!(),
            }
        }
    }

    #[allow(clippy::module_name_repetitions)]
    #[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
    pub struct DeviceIdent {
        pub vendor_id: u16,
        pub device_id: u16,
        pub class: u8,
        pub subclass: u8,
        pub prog_if: u8,
        pub revision: u8,
    }

    impl fmt::Debug for DeviceIdent {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "[{:04x}:{:04x}]({:02x}:{:02x}:{:02x})",
                self.vendor_id, self.device_id, self.class, self.subclass, self.prog_if
            )
        }
    }

    pub struct Device {
        pub addr: BusAddr,
        pub ident: DeviceIdent,
        pub(super) ecam: Ecam,
        pub kind: DeviceKind,
        pub multifunc: bool,
    }

    impl Device {
        pub fn bars(&self) -> impl Iterator<Item = Bar<'_>> {
            let mut bar_index = 0;

            core::iter::from_fn(move || {
                if bar_index >= self.kind.num_bars() {
                    return None;
                }

                // SAFETY: The check above ensures we do not create
                // a BAR with an invalid index for this device.
                let bar = unsafe { Bar::new(self, bar_index) };

                bar_index += if bar.kind() == BarKind::Memory64 {
                    2
                } else {
                    1
                };

                Some(bar)
            })
            .filter(|bar| bar.kind() != BarKind::None)
        }

        fn read_command_register(&self) -> CommandRegister {
            unsafe { CommandRegister::from_bits_unchecked(self.ecam.read_u16(0x4)) }
        }

        unsafe fn write_command_register(&self, cmd: CommandRegister) {
            self.ecam.write_u16(0x4, cmd.bits());
        }

        pub fn update_command_register<F>(&self, f: F)
        where
            F: FnOnce(&mut CommandRegister),
        {
            let mut cmd = self.read_command_register();

            f(&mut cmd);

            unsafe { self.write_command_register(cmd) };
        }
    }

    bitflags::bitflags! {
        #[repr(transparent)]
        pub struct CommandRegister : u16 {
            const IO_SPACE                              = 1 << 0;
            const MEMORY_SPACE                          = 1 << 1;
            const BUS_MASTER                            = 1 << 2;
            const SPECIAL_CYCLES                        = 1 << 3;
            const MEMORY_WRITE_AND_INVALIDATE_ENABLE    = 1 << 4;
            const VGA_PALETTE_SNOOP                     = 1 << 5;
            const PARITY_ERROR_RESPONSE                 = 1 << 6;
            const SERR_ENABLE                           = 1 << 8;
            const FAST_BACK_TO_BACK_ENABLE              = 1 << 9;
            const INTERRUPT_DISABLE                     = 1 << 10;
        }
    }
}

fn device_kind(id: DeviceIdent) -> &'static str {
    let mut s = "<unknown>";

    #[allow(clippy::single_match)]
    match id.class {
        0x1 => match id.subclass {
            0x0 => s = "SCSI Bus Controller",
            0x6 => match id.prog_if {
                0x1 => s = "AHCI 1.0",
                _ => {}
            },
            0x8 => match id.prog_if {
                0x2 => s = "NVMe Controller",
                _ => {}
            },
            _ => {}
        },
        0x6 => match id.subclass {
            0x0 => s = "Host Bridge Controller",
            _ => {}
        },
        _ => {}
    }

    s
}

pub struct HostBridge {
    bus_range: RangeInclusive<u8>,
    ecam_base: *mut u8,
}

// SAFETY: We just wanna store a raw pointer man!
unsafe impl Send for HostBridge {}
// SAFETY: The bootloader executes in a single-threaded context.
unsafe impl Sync for HostBridge {}

impl HostBridge {
    fn get_ecam(&self, bus: u8, dev: u8, func: u8) -> Option<Ecam> {
        if !self.bus_range.contains(&bus) {
            return None;
        }

        let ecam_offset = (((bus - self.bus_range.start()) as usize) << 20)
            | ((dev as usize) << 15)
            | ((func as usize) << 12);

        Some(unsafe { Ecam::new(self.ecam_base.add(ecam_offset)) })
    }

    fn get_device(&self, bus: u8, dev: u8, func: u8) -> Option<Device> {
        let ecam = self.get_ecam(bus, dev, func)?;

        let ident = ecam.read_ident();

        if ident.vendor_id == 0xffff || ident.device_id == 0xffff {
            return None;
        }

        // SAFETY: This register is always implemented.
        let header_type = unsafe { ecam.read_u8(0xe) };
        let multifunc = header_type & 0x80 != 0;
        let header_type = header_type & 0x7f;

        let kind = match header_type {
            0x0 => DeviceKind::Regular,
            0x1 => DeviceKind::PciBridge,
            0x2 => DeviceKind::CardbusBridge,
            unknown => {
                log::info!("skipping unknown device type: {unknown:#02x}");
                return None;
            }
        };

        Some(Device {
            addr: BusAddr { bus, dev, func },
            ident,
            ecam,
            kind,
            multifunc,
        })
    }
}

#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct BusAddr {
    pub bus: u8,
    pub dev: u8,
    pub func: u8,
}

impl fmt::Debug for BusAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:02x}:{:02x}:{:02x}]", self.bus, self.dev, self.func)
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

fn alloc_resources(_host: &HostBridge, device: &mut Device) {
    let mut enable_mmio = false;
    let mut enable_ioio = false;

    for bar in device.bars() {
        match bar.kind() {
            // Skip unimplemented BARs.
            BarKind::None => continue,
            BarKind::Io => enable_ioio = true,
            BarKind::Memory32 | BarKind::Memory64 => enable_mmio = true,
        };

        let Some(mmio_base) = pmm::alloc_frames_aligned(
            pages_for!(bar.layout()),
            bar.layout(),
            // `Memory32` and `Io` BARs need to be allocated below the 4 GiB limit.
            // `Io` can technically be allocated anywhere (even outside of RAM), a potential
            // optimization for later.
            bar.kind() != BarKind::Memory64,
        ) else {
            log::warn!("failed to allocate BAR for device {:?}", device.addr);
            return;
        };

        log::debug!("    allocated BAR{} at {mmio_base:#018x}", bar.bar_index());

        // SAFETY: If `alloc_frames_aligned()` was successful, `mmio_base` is a well aligned
        // address and is within range for the BAR.
        unsafe { bar.write(mmio_base) };
    }

    device.update_command_register(|cmd| {
        // Enable access to Memory and IO space if such BARs were allocated.
        cmd.set(CommandRegister::MEMORY_SPACE, enable_mmio);
        cmd.set(CommandRegister::IO_SPACE, enable_ioio);

        // We pollin'.
        cmd.insert(CommandRegister::INTERRUPT_DISABLE);
    });
}

fn probe(host: &HostBridge, bus: u8, dev: u8, func: u8) -> bool {
    let Some(mut device) = host.get_device(bus, dev, func) else { return false };

    if device.kind != DeviceKind::Regular {
        // TODO: Handle PCI-to-PCI bridges. (Is CardBus worth it? I think no.)
        log::info!("todo: skipping PCI bridge device");
        return false;
    }

    log::debug!(
        "  {:?}: {:?}: {}",
        device.addr,
        device.ident,
        device_kind(device.ident),
    );

    alloc_resources(host, &mut device);

    // check for a driver
    if let Some(driver) = match_pci_driver(&device) {
        let init = driver.pci_init.unwrap();
        init(&device);
    }

    device.multifunc
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct DriverCompat {
    pub class: u8,
    pub subclass: Option<u8>,
}

impl DriverCompat {
    pub fn matches(self, dev: &Device) -> bool {
        self.class == dev.ident.class
            && if let Some(subclass) = self.subclass {
                subclass == dev.ident.subclass
            } else {
                false
            }
    }
}

fn match_pci_driver(dev: &Device) -> Option<&'static DeviceDriver> {
    device_drivers()
        .iter()
        .find(|driver| match driver.pci_compat {
            Some(driver_compat) => driver_compat.iter().any(|compat| compat.matches(dev)),
            None => false,
        })
}

fn get_bus_range(node: &fdt::node::FdtNode) -> RangeInclusive<u8> {
    let Some(property) = node.property("bus-range") else { return 0..=255 };
    let bus_range = property.value;

    let start_bus = u8::try_from(u32::from_be_bytes(bus_range[..4].try_into().unwrap()))
        .expect("bus-range value overflows");
    let end_bus = u8::try_from(u32::from_be_bytes(bus_range[4..].try_into().unwrap()))
        .expect("bus-range value overflows");

    start_bus..=end_bus
}

fn init(_fdt: &fdt::Fdt, node: &fdt::node::FdtNode) {
    let Some(mmio_window) = node.reg().unwrap().next() else { return };

    let host_bridge = HostBridge {
        bus_range: get_bus_range(node),
        ecam_base: mmio_window.starting_address.cast_mut(),
    };

    log::debug!(
        "host bridge @ {:p}, bus range: {:?}",
        host_bridge.ecam_base,
        host_bridge.bus_range
    );

    for (bus, dev) in busdev_iter() {
        if probe(&host_bridge, bus, dev, 0) {
            for func in 1..=7 {
                probe(&host_bridge, bus, dev, func);
            }
        }
    }
}

#[used]
#[link_section = ".device_drivers"]
static PCIE_DRIVER: super::DeviceDriver = super::DeviceDriver {
    name: "pcie",
    fdt_compat: Some(&["pci-host-ecam-generic", "pci-host-cam-generic"]),
    fdt_init: Some(init),
    pci_compat: None,
    pci_init: None,
};
