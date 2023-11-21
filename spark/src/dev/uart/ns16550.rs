/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use super::{Baud, BusAccess, Result};
use crate::{
    console::{self, console_driver, ConsoleBackend},
    time::Timeout,
};
use alloc::sync::Arc;
use anyhow::{anyhow, Context};
use core::time::Duration;

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Clone, Copy, Debug)]
    pub struct InterruptEnable : u8 {
        const RX_DATA_AVAILABLE = 1 << 0;
        const TX_HOLDING_REGISTER_EMPTY = 1 << 1;
        const RX_LINE_STATUS = 1 << 2;
        const MODEM_STATUS = 1 << 3;
    }

    #[repr(transparent)]
    #[derive(Clone, Copy, Debug)]
    pub struct FifoControl : u8 {
        const ENABLE = 1 << 0;
        const RX_RESET = 1 << 1;
        const TX_RESET = 1 << 2;
        const DMA_MODE_SELECT = 1 << 3;
        const RX_TRIGGER_LO = 1 << 6;
        const RX_TRIGGER_HI = 1 << 7;
    }

    #[repr(transparent)]
    #[derive(Clone, Copy, Debug)]
    pub struct LineControl : u8 {
        const WORD_LENGTH_LO = 1 << 0;
        const WORD_LENGTH_HI = 1 << 1;
        const STOP_BITS = 1 << 2;
        const PARITY_ENABLE = 1 << 3;
        const EVEN_PARITY = 1 << 4;
        const STICK_PARITY = 1 << 5;
        const SET_BREAK = 1 << 6;
        const DIVISOR_LATCH_ACCESS = 1 << 7;
    }

    #[repr(transparent)]
    #[derive(Clone, Copy, Debug)]
    pub struct LineStatus : u8 {
        const DATA_READY = 1 << 0;
        const OVERRUN_ERROR = 1 << 1;
        const PARITY_ERROR = 1 << 2;
        const FRAMING_ERROR = 1 << 3;
        const BREAK = 1 << 4;
        const TX_HOLDING_REGISTER_EMPTY = 1 << 5;
        const TX_EMPTY = 1 << 6;

    }
}

mod reg {
    pub const DATA: usize = 0;
    pub const INTERRUPT_ENABLE: usize = 1;
    pub const FIFO_CONTROL: usize = 2;
    pub const LINE_CONTROL: usize = 3;
    pub const LINE_STATUS: usize = 5;

    pub const DIVISOR_LO: usize = 0;
    pub const DIVISOR_HI: usize = 1;
}

pub struct Uart {
    ba: BusAccess,
    baud_freq: u32,
    current_speed: Baud,
}

impl Uart {
    pub fn new(ba: BusAccess, baud_freq: u32) -> Uart {
        let mut uart = Self {
            ba,
            baud_freq,
            current_speed: Baud(0),
        };
        uart.current_speed = divisor_to_baud(baud_freq, uart.read_divisor());
        uart
    }

    fn read_register(&self, reg: usize) -> u8 {
        unsafe { self.ba.read(reg) as u8 }
    }

    fn write_register(&self, reg: usize, val: u8) {
        unsafe { self.ba.write(reg, val as u32) };
    }

    pub fn line_control(&self) -> LineControl {
        LineControl::from_bits_retain(self.read_register(reg::LINE_CONTROL))
    }

    pub fn set_line_control(&self, val: LineControl) {
        self.write_register(reg::LINE_CONTROL, val.bits());
    }

    pub fn line_status(&self) -> LineStatus {
        LineStatus::from_bits_retain(self.read_register(reg::LINE_STATUS))
    }

    pub fn set_fifo_control(&self, val: FifoControl) {
        self.write_register(reg::FIFO_CONTROL, val.bits());
    }

    fn read_divisor(&self) -> u16 {
        let lcr = self.line_control();
        self.set_line_control(lcr | LineControl::DIVISOR_LATCH_ACCESS);
        let lo = self.read_register(reg::DIVISOR_LO);
        let hi = self.read_register(reg::DIVISOR_HI);
        self.set_line_control(lcr);
        u16::from_le_bytes([lo, hi])
    }

    fn write_divisor(&self, div: u16) {
        let [lo, hi] = div.to_le_bytes();
        let lcr = self.line_control();
        self.set_line_control(lcr | LineControl::DIVISOR_LATCH_ACCESS);
        self.write_register(reg::DIVISOR_LO, lo);
        self.write_register(reg::DIVISOR_HI, hi);
        self.set_line_control(lcr);
    }

    pub fn initialize(&mut self, baud: Baud) -> Result<()> {
        self.write_register(reg::INTERRUPT_ENABLE, 0);
        self.write_divisor(baud_to_divisor(self.baud_freq, baud)?);
        self.set_line_control(LineControl::WORD_LENGTH_HI | LineControl::WORD_LENGTH_LO);
        self.set_fifo_control(
            FifoControl::ENABLE
                | FifoControl::RX_RESET
                | FifoControl::TX_RESET
                | FifoControl::RX_TRIGGER_HI
                | FifoControl::RX_TRIGGER_LO,
        );
        Ok(())
    }

    pub fn check_errors(&self) -> Result<()> {
        let lsr = self.line_status();
        if lsr.contains(LineStatus::FRAMING_ERROR) {
            Err(super::Error::Framing)
        } else if lsr.contains(LineStatus::OVERRUN_ERROR) {
            Err(super::Error::Overrun)
        } else if lsr.contains(LineStatus::PARITY_ERROR) {
            Err(super::Error::Parity)
        } else {
            Ok(())
        }
    }

    pub fn receive_timeout(&self, duration: Duration) -> Result<u8> {
        let timeout = Timeout::start(duration);
        while !self.line_status().contains(LineStatus::DATA_READY) {
            self.check_errors()?;
            if timeout.expired() {
                return Err(super::Error::TimedOut);
            }
        }
        Ok(self.read_register(reg::DATA))
    }

    pub fn receive(&self) -> Result<u8> {
        self.receive_timeout(Duration::MAX)
    }

    pub fn transmit(&self, byte: u8) -> Result<()> {
        while !self
            .line_status()
            .contains(LineStatus::TX_HOLDING_REGISTER_EMPTY)
        {
            self.check_errors()?;
        }
        self.write_register(reg::DATA, byte);
        Ok(())
    }
}

fn baud_to_divisor(freq: u32, baud: Baud) -> Result<u16> {
    match u16::try_from(freq / (baud.0 * 16)) {
        Ok(divisor) => Ok(divisor),
        Err(_) => Err(super::Error::InvalidSpeed),
    }
}

fn divisor_to_baud(freq: u32, div: u16) -> Baud {
    Baud(freq / (div as u32 * 16))
}

impl super::UartDevice for Uart {
    fn receive(&self) -> Result<u8> {
        self.receive()
    }

    fn receive_timeout(&self, duration: Duration) -> Result<u8> {
        self.receive_timeout(duration)
    }

    fn transmit(&self, byte: u8) -> Result<()> {
        self.transmit(byte)
    }
}

console_driver!(console::Driver {
    name: "ns16550",
    compatible: &["ns16550", "ns16550a", "snps,dw-apb-uart"],
    init: init_fdt,
});

fn init_fdt(node: &fdt::Node) -> anyhow::Result<Arc<dyn ConsoleBackend>> {
    println!("{}", node.path());
    let reg = node.reg_by_index(0)?;
    let width = node.try_property_as::<u32>("reg-io-width")?.unwrap_or(1);
    let shift = node
        .try_property_as::<u32>("reg-shift")?
        .unwrap_or_default();
    let baud_freq = node
        .try_property_as::<u32>("clock-frequency")?
        .with_context(|| anyhow!("missing `clock-frequency` property"))?;

    let ba = BusAccess::new(reg.addr as *mut u8, width as u8, shift as u8);
    let mut uart = Uart::new(ba, baud_freq);

    uart.initialize(Baud::B115200)?;

    Ok(Arc::new(uart))
}
