/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

mod ns16550;

use crate::console::{self, ConsoleBackend};
use anyhow::anyhow;
use core::time::Duration;

#[derive(Debug)]
pub enum Error {
    InvalidSpeed,
    Overrun,
    Framing,
    Parity,
    TimedOut,
}

impl From<Error> for anyhow::Error {
    fn from(error: Error) -> anyhow::Error {
        anyhow!("{error:?}")
    }
}

impl From<Error> for console::Error {
    fn from(error: Error) -> console::Error {
        match error {
            Error::TimedOut => console::Error::TimedOut,
            error => console::Error::Uart(error),
        }
    }
}

pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(Clone, Copy)]
pub struct BusAccess {
    base: *mut u8,
    width: u8,
    shift: u8,
}

unsafe impl Send for BusAccess {}
unsafe impl Sync for BusAccess {}

impl BusAccess {
    pub const fn new(base: *mut u8, width: u8, shift: u8) -> BusAccess {
        Self { base, width, shift }
    }

    pub unsafe fn read(self, offset: usize) -> u32 {
        let ptr = self.base.add(offset << self.shift);
        match self.width {
            1 => ptr.read_volatile() as u32,
            2 => ptr.cast::<u16>().read_volatile() as u32,
            4 => ptr.cast::<u32>().read_volatile(),
            w => unimplemented!("invalid access width: {w} bytes"),
        }
    }

    pub unsafe fn write(self, offset: usize, value: u32) {
        let ptr = self.base.add(offset << self.shift);
        match self.width {
            1 => ptr.write_volatile(value as u8),
            2 => ptr.cast::<u16>().write_volatile(value as u16),
            4 => ptr.cast::<u32>().write_volatile(value),
            w => unimplemented!("invalid access width: {w} bytes"),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Baud(pub u32);

impl Baud {
    pub const B115200: Baud = Baud(115200);
}

pub trait UartDevice: Send + Sync {
    fn receive(&self) -> Result<u8>;
    fn receive_timeout(&self, duration: Duration) -> Result<u8>;
    fn transmit(&self, byte: u8) -> Result<()>;
}

impl<T: UartDevice> ConsoleBackend for T {
    fn receive(&self, duration: Option<Duration>) -> console::Result<u8> {
        match duration {
            Some(duration) => Ok(self.receive_timeout(duration)?),
            None => Ok(self.receive()?),
        }
    }

    fn transmit(&self, byte: u8) -> console::Result<()> {
        Ok(self.transmit(byte)?)
    }
}
