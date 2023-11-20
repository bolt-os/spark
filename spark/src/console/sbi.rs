/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use super::{Console, ConsoleBackend, ConsoleInner, Error};
use crate::util::maybe_static_arc::MaybeStaticArc;
use spin::Mutex;

#[derive(Clone, Copy)]
pub enum SbiConsole {
    Legacy,
}

impl ConsoleBackend for SbiConsole {
    fn receive(&self, _duration: Option<core::time::Duration>) -> super::Result<u8> {
        Err(Error::NoDevice)
    }

    fn transmit(&self, byte: u8) -> super::Result<()> {
        match *self {
            Self::Legacy => {
                sbi::legacy::console_putchar(byte);
                Ok(())
            }
        }
    }
}

pub static SBI_CONSOLE: Console = Console {
    inner: Mutex::new(ConsoleInner {
        driver: MaybeStaticArc::Static(&SbiConsole::Legacy),
    }),
};
