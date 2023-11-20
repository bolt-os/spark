/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

mod sbi;

use crate::{
    dev::{self, fdt, uart},
    util::{linkset, maybe_static_arc::MaybeStaticArc},
};
use ::fdt::node::{FdtNode, NodeProperty};
use alloc::sync::Arc;
use core::{fmt, time::Duration};
use spin::Mutex;

#[derive(Debug)]
pub enum Error {
    TimedOut,
    NoDevice,
    Uart(uart::Error),
}

pub type Result<T, E = Error> = core::result::Result<T, E>;

pub trait ConsoleBackend: Send + Sync {
    fn transmit(&self, byte: u8) -> Result<()>;
    fn receive(&self, duration: Option<Duration>) -> Result<u8>;
}

#[repr(C)]
pub struct Driver {
    pub name: &'static str,
    pub compatible: &'static [&'static str],
    pub init: fn(&FdtNode) -> anyhow::Result<Arc<dyn ConsoleBackend>>,
}

linkset::declare!(console_drivers: Driver);

pub macro console_driver($driver:expr) {
     linkset::entry!(console_drivers: Driver, $driver);
}

pub struct Console {
    inner: Mutex<ConsoleInner>,
}

struct ConsoleInner {
    driver: MaybeStaticArc<dyn ConsoleBackend>,
}

impl ConsoleInner {
    fn write_byte(&mut self, byte: u8) -> Result<()> {
        self.driver.transmit(byte)
    }
}

impl fmt::Write for ConsoleInner {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for byte in s.bytes() {
            self.write_byte(byte).map_err(|_| fmt::Error)?;
        }
        Ok(())
    }
}

static CONSOLE: Mutex<MaybeStaticArc<Console>> =
    Mutex::new(MaybeStaticArc::Static(&sbi::SBI_CONSOLE));

pub fn console() -> MaybeStaticArc<Console> {
    CONSOLE.lock().clone()
}

pub fn print(args: fmt::Arguments) {
    let _ = fmt::write(&mut *console().inner.lock(), args);
}

pub macro print($fmt:literal $(, $arg:expr)* $(,)?) {
     print(format_args!($fmt, $($arg),*))
 }

pub macro println {
     () => { print!("\n") },
     ($fmt:literal $(, $arg:expr)* $(,)?) => {
         print!("{}\n", format_args!($fmt, $($arg),*))
     },
 }

pub fn init() {
    let fdt = fdt::get_fdt().unwrap();

    let driver = 'probe: {
        if let Some(node) = fdt
            .find_node("/chosen")
            .and_then(|node| node.property("stdout-path"))
            .and_then(NodeProperty::as_str)
            .map(|path| &path[..path.as_bytes().partition_point(|b| *b != b':')])
            .and_then(|path| fdt.find_node(path))
        {
            for driver in console_drivers
                .as_slice()
                .iter()
                .filter(|driver| dev::match_fdt_node(&node, driver.compatible))
            {
                let backend = match (driver.init)(&node) {
                    Ok(backend) => backend,
                    Err(error) => {
                        println!("probe failed: {error}");
                        continue;
                    }
                };
                break 'probe backend;
            }
        }

        for node in fdt.all_nodes() {
            let enabled = node
                .property("status")
                .and_then(NodeProperty::as_str)
                .map_or(true, |status| matches!(status, "ok" | "okay"));
            if !enabled {
                continue;
            }

            for driver in console_drivers
                .as_slice()
                .iter()
                .filter(|driver| dev::match_fdt_node(&node, driver.compatible))
            {
                let backend = match (driver.init)(&node) {
                    Ok(backend) => backend,
                    Err(error) => {
                        println!("probe failed: {error}");
                        continue;
                    }
                };
                break 'probe backend;
            }
        }

        // No console found.
        return;
    };

    *CONSOLE.lock() = MaybeStaticArc::Arc(Arc::new(Console {
        inner: Mutex::new(ConsoleInner {
            driver: MaybeStaticArc::Arc(driver),
        }),
    }));
}
