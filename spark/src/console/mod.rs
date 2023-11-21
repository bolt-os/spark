// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

mod sbi;

use crate::{dev::uart, sys::fdt, util::maybe_static_arc::MaybeStaticArc};
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
    pub init: fn(&fdt::Node) -> anyhow::Result<Arc<dyn ConsoleBackend>>,
}

linkset::declare!(console_drivers: Driver);

pub macro console_driver($driver:expr) {
    linkset::entry!(console_drivers, Driver, $driver);
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

#[cfg(sbi)]
pub fn init() {
    let fdt = fdt::get_fdt();

    let driver = 'probe: {
        if let Some(node) = fdt
            .find_node("/chosen")
            .and_then(|node| node.property_as::<&str>("stdout-path"))
            .map(|path| &path[..path.find(':').unwrap_or(path.len())])
            .and_then(|path| fdt.find_node(path))
        {
            for driver in console_drivers.as_slice() {
                if !node.is_compatible_any(driver.compatible) {
                    continue;
                }
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

        let Some(nodes) = fdt.find_node("/soc").map(|node| node.children()) else {
            // No console found.
            println!("no /soc");
            return;
        };

        for node in nodes {
            if !node.is_enabled() {
                continue;
            }

            for driver in console_drivers.as_slice() {
                if !node.is_compatible_any(driver.compatible) {
                    continue;
                }
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
