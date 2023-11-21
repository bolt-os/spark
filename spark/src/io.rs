// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        println!("{}: {}", record.target(), record.args());
    }

    fn flush(&self) {}
}

static LOGGER: Logger = Logger;

pub fn init() {
    log::set_logger(&LOGGER).unwrap();
    log::set_max_level(log::LevelFilter::Trace);
}

pub type Result<T> = core::result::Result<T, Error>;

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
    DeviceError,
    InvalidArgument,
    IsADirectory,
    NameTooLong,
    NotADirectory,
    NotFound,
    Other,
    OutOfBounds,
    TimedOut,
    Unsupported,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(self, f)
    }
}

impl From<Error> for anyhow::Error {
    fn from(value: Error) -> Self {
        anyhow::anyhow!("{value}")
    }
}

#[cfg(uefi)]
impl From<uefi::Status> for Error {
    fn from(value: uefi::Status) -> Self {
        use uefi::Status;
        match value {
            Status::DEVICE_ERROR => Self::DeviceError,
            Status::TIMEOUT => Self::TimedOut,
            _ => {
                log::error!("uefi->io: {value:?}");
                Self::Other
            }
        }
    }
}
