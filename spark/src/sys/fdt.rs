/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use core::fmt::Display;

pub use fdt::*;

static mut FDT: Option<Fdt> = None;

pub unsafe fn init(dtb: *const u8) -> &'static Fdt<'static> {
    assert!(FDT.is_none(), "device tree is already initialized");
    let fdt = match Fdt::from_ptr(dtb) {
        Ok(fdt) => fdt,
        Err(error) => panic!("invalid device tree: {error:?}"),
    };
    FDT = Some(fdt);
    FDT.as_ref().unwrap_unchecked()
}

#[cfg(sbi)]
pub fn get_fdt() -> &'static Fdt<'static> {
    let Some(fdt) = (unsafe { FDT.as_ref() }) else {
        panic!("device tree not yet initialized");
    };
    fdt
}

#[allow(clippy::unnecessary_wraps)]
pub fn try_get_fdt() -> Option<&'static Fdt<'static>> {
    match unsafe { FDT.as_ref() } {
        Some(fdt) => Some(fdt),
        #[cfg(sbi)]
        None => panic!("no device tree"),
        #[cfg(not(sbi))]
        None => None,
    }
}

pub trait NodeExt {
    fn error<P: Display>(&self, error: P) -> anyhow::Error;
}

impl<'f, 'dtb: 'f> NodeExt for Node<'f, 'dtb> {
    fn error<P: Display>(&self, error: P) -> anyhow::Error {
        anyhow::anyhow!("{}: {error}", self.name)
    }
}
