/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

#![cfg(sbi)]

use crate::{console, dev, hcf, io, pmm, sys::fdt, time, BOOT_HART_ID};
use core::sync::atomic::Ordering;

global_asm!(include_str!("locore.s"), options(raw));

#[no_mangle]
extern "C" fn spark_main(hartid: usize, dtb_ptr: *mut u8) -> ! {
    // Initialize the logger
    //  TODO: Use legacy SBI console until we probe for consoles.
    io::init();

    BOOT_HART_ID.store(hartid, Ordering::Relaxed);

    // Install the Device Tree
    let fdt = unsafe { fdt::init(dtb_ptr) };

    let Some(timebase_freq) = fdt.property_as::<u32>("/cpus/timebase-frequency") else {
        log::error!("device tree missing `/cpus/timebase-frequency` property");
        hcf();
    };
    time::init(timebase_freq as u64);

    // Bootstrap memory allocation
    pmm::init();

    console::init();

    // Probe the full device tree before we search for a boot partition
    dev::init();

    crate::main();
}
