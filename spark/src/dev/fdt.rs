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

use core::{cell::SyncUnsafeCell, sync::atomic::Ordering};

use ::fdt as libfdt;

use crate::BOOT_HART_ID;

static FDT: SyncUnsafeCell<Option<libfdt::Fdt>> = SyncUnsafeCell::new(None);

pub fn init(dtb_ptr: *mut u8) -> &'static libfdt::Fdt<'static> {
    // "Install" the FDT.
    // The reference returned here has a 'static lifetime.
    let fdt = unsafe {
        let fdt = libfdt::Fdt::from_ptr(dtb_ptr).unwrap();
        FDT.get().write_volatile(Some(fdt));
        (*FDT.get()).as_ref().unwrap()
    };

    let bsp_node = fdt
        .cpus()
        .find(|node| node.ids().first() == BOOT_HART_ID.load(Ordering::Relaxed))
        .expect("no matching /cpus node for the boot hart");

    let timebase_freq = bsp_node.timebase_frequency();
    crate::time::init(timebase_freq as u64);

    fdt
}

pub fn get_fdt() -> &'static libfdt::Fdt<'static> {
    unsafe { (*FDT.get()).as_ref().unwrap() }
}
