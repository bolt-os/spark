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

use crate::{
    config::{Entry, Value},
    dev::fdt::DTB_PTR,
    fs::File,
    rtld::Rtld,
    vmm, BOOT_HART_ID,
};
use core::sync::atomic::Ordering;
use elf::Elf;

pub fn main(mut fs: Box<dyn File>, config: &Entry) -> anyhow::Result<!> {
    let Some(Value::String(kernel_path)) = config.param("kernel-path") else {
        panic!();
    };

    let kernel_path = kernel_path.strip_prefix("boot://").unwrap();
    let mut kernel_file = fs.open(kernel_path)?;
    let kernel_data = kernel_file.read_to_end()?;
    let kernel_elf = Elf::new(&kernel_data).unwrap();
    let mut rtld = Rtld::new(&kernel_elf).unwrap();
    let mut vmspace = vmm::init_from_fdt(false);

    rtld.load_image();
    rtld.map_image(&mut vmspace).unwrap();
    rtld.do_relocations();

    let entry_point = rtld.reloc(rtld.elf.entry_point() as _);
    let global_pointer = rtld
        .elf
        .symbol_table()
        .and_then(|symtab| {
            symtab
                .find(|s| s.name() == Some("__global_pointer$"))
                .map(|sym| sym.value())
        })
        .unwrap_or(0);
    let boot_hartid = BOOT_HART_ID.load(Ordering::Relaxed);
    let dtb_ptr = DTB_PTR.load(Ordering::Relaxed);

    unsafe {
        vmspace.switch_to();
        spinup(
            boot_hartid,
            dtb_ptr,
            rtld.image_base,
            global_pointer as _,
            entry_point,
        );
    }
}

#[naked]
unsafe extern "C" fn spinup(
    hart_id: usize,
    dtb_ptr: *mut u8,
    phys_base: usize,
    global_pointer: usize,
    entry_point: usize,
) -> ! {
    asm!(
        "
            mv      gp, a3
            csrci   sstatus, 0x2
            csrw    sie, zero
            csrw    stvec, zero
            csrw    sscratch, zero
            jr      a4
        ",
        options(noreturn)
    );
}
