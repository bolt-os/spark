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

#![doc = include_str!("../../README.md")]
#![no_std]
#![no_main]
#![feature(
    custom_test_frameworks,
    default_alloc_error_handler,
    prelude_import,
    asm_sym,                            // https://github.com/rust-lang/rust/issues/93333
    let_else,                           // https://github.com/rust-lang/rust/issues/87335
    naked_functions,                    // https://github.com/rust-lang/rust/issues/32408
    new_uninit,                         // https://github.com/rust-lang/rust/issues/63291
    once_cell,                          // https://github.com/rust-lang/rust/issues/74465
    pointer_is_aligned,                 // https://github.com/rust-lang/rust/issues/96284
    strict_provenance,                  // https://github.com/rust-lang/rust/issues/95228
)]
#![reexport_test_harness_main = "test_main"]
#![test_runner(test::runner)]
#![warn(clippy::cargo, clippy::pedantic, clippy::undocumented_unsafe_blocks)]
#![deny(
    clippy::semicolon_if_nothing_returned,
    clippy::debug_assert_with_mut_call
)]
#![allow(
    clippy::cast_lossless,
    clippy::enum_glob_use,
    clippy::inline_always,
    clippy::items_after_statements,
    clippy::must_use_candidate,
    clippy::unreadable_literal,
    clippy::wildcard_imports
)]

extern crate alloc;

#[prelude_import]
#[allow(unused_imports)]
use crate::prelude::*;
mod prelude {
    pub use crate::{print, println};
    pub use alloc::{
        borrow::ToOwned,
        boxed::Box,
        format,
        string::{String, ToString},
        vec,
        vec::Vec,
    };
    pub use core::{
        arch::{asm, global_asm},
        prelude::rust_2021::*,
    };
}

mod dev;
mod io;
mod malloc;
mod mem;
mod panic;
mod proto;
mod rtld;
mod test;

pub use mem::{pmm, vmm};

use core::{
    ptr,
    sync::atomic::{AtomicPtr, Ordering},
};
use spark::Bootinfo;

global_asm!(include_str!("locore.s"), options(raw));

pub fn hcf() -> ! {
    println!("bruh.");
    loop {
        core::hint::spin_loop();
    }
}

/// # Safety
#[no_mangle]
pub unsafe extern "C" fn trap_handler() {
    println!("trap!");
    hcf();
}

/// # Safety
///
/// lmao.
#[naked]
#[no_mangle]
#[link_section = ".text._start"]
pub unsafe extern "C" fn _start(hart_id: usize, dtb_ptr: *const u8) {
    asm! {
        r#"
            // Load the global pointer
        .option push
        .option norelax
            la          gp, __global_pointer$
        .option pop

            // Load the stack pointer
            la          sp, __boot_stackp

            // Set the trap entry point
            la          t0, trap_entry
            csrw        stvec, t0

            // Zero out the .bss segment
            la          t0, __bss_start
            la          t1, __bss_end
        4:  sb          zero, (t0)
            addi        t0, t0, 1
            bltu        t0, t1, 4b

            // Clear frame pointer
            mv          fp, zero

            // To Rust!
            call        {spark_main}

            j           .
        "#,
        spark_main = sym spark_main,
        options(noreturn),
    }
}

static DTB_PTR: AtomicPtr<u8> = AtomicPtr::new(ptr::null_mut());

#[allow(clippy::not_unsafe_ptr_arg_deref, clippy::missing_panics_doc)]
pub extern "C" fn spark_main(hartid: usize, dtb_ptr: *mut u8) -> ! {
    DTB_PTR.store(dtb_ptr, Ordering::Relaxed);

    io::init();

    let fdt = unsafe { fdt::Fdt::from_ptr(dtb_ptr) }.unwrap();

    pmm::init_from_fdt(&fdt, dtb_ptr);
    // print_fdt(&fdt);

    let mut vmspace = vmm::init_from_fdt(&fdt, hartid);

    dev::init(&fdt);

    let fw_cfg = {
        let fdt_node = fdt.find_compatible(&["qemu,fw-cfg-mmio"]).unwrap();
        let mmio_window = fdt_node.reg().unwrap().next().unwrap();
        dev::fw_cfg::FwCfg::new(mmio_window.starting_address as _).unwrap()
    };

    if let Some(spark_file) = fw_cfg.lookup("opt/org.spark/self") {
        let file_data = fw_cfg.read_file(spark_file).unwrap();
        unsafe { panic::register_executable(file_data) };
    }

    let kernel_file = fw_cfg
        .lookup("opt/org.spark/kernel")
        .expect("no kernel found");
    let mut kernel_data = fw_cfg.read_file(kernel_file).unwrap();
    let kernel_elf = elf::Elf::new(&kernel_data).unwrap();

    log::info!("loading kernel: `opt/org.spark/kernel`");
    let rtld_object = rtld::load_object(&kernel_elf, &mut vmspace).unwrap();

    log::info!("physical base: {:#018x}", rtld_object.image_base);
    log::info!("virtual base:  {:#018x}", rtld_object.load_base);
    log::info!("reloc slide:   {:#018x}", rtld_object.reloc_base);
    log::info!("entry point:   {:#018x}", kernel_elf.entry_point());

    proto::handle_requests(hartid, &rtld_object, &vmspace, &fdt);

    let tp = rtld_object.allocate_tls(hartid, &mut vmspace);

    let stack_ptr = unsafe {
        extern "C" {
            static __boot_stackp: u8;
        }

        ptr::addr_of!(__boot_stackp).addr()
    };

    let bootinfo = pmm::alloc_frames(pages_for!(type spark::Bootinfo)).unwrap();
    let bootinfo =
        unsafe { &mut *((vmspace.higher_half_start() + bootinfo) as *mut spark::Bootinfo) };

    bootinfo.hart_id = hartid;
    bootinfo.free_list = unsafe { pmm::handoff() };

    let entry_point = rtld_object.entry_point();

    bootinfo.kern_file_len = kernel_data.len();
    bootinfo.kern_file_ptr = kernel_data
        .as_mut_ptr()
        .with_addr(vmspace.higher_half_start() + kernel_data.as_mut_ptr().addr());

    unsafe {
        spinup(entry_point, stack_ptr, 0, bootinfo as _, tp);
    }
}

/// Pass off control to the kernel
///
/// All registers should be cleared to zero (we need to keep one to hold the jump address),
/// disable interrupts and clear the `stvec` CSR.
#[naked]
unsafe extern "C" fn spinup(
    entry_point: usize,
    stack_ptr: usize,
    global_ptr: usize,
    bootinfo: *mut Bootinfo,
    tp: usize,
) -> ! {
    asm!(
        r#"
            mv      t0, a0
            mv      sp, a1
            mv      gp, a2
            mv      a0, a3
            mv      tp, a4
            mv      a1, zero
            mv      a2, zero
            mv      a3, zero
            mv      a4, zero
            mv      a5, zero
            mv      a6, zero
            mv      a7, zero
            mv      s0, zero
            mv      s1, zero
            mv      s2, zero
            mv      s3, zero
            mv      s4, zero
            mv      s5, zero
            mv      s6, zero
            mv      s7, zero
            mv      s8, zero
            mv      s9, zero
            mv      s10, zero
            mv      s11, zero
            mv      t1, zero
            mv      t2, zero
            mv      t3, zero
            mv      t4, zero
            mv      t5, zero
            mv      t6, zero
            // mv      tp, zero
            mv      ra, zero
            csrci   sstatus, 0x2
            csrw    sie, zero
            csrw    stvec, zero
            csrw    sscratch, zero
            jr      t0
        "#,
        options(noreturn)
    )
}

#[allow(dead_code)]
fn print_fdt(fdt: &fdt::Fdt) {
    fn print_fdt_node(node: &fdt::node::FdtNode, depth: &mut usize) {
        (0..*depth).for_each(|_| print!("    "));
        println!("{} {{", node.name);
        *depth += 1;
        for prop in node.properties() {
            (0..*depth).for_each(|_| print!("    "));

            print!("{} = ", prop.name);
            match prop.name {
                "interrupt-map"
                    if node
                        .compatible()
                        .unwrap()
                        .all()
                        .any(|c| c == "pci-host-ecam-generic") =>
                {
                    let mut chunks = prop
                        .value
                        .chunks_exact(4)
                        .map(|c| u32::from_be_bytes(c.try_into().unwrap()));
                    println!("[");
                    while let Some(x) = chunks.next() {
                        let _y = chunks.next().unwrap();
                        let _z = chunks.next().unwrap();
                        let intn = chunks.next().unwrap();
                        let ctrl = chunks.next().unwrap();
                        let cintr = chunks.next().unwrap();

                        let bus = (x >> 16) & 0xff;
                        let dev = (x >> 11) & 0x1f;
                        let func = (x >> 8) & 0x7;

                        println!("  {bus:02x}:{dev:02x}:{func:02x} INT{} on controller {ctrl:#x}, vector {cintr}", (b'A' - 1 + intn as u8) as char);
                    }
                }
                "compatible" => {
                    println!("{:?}", node.compatible().unwrap().all().collect::<Vec<_>>());
                }
                "stdout-path" | "riscv,isa" | "status" | "mmu-type" | "model" | "device_type" => {
                    println!("{}", prop.as_str().unwrap());
                }
                _ => {
                    print!("[");
                    let mut first = true;
                    prop.value.chunks_exact(4).for_each(|c| {
                        if !first {
                            print!(" ");
                        }
                        first = false;
                        print!(
                            "{:#010x}",
                            u32::from_be_bytes(<[u8; 4]>::try_from(c).unwrap())
                        );
                    });
                    println!("]");
                }
            }
        }
        for node in node.children() {
            print_fdt_node(&node, depth);
        }
        *depth -= 1;
        (0..*depth).for_each(|_| print!("    "));
        println!("}};");
    }
    let root = fdt.all_nodes().next().unwrap();
    let mut depth = 0;
    print_fdt_node(&root, &mut depth);
}
