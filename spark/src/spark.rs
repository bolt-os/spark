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
    prelude_import,
    array_windows,                      // https://github.com/rust-lang/rust/issues/75027
    asm_const,                          // https://github.com/rust-lang/rust/issues/93332
    get_mut_unchecked,                  // https://github.com/rust-lang/rust/issues/63292
    maybe_uninit_slice,                 // https://github.com/rust-lang/rust/issues/63569
    naked_functions,                    // https://github.com/rust-lang/rust/issues/32408
    never_type,                         // https://github.com/rust-lang/rust/issues/35121
    new_uninit,                         // https://github.com/rust-lang/rust/issues/63291
    offset_of,                          // https://github.com/rust-lang/rust/issues/106655
    pointer_byte_offsets,               // https://github.com/rust-lang/rust/issues/96283
    pointer_is_aligned,                 // https://github.com/rust-lang/rust/issues/96284
    ptr_metadata,                       // https://github.com/rust-lang/rust/issues/81513
    result_option_inspect,              // https://github.com/rust-lang/rust/issues/91345
    slice_flatten,                      // https://github.com/rust-lang/rust/issues/95629
    slice_ptr_get,                      // https://github.com/rust-lang/rust/issues/74265
    slice_ptr_len,                      // https://github.com/rust-lang/rust/issues/71146
    strict_provenance,                  // https://github.com/rust-lang/rust/issues/95228
    sync_unsafe_cell,                   // https://github.com/rust-lang/rust/issues/95439
)]
#![reexport_test_harness_main = "test_main"]
#![test_runner(test::runner)]
#![warn(clippy::cargo, clippy::pedantic)]
#![deny(
    clippy::semicolon_if_nothing_returned,
    clippy::debug_assert_with_mut_call
)]
#![allow(
    internal_features,
    clippy::cast_lossless,
    clippy::cast_possible_truncation,
    clippy::cast_ptr_alignment,
    clippy::enum_glob_use,
    clippy::inline_always,
    clippy::items_after_statements,
    clippy::module_name_repetitions,
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

mod config;
mod dev;
mod fs;
mod io;
mod malloc;
mod mem;
mod panic;
mod proto;
mod rtld;
mod smp;
mod test;
mod time;
mod trap;

pub use anyhow::Result;
pub use mem::{pmm, vmm};

use config::Value;
use core::sync::atomic::{AtomicUsize, Ordering};
#[cfg(uefi)]
use uefi::{
    proto::riscv::RiscvBoot,
    table::{SystemTable, TableGuid},
    Handle,
};

#[cfg(uefi)]
global_asm!(include_str!("locore-uefi.s"), options(raw));
#[cfg(sbi)]
global_asm!(include_str!("locore.s"), options(raw));

#[cfg(uefi)]
global_asm!(
    r#"
    .section .data.sbat
    sbat:
        .ascii  "sbat,1,SBAT Version,sbat,1,https://github.com/rhboot/shim/blob/main/SBAT.md\n"
    "#,
    concat!(
        r#".ascii  "spark,1,Spark,spark,"#,
        env!("CARGO_PKG_VERSION"),
        r#",https://github.com/bolt-os/spark\n""#,
    ),
    "__sbat_endv:",
);

pub fn hcf() -> ! {
    println!("bruh.");
    loop {
        core::hint::spin_loop();
    }
}

static BOOT_HART_ID: AtomicUsize = AtomicUsize::new(0);

#[cfg(uefi)]
#[no_mangle]
#[allow(clippy::missing_panics_doc)]
pub extern "C" fn spark_main(image: Handle, system_table: &'static SystemTable) -> ! {
    unsafe { uefi::bootstrap(image, system_table) };

    io::init();

    // Print the address we've been loaded to for easier debugging.
    let image_base: usize;
    unsafe {
        asm!("lla {}, __image_base", out(reg) image_base, options(nomem, nostack));
    }
    log::debug!("image base: {image_base:#x}");

    let boot_services = uefi::boot_services();

    let mut riscv_boot_proto = boot_services
        .first_protocol::<RiscvBoot>()
        .expect("risc-v boot protocol is not available");

    let hartid = riscv_boot_proto
        .get_boot_hartid()
        .expect("failed to get bsp's hart id");
    BOOT_HART_ID.store(hartid, Ordering::Relaxed);

    let config_table = system_table.config_table();

    if let Some(ptr) = config_table.get_table(TableGuid::ACPI_20) {
        dev::acpi::init(ptr.cast());
    } else if let Some(ptr) = config_table.get_table(TableGuid::ACPI) {
        dev::acpi::init(ptr.cast());
    }

    if let Some(ptr) = config_table.get_table(TableGuid::DEVICE_TREE) {
        dev::fdt::init(ptr.cast());
    }

    dev::init();

    main();
}

#[cfg(sbi)]
#[allow(clippy::not_unsafe_ptr_arg_deref, clippy::missing_panics_doc)]
#[no_mangle]
pub extern "C" fn spark_main(hartid: usize, dtb_ptr: *mut u8) -> ! {
    // Initialize the logger
    //  TODO: Use legacy SBI console until we probe for consoles.
    io::init();

    BOOT_HART_ID.store(hartid, Ordering::Relaxed);

    // Install the Device Tree
    let fdt = dev::fdt::init(dtb_ptr);

    // TODO: the platform stuff in `fdt::init()` should be pulled out

    // Bootstrap memory allocation
    pmm::init_from_fdt(fdt, dtb_ptr);

    // TODO: Probe console devices

    // Probe the full device tree before we search for a boot partition
    dev::init();

    main();
}

static SPARK_CFG_PATHS: &[&str] = &["/boot/spark.cfg", "/spark.cfg"];

fn main() -> ! {
    // Search each volume on each disk for the config file.
    let mut config_file = 'b: {
        for disk in dev::block::DISKS.read().iter() {
            for volume in disk.volumes() {
                let mut root = match fs::mount(volume) {
                    Ok(file) => file,
                    Err(io::Error::Unsupported) => {
                        // no driver for this filesystem
                        continue;
                    }
                    Err(err) => {
                        log::warn!("error mounting volume: {err:?}");
                        continue;
                    }
                };

                for path in SPARK_CFG_PATHS {
                    match root.open(path) {
                        Ok(file) => {
                            break 'b file;
                        }
                        Err(io::Error::NotFound) => {}
                        Err(err) => {
                            log::warn!("error opening path {path:?}: {err:?}");
                            continue;
                        }
                    }
                }
            }
        }

        panic!("cannot find `spark.cfg` on any device");
    };

    let boot_config_data = config_file.read_to_end().unwrap();
    let boot_config = config::parse_config_file(&boot_config_data);

    let boot_entry = boot_config.entries.first().expect("no boot entry");
    let protocol = match boot_entry.param("protocol") {
        Some(Value::String(proto)) => *proto,
        None => panic!("`protocol` parameter was not specified"),
        _ => panic!("`protocol` parameter is not a string"),
    };

    match protocol {
        #[cfg(feature = "proto-bootelf")]
        "bootelf" => proto::bootelf::main(config_file, boot_entry).unwrap(),
        #[cfg(feature = "proto-limine")]
        "limine" => proto::limine::main(config_file, boot_entry).unwrap(),
        _ => panic!("protocol `{protocol}` is not supported"),
    }
}

#[allow(dead_code)]
#[cfg(feature = "fdt")]
fn print_fdt(fdt: &fdt::Fdt) {
    fn print_fdt_node(node: &fdt::node::FdtNode, depth: &mut usize) {
        (0..*depth).for_each(|_| print!("    "));
        println!("{} {{", node.name);
        *depth += 1;
        for prop in node.properties() {
            (0..*depth).for_each(|_| print!("    "));

            print!("{}", prop.name);
            match prop.name {
                //                 "interrupt-map"
                //                     if node
                //                         .compatible()
                //                         .unwrap()
                //                         .all()
                //                         .any(|c| c == "pci-host-ecam-generic") =>
                //                 {
                //                     let mut chunks = prop
                //                         .value
                //                         .chunks_exact(4)
                //                         .map(|c| u32::from_be_bytes(c.try_into().unwrap()));
                //                     println!("[");
                //                     while let Some(x) = chunks.next() {
                //                         let _y = chunks.next().unwrap();
                //                         let _z = chunks.next().unwrap();
                //                         let intn = chunks.next().unwrap();
                //                         let ctrl = chunks.next().unwrap();
                //                         let cintr = chunks.next().unwrap();
                //
                //                         let bus = (x >> 16) & 0xff;
                //                         let dev = (x >> 11) & 0x1f;
                //                         let func = (x >> 8) & 0x7;
                //
                //                         println!("  {bus:02x}:{dev:02x}:{func:02x} INT{} on controller {ctrl:#x}, vector {cintr}", (b'A' - 1 + intn as u8) as char);
                //                     }
                //                 }
                "compatible" => {
                    println!(
                        " = {};",
                        node.compatible()
                            .unwrap()
                            .all()
                            .map(|c| format!("{c:?}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                "stdout-path" | "riscv,isa" | "status" | "mmu-type" | "model" | "device_type" => {
                    println!(" = {};", prop.as_str().unwrap());
                }
                _ => {
                    if prop.value.is_empty() {
                        println!(";");
                        continue;
                    }
                    print!(" = <");
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
                    println!(">;");
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
