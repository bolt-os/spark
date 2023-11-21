// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

#![doc = include_str!("../../README.md")]
#![no_std]
#![no_main]
#![feature(
    custom_test_frameworks,
    prelude_import,
    arbitrary_self_types,               // https://github.com/rust-lang/rust/issues/44874
    array_windows,                      // https://github.com/rust-lang/rust/issues/75027
    asm_const,                          // https://github.com/rust-lang/rust/issues/93332
    const_mut_refs,                     // https://github.com/rust-lang/rust/issues/57349
    decl_macro,                         // https://github.com/rust-lang/rust/issues/39412
    get_mut_unchecked,                  // https://github.com/rust-lang/rust/issues/63292
    maybe_uninit_slice,                 // https://github.com/rust-lang/rust/issues/63569
    naked_functions,                    // https://github.com/rust-lang/rust/issues/32408
    never_type,                         // https://github.com/rust-lang/rust/issues/35121
    new_uninit,                         // https://github.com/rust-lang/rust/issues/63291
    offset_of,                          // https://github.com/rust-lang/rust/issues/106655
    pointer_is_aligned,                 // https://github.com/rust-lang/rust/issues/96284
    result_option_inspect,              // https://github.com/rust-lang/rust/issues/91345
    slice_flatten,                      // https://github.com/rust-lang/rust/issues/95629
    slice_ptr_get,                      // https://github.com/rust-lang/rust/issues/74265
    slice_ptr_len,                      // https://github.com/rust-lang/rust/issues/71146
    strict_provenance,                  // https://github.com/rust-lang/rust/issues/95228
)]
#![reexport_test_harness_main = "test_main"]
#![test_runner(test::runner)]
#![warn(clippy::pedantic)]
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
#[allow(unused_imports)]
mod prelude {
    pub use crate::console::{print, println};
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
mod console;
mod dev;
mod fs;
mod io;
mod malloc;
mod mem;
mod panic;
mod proto;
mod rtld;
mod smp;
mod sys;
mod test;
mod time;
mod trap;
mod util;

pub use anyhow::Result;
pub use mem::{pmm, vmm};

use config::Value;
use core::sync::atomic::AtomicUsize;

pub fn hcf() -> ! {
    println!("bruh.");
    loop {
        core::hint::spin_loop();
    }
}

static BOOT_HART_ID: AtomicUsize = AtomicUsize::new(0);

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
