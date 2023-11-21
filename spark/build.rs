// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use std::env;

fn main() {
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
    if target_os == "uefi" {
        println!("cargo:rustc-cfg=uefi");
    } else {
        println!("cargo:rustc-cfg=sbi");
    }
}
