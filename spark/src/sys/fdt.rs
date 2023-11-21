/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

#![cfg(feature = "fdt")]

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

#[allow(dead_code)]
#[cfg(feature = "fdt")]
fn print_fdt(fdt: &fdt::Fdt) {
    fn print_fdt_node(node: &fdt::Node, depth: &mut usize) {
        (0..*depth).for_each(|_| print!("    "));
        println!("{} {{", node.name);
        *depth += 1;
        for prop in node.properties() {
            (0..*depth).for_each(|_| print!("    "));

            print!("{}", prop.name);
            if prop.is_empty() {
                println!(";");
                continue;
            }
            print!(" = ");

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
                    for (n, s) in prop.string_list().unwrap().enumerate() {
                        if n > 0 {
                            print!(", ");
                        }
                        print!("{s:?}");
                    }
                    println!(";");
                }
                "stdout-path" | "riscv,isa" | "status" | "mmu-type" | "model" | "device_type" => {
                    println!("{};", prop.string().unwrap());
                }
                _ => {
                    print!("<");
                    for (n, cell) in prop.as_cell_slice().iter().enumerate() {
                        if n > 0 {
                            print!(", ");
                        }
                        print!("{cell:#0110x}");
                    }
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
    let root = fdt.root();
    let mut depth = 0;
    print_fdt_node(&root, &mut depth);
}
