/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use crate::BuildContext;
use elf::{Elf, SegmentKind};
use std::{
    fs::{self, File},
    io::{Seek, SeekFrom, Write},
    path::Path,
    process::{Command, Stdio},
};
use xtask::process::CommandExt;

pub fn rustc_create_obj_from_bin<Pb: AsRef<Path>, Po: AsRef<Path>>(
    ctx: &BuildContext,
    name: &str,
    section_name: &str,
    bin_path: Pb,
    obj_path: Po,
) -> anyhow::Result<()> {
    rustc_create_obj_from_bin_(
        ctx,
        name,
        section_name,
        bin_path.as_ref(),
        obj_path.as_ref(),
    )
}

fn rustc_create_obj_from_bin_(
    ctx: &BuildContext,
    name: &str,
    section_name: &str,
    bin_path: &Path,
    obj_path: &Path,
) -> anyhow::Result<()> {
    let mut rustc = Command::new(&ctx.rustc_cmd)
        .args(["+nightly", "--emit", "obj", "-", "--target"])
        .arg(&ctx.rust_target)
        .arg("-o")
        .arg(obj_path)
        .log_command()
        .stdin(Stdio::piped())
        .log_command()
        .spawn()?;

    rustc.stdin.take().unwrap().write_all(
        format!(
            r##"
             #![allow(internal_features)]
             #![feature(no_core, rustc_attrs)]
             #![no_core]
             #![no_main]

             #[rustc_builtin_macro]
             macro_rules! global_asm {{ () => (); }}

             global_asm!(
                 r#"
                 .pushsection {section_name}

                 .global {name}
                 .p2align 4
                 {name}:
                 .incbin "{}"
                 .size {name}, . - {name}

                 .global {name}_size
                 .set {name}_size, . - {name}

                 .popsection
                 "#
             );
             "##,
            bin_path.display()
        )
        .as_bytes(),
    )?;

    rustc.wait()?.exit_ok()?;

    Ok(())
}

pub fn elf_to_binary<Pe: AsRef<Path>, Pb: AsRef<Path>>(
    elf_path: Pe,
    bin_path: Pb,
) -> anyhow::Result<()> {
    elf_to_binary_(elf_path.as_ref(), bin_path.as_ref())
}

fn elf_to_binary_(elf_path: &Path, bin_path: &Path) -> anyhow::Result<()> {
    let file_data = fs::read(elf_path)?;
    let elf = Elf::new(&file_data).unwrap();
    let mut out = File::create(bin_path)?;

    for segment in elf.segments().filter(|s| s.kind() == SegmentKind::Load) {
        out.seek(SeekFrom::Start(segment.virtual_address()))?;
        out.write_all(segment.file_data())?;
    }

    Ok(())
}
