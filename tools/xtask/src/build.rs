/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use crate::{
    util::{self, elf_to_binary},
    BuildContext,
};
use std::{
    fs,
    ops::Deref,
    path::{Path, PathBuf},
    process::Command,
};
use xtask::{concat_paths, process::CommandExt};

#[derive(clap::Parser)]
pub struct BuildArguments {
    #[clap(flatten)]
    pub general: crate::Arguments,
}

impl Deref for BuildArguments {
    type Target = crate::Arguments;

    fn deref(&self) -> &Self::Target {
        &self.general
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum BuildCmd {
    Build,
    Check,
    Doc,
}

pub fn main(ctx: &BuildContext, args: &BuildArguments, cmd: BuildCmd) -> anyhow::Result<()> {
    let rustflags = format!(
        "--cfg spark_platform=\"{}\" {} -C panic=unwind -C force-unwind-tables ",
        args.target.platform(),
        if args.verbose { "-v" } else { "" }
    );

    Command::new(&ctx.cargo_cmd)
        .arg("fmt")
        .arg_if(args.verbose, "-vv")
        .arg_if(args.ci, "--check")
        .current_dir(&ctx.paths.spark_dir)
        .env("RUSTFLAGS", &rustflags)
        .execute()?;

    Command::new(&ctx.cargo_cmd)
        .args::<&[_], _>(match cmd {
            BuildCmd::Build => &[
                "build",
                "-Zunstable-options",
                "-Zbuild-std=core,alloc,compiler_builtins",
                "-Zbuild-std-features=compiler-builtins-mem",
            ],
            BuildCmd::Check => &["check"],
            BuildCmd::Doc => &["doc", "--package", "spark", "--document-private-items"],
        })
        .args(["--profile", &ctx.rust_profile, "--target"])
        .arg(&ctx.rust_target)
        .arg("--target-dir")
        .arg(&ctx.paths.rust_out_dir)
        .arg_if(args.verbose, "-vv")
        .current_dir(&ctx.paths.spark_dir)
        .env("RUSTFLAGS", &rustflags)
        .execute()?;

    if cmd == BuildCmd::Build {
        let spark_elf = link_spark(ctx, args, None)?;
        let symbol_map = create_symbol_map(ctx, spark_elf)?;
        let spark_elf = link_spark(ctx, args, Some(symbol_map))?;

        let spark_bin = spark_elf.with_extension("bin");
        elf_to_binary(spark_elf, spark_bin)?;
    }

    Ok(())
}

fn link_spark(
    ctx: &BuildContext,
    args: &BuildArguments,
    symbol_map: Option<PathBuf>,
) -> anyhow::Result<PathBuf> {
    let spark_lib = ctx.paths.rust_build_dir.join("libspark.a");
    let spark_elf = ctx.paths.build_dir.join("spark.elf");
    let linker_script =
        concat_paths!(ctx.paths.spark_dir, "conf", args.target.as_str()).with_extension("ld");

    let mut linker = Command::new(&ctx.linker_cmd);

    linker.args([
        "-static",
        "-pie",
        "--no-dynamic-linker",
        "--whole-archive",
        "--gc-sections",
        "--eh-frame-hdr",
        "-znostart-stop-gc",
        "-zrelro",
        &format!("--script={}", linker_script.display()),
    ]);
    linker.arg("-o").arg(&spark_elf).arg(spark_lib);

    if let Some(symbol_map) = symbol_map {
        linker.arg(symbol_map);
    }

    linker.execute()?;

    Ok(spark_elf)
}

fn create_symbol_map<P: AsRef<Path>>(ctx: &BuildContext, path: P) -> anyhow::Result<PathBuf> {
    let map_bin = ctx.paths.build_dir.join("symbol-map.bin");
    let map_obj = map_bin.with_extension("o");

    fs::write(&map_bin, symbol_map::generate(path)?)?;
    util::rustc_create_obj_from_bin(
        ctx,
        "__symbol_map",
        r#".rodata.__symbol_map,"a",@progbits"#,
        map_bin,
        &map_obj,
    )?;

    Ok(map_obj)
}
