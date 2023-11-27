/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use crate::{build, BuildContext, Target};
use std::{ffi::OsString, ops::Deref, path::PathBuf, process::Command};
use xtask::{concat_paths, process::CommandExt};

#[derive(clap::Parser)]
pub struct RunArguments {
    #[clap(flatten)]
    pub general: crate::Arguments,
    /// Machine to emulate
    #[clap(long, default_value = "virt")]
    pub machine: String,
    /// CPU to emulate
    #[clap(long, default_value = "rv64")]
    pub cpu: String,
    /// Number of CPUs to emulate
    #[clap(long, default_value = "4")]
    pub smp: String,
    /// RAM size in MB
    #[clap(long, default_value = "512")]
    pub ram: String,
    /// Enable debug logging
    #[clap(long, default_value = "int,guest_errors")]
    pub log: Option<Option<String>>,
    /// Wait for a debugger to attach
    #[clap(short, long)]
    pub debugger: bool,
    /// Path to QEMU executable
    #[clap(long, default_value = "qemu-system-riscv64")]
    pub qemu: PathBuf,
    /// Extra arguments to pass to QEMU
    #[clap(last = true)]
    pub qemu_args: Vec<OsString>,
}

impl Deref for RunArguments {
    type Target = crate::Arguments;

    fn deref(&self) -> &Self::Target {
        &self.general
    }
}

pub fn main(ctx: &BuildContext, args: &RunArguments) -> anyhow::Result<()> {
    {
        let build_args = build::BuildArguments {
            general: args.general.clone(),
        };
        build::main(ctx, &build_args, build::BuildCmd::Build)?;
    }

    let spark_bin = ctx.paths.build_dir.join("spark.bin");
    let mut qemu = Command::new(&args.qemu);

    #[rustfmt::skip]
     qemu.args([
         "-machine", &args.machine,
         "-cpu", &args.cpu,
         "-smp", &args.smp,
         "-m", &args.ram,

         "-serial", "mon:stdio",
     ]);

    if args.debugger {
        qemu.args(["-s", "-S"]);
    }

    if let Some(opts) = &args.log {
        let log_path = concat_paths!(ctx.paths.build_dir, "log", "cpu%d.txt");
        xtask::fs::make_dir(log_path.parent().unwrap())?;
        qemu.arg("-D")
            .arg(log_path)
            .arg("-d")
            .arg(opts.as_deref().unwrap_or("int,guest_errors"));
    }

    match args.target {
        Target::riscv64_sbi => {
            qemu.arg("-kernel").arg(spark_bin);
        }
        Target::riscv64_uefi => {
            todo!("need to fetch ovmf");
        }
    }

    qemu.args(&args.qemu_args).execute()?;

    Ok(())
}
