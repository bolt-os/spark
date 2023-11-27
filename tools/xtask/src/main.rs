/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

#![feature(exit_status_error)]

mod build;
mod run;
mod util;

use build::BuildCmd;
use std::{env, ffi::OsString, fmt, ops::Deref, path::PathBuf, process::Command, str::FromStr};
use xtask::concat_paths;

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Target {
    riscv64_sbi,
    riscv64_uefi,
}

impl Target {
    fn as_str(self) -> &'static str {
        match self {
            Self::riscv64_sbi => "riscv64-sbi",
            Self::riscv64_uefi => "riscv64-uefi",
        }
    }

    fn arch(self) -> &'static str {
        match self {
            Self::riscv64_sbi | Self::riscv64_uefi => "riscv64",
        }
    }

    fn platform(self) -> &'static str {
        match self {
            Self::riscv64_sbi => "sbi",
            Self::riscv64_uefi => "uefi",
        }
    }

    fn rust_triple(self) -> &'static str {
        match self {
            Self::riscv64_sbi | Self::riscv64_uefi => "riscv64imac-unknown-none",
        }
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "riscv64-sbi" => Ok(Self::riscv64_sbi),
            "riscv64-uefi" => Ok(Self::riscv64_uefi),
            _ => Err(format!("`{s}` is not a supported target")),
        }
    }
}

struct Paths {
    spark_dir: PathBuf,      // /spark
    build_dir: PathBuf,      // /<build_dir>/<target>/<profile>
    rust_out_dir: PathBuf,   // /<build_dir>/target
    rust_build_dir: PathBuf, // /<rust_target_dir>/<rust_target>/<profile>
}

struct BuildContext {
    cargo_cmd: OsString,
    rustc_cmd: OsString,
    linker_cmd: OsString,
    paths: Paths,
    rust_profile: String,
    rust_target: OsString,
}

#[derive(Clone, clap::Parser)]
pub struct Arguments {
    #[clap(long, default_value = "riscv64-sbi")]
    target: Target,
    #[clap(long)]
    release: bool,
    #[clap(short, long)]
    verbose: bool,
    #[clap(long)]
    ci: bool,

    #[clap(long)]
    cargo: Option<OsString>,
    #[clap(long)]
    rustc: Option<OsString>,
    #[clap(long)]
    linker: Option<OsString>,
}

#[derive(clap::Parser)]
enum Subcommand {
    #[clap(alias = "b")]
    Build(build::BuildArguments),
    #[clap(alias = "c")]
    Check(build::BuildArguments),
    #[clap(alias = "d")]
    Doc(build::BuildArguments),
    #[clap(alias = "r")]
    Run(run::RunArguments),
}

impl Deref for Subcommand {
    type Target = Arguments;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Build(args) => args,
            Self::Check(args) => args,
            Self::Doc(args) => args,
            Self::Run(args) => args,
        }
    }
}

macro_rules! default_var {
    ($args:expr, $name:ident, $var:expr, $default:expr) => {
        'b: {
            if let Some(cmd) = &$args.$name {
                break 'b cmd.clone();
            }
            if let Some(cmd) = env::var_os($var) {
                break 'b cmd;
            }
            OsString::from($default)
        }
    };
}

fn find_linker(args: &Arguments) -> anyhow::Result<OsString> {
    let cmds: &[_] = match args.target {
        Target::riscv64_sbi | Target::riscv64_uefi => {
            &["ld.lld", "riscv64-unknown-elf-ld", "riscv64-elf-ld", "ld"]
        }
    };

    for cmd in cmds {
        match check_linker(args, cmd) {
            Ok(true) => return Ok(cmd.into()),
            Ok(false) => (),
            Err(error) => {
                eprintln!("{error}");
            }
        }
    }

    let arch = args.target.arch();

    Err(anyhow::anyhow!(
        "Cannot find a suitable linker.\n\
         Make sure a linker that supports the {arch} architecture is in your PATH,\
         or specify one with the `--linker` argument or the `LD` environment variable.",
    ))
}

fn check_linker(args: &Arguments, cmd: &str) -> anyhow::Result<bool> {
    let output = Command::new(cmd).arg("-V").output()?;
    output.status.exit_ok()?;
    let stdout = String::from_utf8(output.stdout)?;

    if stdout.starts_with("LLD") || stdout.contains(" LLD ") {
        return Ok(true);
    }

    let arch = match args.target {
        Target::riscv64_sbi | Target::riscv64_uefi => "elf64lriscv",
    };
    if stdout.starts_with("GNU ld") && stdout.contains(arch) {
        return Ok(true);
    }

    Ok(false)
}

fn main() -> anyhow::Result<()> {
    let args = <Subcommand as clap::Parser>::parse();
    let ctx = {
        let (profile, rust_profile) = match args.release {
            true => ("release", "release"),
            false => ("debug", "dev"),
        };

        let pwd = env::current_dir()?;
        let spark_dir = pwd.join("spark");
        let out_dir = pwd.join("build");
        let build_dir = concat_paths!(out_dir, args.target.as_str(), profile);
        let rust_out_dir = out_dir.join("target");
        let rust_build_dir = concat_paths!(rust_out_dir, args.target.rust_triple(), profile);

        xtask::fs::make_dir(&build_dir)?;

        let rust_target = match args.target {
            Target::riscv64_sbi | Target::riscv64_uefi => {
                concat_paths!(pwd, "riscv64imac-unknown-none.json").into_os_string()
            }
        };

        BuildContext {
            cargo_cmd: default_var!(args, cargo, "CARGO", "cargo"),
            rustc_cmd: default_var!(args, rustc, "RUSTC", "rustc"),
            linker_cmd: default_var!(args, linker, "LD", find_linker(&args)?),
            paths: Paths {
                spark_dir,
                build_dir,
                rust_out_dir,
                rust_build_dir,
            },
            rust_profile: rust_profile.to_string(),
            rust_target,
        }
    };

    match args {
        Subcommand::Build(args) => build::main(&ctx, &args, BuildCmd::Build)?,
        Subcommand::Check(args) => build::main(&ctx, &args, BuildCmd::Check)?,
        Subcommand::Doc(args) => build::main(&ctx, &args, BuildCmd::Doc)?,
        Subcommand::Run(args) => run::main(&ctx, &args)?,
    }

    Ok(())
}
