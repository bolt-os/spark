#![feature(exit_status_error)]

pub mod build;
pub mod runner;

use clap::{AppSettings, Parser};
use core::fmt;
use std::{env, path::PathBuf, process::Command, str::FromStr};

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target {
    riscv_sbi,
    riscv_uefi,
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Target::riscv_sbi => write!(f, "riscv-sbi"),
            Target::riscv_uefi => write!(f, "riscv-uefi"),
        }
    }
}

impl FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "riscv-sbi" => Ok(Self::riscv_sbi),
            "riscv-uefi" => Ok(Self::riscv_uefi),
            _ => Err(format!("{} is not a supported architecture", s)),
        }
    }
}

#[derive(Clone, Parser)]
pub struct SparkBuildOptions {
    #[clap(long, default_value = "riscv-sbi")]
    pub target: Target,

    #[clap(long)]
    pub release: bool,

    #[clap(short, long)]
    pub verbose: bool,

    #[clap(long)]
    pub ci: bool,
}

pub struct BuildCtx {
    shell: xshell::Shell,
    target_dir: PathBuf,
    cargo_cmd: String,
    objcopy_cmd: PathBuf,
}

fn find_objcopy() -> anyhow::Result<PathBuf> {
    if let Ok(llvm_tools) = llvm_tools::LlvmTools::new() {
        if let Some(llvm_objcopy) = llvm_tools.tool(&llvm_tools::exe("llvm-objcopy")) {
            return Ok(llvm_objcopy);
        }
    }

    let shell = xshell::Shell::new()?;

    for cmd in ["llvm-objcopy", "riscv64-unknown-elf-objcopy", "objcopy"] {
        if shell.cmd(cmd).arg("-V").quiet().run().is_ok() {
            return Ok(PathBuf::from(cmd));
        }
    }

    Err(anyhow::anyhow!(concat!(
        "Cannot find a usable objcopy.\n",
        "\n",
        "Make sure an objcopy which supports the RISC-V architecture is in your PATH.\n",
        "Optionally, you can instead install the `llvm-tools-preview` component via rustup:\n",
        "    rustup component add llvm-tools-preview\n",
    )))
}

impl BuildCtx {
    fn new() -> anyhow::Result<BuildCtx> {
        let mut target_dir = env::current_dir()?;
        target_dir.push("target");

        /*
         * When we're invoked through cargo, it will set $CARGO to its path.
         * This may be different than what we get with `cargo`.
         */
        let cargo_cmd = match env::var("CARGO") {
            Ok(cmd) => cmd,
            Err(_) => "cargo".into(),
        };

        Ok(Self {
            target_dir,
            cargo_cmd,
            shell: xshell::Shell::new()?,
            objcopy_cmd: find_objcopy()?,
        })
    }
}

#[derive(Parser)]
#[clap(rename_all = "snake_case", setting = AppSettings::DisableVersionFlag)]
enum Arguments {
    Build(build::Options),
    Doc(build::Options),
    Run(runner::Options),
}

fn main() -> anyhow::Result<()> {
    let ctx = BuildCtx::new()?;

    match Arguments::parse() {
        Arguments::Build(build_options) => build::build(&ctx, build_options, false)?,
        Arguments::Run(run_options) => runner::run(&ctx, run_options)?,
        Arguments::Doc(build_options) => build::build(&ctx, build_options, true)?,
    }

    Ok(())
}

fn run_command(mut cmd: Command) -> anyhow::Result<()> {
    eprintln!(
        "+ {} {}",
        cmd.get_program().to_string_lossy(),
        cmd.get_args()
            .collect::<Vec<_>>()
            .join(" ".as_ref())
            .to_string_lossy()
    );
    cmd.spawn()?.wait()?.exit_ok()?;
    Ok(())
}
