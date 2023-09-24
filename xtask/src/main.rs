#![feature(exit_status_error)]

pub mod build;
pub mod runner;

use build::BuildCmd;
use clap::Parser;
use core::fmt;
use std::{env, ffi::OsString, path::PathBuf, process::Command, str::FromStr};
use xtask::concat_paths;

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Target {
    riscv_sbi,
    riscv_uefi,
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
            "riscv-sbi" => Ok(Self::riscv_sbi),
            "riscv-uefi" => Ok(Self::riscv_uefi),
            _ => Err(format!("{} is not a supported architecture", s)),
        }
    }
}

impl Target {
    const fn as_str(self) -> &'static str {
        match self {
            Target::riscv_sbi => "riscv-sbi",
            Target::riscv_uefi => "riscv-uefi",
        }
    }

    const fn triple(self) -> &'static str {
        match self {
            Target::riscv_sbi => "riscv64gc-unknown-none",
            Target::riscv_uefi => "riscv64gc-unknown-uefi",
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

    #[clap(long)]
    pub cargo: Option<OsString>,
    #[clap(long)]
    pub objcopy: Option<OsString>,
}

macro_rules! common_cmds {
    ($($name:ident $(, $args:ident)?;)*) => {
        struct CommonCmds {
            $($name: OsString $(, $args: Vec<OsString>)?),*
        }

        impl CommonCmds {
            $(
                fn $name(&self) -> Command {
                    #[allow(unused_mut)]
                    let mut cmd = Command::new(&self.$name);
                    $(cmd.args(&self.$args);)?
                    cmd
                }
            )*
        }
    };
}

common_cmds! {
    cargo;
}

struct CommonPaths {
    spark_dir: PathBuf,
    rust_target_dir: PathBuf,
    rust_build_dir: PathBuf,
    build_dir: PathBuf,
}

pub struct BuildCtx {
    cmds: CommonCmds,
    paths: CommonPaths,
    rust_profile: String,
}

impl BuildCtx {
    fn new(opts: &SparkBuildOptions) -> anyhow::Result<BuildCtx> {
        let (profile, rust_profile) = if opts.release {
            ("release", "release")
        } else {
            ("debug", "dev")
        };

        let pwd = env::current_dir()?;
        let rust_target_dir = pwd.join("target");
        let paths = CommonPaths {
            build_dir: concat_paths!(pwd, "build", opts.target.as_str(), profile),
            rust_build_dir: concat_paths!(rust_target_dir, opts.target.triple(), profile),
            rust_target_dir,
            spark_dir: pwd.join("spark"),
        };

        xtask::fs::make_dir(&paths.build_dir)?;

        macro_rules! find_command {
            ($opt:ident, $env:expr, $def:expr $(,)?) => {
                opts.$opt
                    .clone()
                    .unwrap_or_else(|| env::var_os($env).unwrap_or_else(|| $def))
                    .into()
            };
        }

        let cmds = CommonCmds {
            cargo: find_command!(cargo, "CARGO", "rustc".into()),
        };

        Ok(Self {
            cmds,
            paths,
            rust_profile: rust_profile.to_string(),
        })
    }
}

#[derive(Parser)]
enum Arguments {
    Build(build::Options),
    Check(build::Options),
    Doc(build::Options),
    Run(runner::Options),
}

impl Arguments {
    fn global_opts(&self) -> &SparkBuildOptions {
        match self {
            Arguments::Build(opts) => opts,
            Arguments::Check(opts) => opts,
            Arguments::Doc(opts) => opts,
            Arguments::Run(opts) => opts,
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();
    let ctx = BuildCtx::new(args.global_opts())?;

    match args {
        Arguments::Build(build_options) => build::build(&ctx, build_options, BuildCmd::Build)?,
        Arguments::Check(build_options) => build::build(&ctx, build_options, BuildCmd::Check)?,
        Arguments::Doc(build_options) => build::build(&ctx, build_options, BuildCmd::Doc)?,
        Arguments::Run(run_options) => runner::run(&ctx, run_options)?,
    }

    Ok(())
}
