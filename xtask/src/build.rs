use crate::{BuildCtx, SparkBuildOptions, Target};
use clap::Parser;
use std::{ffi::OsStr, process::Command};
use xtask::{concat_paths, process::CommandExt};

#[derive(Parser)]
pub struct Options {
    #[clap(flatten)]
    pub general_options: SparkBuildOptions,
}

impl core::ops::Deref for Options {
    type Target = SparkBuildOptions;

    fn deref(&self) -> &Self::Target {
        &self.general_options
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuildCmd {
    Build,
    Check,
    Doc,
}

pub fn build(ctx: &BuildCtx, options: Options, cmd: BuildCmd) -> anyhow::Result<()> {
    let spark_dir = concat_paths!(ctx.pwd, "spark");
    let rust_target =
        concat_paths!(spark_dir, "conf", options.target.triple()).with_extension("json");
    let linker_script =
        concat_paths!(spark_dir, "conf", options.target.to_string()).with_extension("ld");

    let rustflags = format!(
        "--cfg {} {}",
        match options.target {
            crate::Target::riscv_sbi => "sbi",
            crate::Target::riscv_uefi => "uefi",
        },
        if options.verbose { "-v " } else { "" }
    );

    let envs: &[(&dyn AsRef<OsStr>, &dyn AsRef<OsStr>)] = &[
        (&"RUSTFLAGS", &rustflags),
        (&"SPARK_LINKER_SCRIPT", &linker_script),
    ];

    // Format the source.
    {
        let mut cargo = Command::new(&ctx.cargo_cmd);

        cargo.arg("fmt");

        if options.verbose {
            cargo.arg("-vv");
        }
        if options.ci {
            cargo.arg("--check");
        }

        cargo
            .current_dir(&spark_dir)
            .envs(envs.iter().copied())
            .execute()?;
    }

    // Run cargo command.
    let mut cargo = Command::new(&ctx.cargo_cmd);
    cargo
        .args::<&[_], _>(match cmd {
            BuildCmd::Build => &["build"],
            BuildCmd::Check => &["clippy"],
            BuildCmd::Doc => &["doc", "--bin", "spark", "--document-private-items"],
        })
        .args(["--profile", if options.release { "release" } else { "dev" }])
        .arg("--target")
        .arg(&rust_target)
        .arg("--target-dir")
        .arg(&ctx.target_dir);
    if options.verbose {
        cargo.arg("-vv");
    }
    cargo
        .current_dir(&spark_dir)
        .envs(envs.iter().copied())
        .execute()?;

    // Actually building the bootloader requires some more steps.
    if cmd == BuildCmd::Build {
        // Copy binary to build directory
        let profile = if options.release { "release" } else { "debug" };
        let target_elf = concat_paths!(ctx.target_dir, options.target.triple(), profile, "spark");
        let spark_elf = concat_paths!(
            ctx.build_dir,
            format!("spark-{}-{profile}.elf", options.target)
        );
        xtask::fs::copy(target_elf, &spark_elf)?;

        // Create a flat binary
        let spark_bin = spark_elf.with_extension("bin");
        Command::new(&ctx.objcopy_cmd)
            .args(["-O", "binary"])
            .arg(spark_elf)
            .arg(spark_bin)
            .execute()?;
    }

    Ok(())
}
