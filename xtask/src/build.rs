use crate::{BuildCtx, SparkBuildOptions};
use clap::Parser;
use std::ffi::OsStr;
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
    let rust_target =
        concat_paths!(ctx.paths.spark_dir, "conf", options.target.triple()).with_extension("json");
    let linker_script =
        concat_paths!(ctx.paths.spark_dir, "conf", options.target.to_string()).with_extension("ld");

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
    ctx.cmds
        .cargo()
        .arg("fmt")
        .arg_if(options.verbose, "-vv")
        .arg_if(options.ci, "--check")
        .current_dir(&ctx.paths.spark_dir)
        .envs(envs.iter().copied())
        .execute()?;

    // Run cargo command.
    ctx.cmds
        .cargo()
        .args::<&[_], _>(match cmd {
            BuildCmd::Build => &["build"],
            BuildCmd::Check => &["clippy"],
            BuildCmd::Doc => &["doc", "--bin", "spark", "--document-private-items"],
        })
        .args(["--profile", &ctx.rust_profile, "--target"])
        .arg(&rust_target)
        .arg("--target-dir")
        .arg(&ctx.paths.rust_target_dir)
        .arg_if(options.verbose, "-vv")
        .current_dir(&ctx.paths.spark_dir)
        .envs(envs.iter().copied())
        .execute()?;

    // Actually building the bootloader requires some more steps.
    if cmd == BuildCmd::Build {
        // Copy binary to build directory
        let target_elf = ctx.paths.rust_build_dir.join("spark");
        let spark_elf = ctx.paths.build_dir.join("spark.elf");
        xtask::fs::copy(target_elf, &spark_elf)?;

        // Create a flat binary
        let spark_bin = spark_elf.with_extension("bin");
        ctx.cmds
            .objcopy()
            .args(["-O", "binary"])
            .args([spark_elf, spark_bin])
            .execute()?;
    }

    Ok(())
}
