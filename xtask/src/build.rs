use crate::{BuildCtx, SparkBuildOptions};
use clap::Parser;
use xshell::cmd;

#[derive(Parser)]
pub struct Options {
    #[clap(flatten)]
    pub general_options: SparkBuildOptions,

    /// Whether to use `cargo clippy` rather than `cargo build`.
    #[clap(short, long)]
    pub clippy: bool,
}

impl core::ops::Deref for Options {
    type Target = SparkBuildOptions;

    fn deref(&self) -> &Self::Target {
        &self.general_options
    }
}

static REQUIRED_ROOT_DIRS: [&str; 2] = [".hdd/", ".debug/"];

pub fn build(ctx: &BuildCtx, options: Options, document: bool) -> anyhow::Result<()> {
    /* setup default files and folders */
    {
        for root_dir in REQUIRED_ROOT_DIRS {
            if !ctx.shell.path_exists(root_dir) {
                ctx.shell.create_dir(root_dir)?;
            }
        }

        if !ctx.shell.path_exists(".hdd/disk0.img") {
            cmd!(ctx.shell, "qemu-img create -f raw .hdd/disk0.img 256M").run()?;
        }
    }

    /* bootloader */
    {
        let _dir = ctx.shell.push_dir("spark/");

        let cargo_cmd = &ctx.cargo_cmd;
        let cargo_cmd_str = if options.clippy { "clippy" } else { "build" };
        let profile_str = if options.release { "release" } else { "dev" };
        let verbose: &[&str] = if options.verbose { &["-vv"] } else { &[] };
        let target_dir = &ctx.target_dir;

        let linker_script = format!("conf/{}.ld", options.target);
        let _env_linker_script = ctx.shell.push_env("SPARK_LINKER_SCRIPT", &linker_script);

        let mut rustflags = format!(
            "--cfg {} ",
            match options.target {
                crate::Target::riscv_sbi => "sbi",
                crate::Target::riscv_uefi => "uefi",
            }
        );
        if options.verbose {
            rustflags.push_str("-v ");
        }
        let _rustflags = ctx.shell.push_env("RUSTFLAGS", &rustflags);

        let fmt_check: &[&str] = if options.ci { &["--check"] } else { &[] };
        cmd!(ctx.shell, "{cargo_cmd} fmt {verbose...} {fmt_check...}").run()?;

        if !document {
            cmd!(
                ctx.shell,
                "
                    {cargo_cmd} {cargo_cmd_str}
                        --profile {profile_str}
                        --target conf/riscv64gc-unknown-none.json
                        --target-dir {target_dir}
                        {verbose...}
                "
            )
            .run()?;
        } else {
            cmd!(
                ctx.shell,
                "
                    {cargo_cmd} doc
                        --bin spark
                        --document-private-items
                        --profile {profile_str}
                        --target conf/riscv64gc-unknown-none.json
                        --target-dir {target_dir}
                        {verbose...}
                "
            )
            .run()?;
        }
    }

    if !document {
        let profile = if options.release { "release" } else { "debug" };
        let target_elf = format!("target/riscv64gc-unknown-none/{profile}/spark",);
        let spark_elf = format!(".hdd/spark-{}-{profile}.elf", options.target);
        let spark_bin = format!(".hdd/spark-{}-{profile}.bin", options.target);

        // Copy binary to root hdd
        ctx.shell.copy_file(&target_elf, &spark_elf)?;

        // Create a flat binary
        let objcopy = &ctx.objcopy_cmd;
        cmd!(ctx.shell, "{objcopy} -O binary {target_elf} {spark_bin}").run()?;
    }

    Ok(())
}
