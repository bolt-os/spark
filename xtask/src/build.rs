use clap::{clap_derive::ArgEnum, Parser};
use std::path::PathBuf;
use xshell::{cmd, Shell};

#[allow(non_camel_case_types)]
#[derive(ArgEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    x64,
    rv64,
}

#[derive(Parser)]
pub struct Options {
    /// Whether the current build is a release build.
    #[clap(long)]
    release: bool,

    /// Whether to use `cargo clippy` rather than `cargo build`.
    #[clap(short, long)]
    clippy: bool,
}

static REQUIRED_ROOT_DIRS: [&str; 2] = [".hdd/", ".debug/"];

pub fn build(options: Options) -> Result<(), xshell::Error> {
    let shell = Shell::new()?;

    /* setup default files and folders */
    {
        for root_dir in REQUIRED_ROOT_DIRS {
            let path = PathBuf::from(root_dir);
            if !shell.path_exists(&path) {
                shell.create_dir(path)?;
            }
        }

        if !shell.path_exists(".hdd/disk0.img") {
            cmd!(shell, "qemu-img create -f raw .hdd/disk0.img 256M").run()?;
        }
    }

    /* bootloader */
    {
        let _dir = shell.push_dir("spark/");

        let cargo_cmd_str = format!("{}", if options.clippy { "clippy" } else { "build" });
        let profile_str = format!("{}", if options.release { "release" } else { "dev" });

        let _rustflags = shell.push_env("RUSTFLAGS", "-C link-arg=-Tconf/virt.ld");
        cmd!(shell, "cargo fmt").run()?;
        cmd!(
            shell,
            "
                cargo {cargo_cmd_str}
                    --profile {profile_str}
                    --target riscv64gc-unknown-none-elf
                    -Z unstable-options
            "
        )
        .run()?;
    }

    let bootloader_file_str = "bootloader.elf";
    let bootloader_file_path_str = format!(".hdd/{}", bootloader_file_str);

    // Copy binary to root hdd
    shell.copy_file(
        PathBuf::from(
            format!(
                "spark/target/riscv64gc-unknown-none-elf/{}/spark",
                // determine correct build optimization
                if options.release { "release" } else { "debug" }
            )
            .to_lowercase(),
        ),
        PathBuf::from(bootloader_file_path_str),
    )?;

    Ok(())
}
