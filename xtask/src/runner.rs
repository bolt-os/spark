use crate::{BuildCtx, SparkBuildOptions, Target};
use clap::{clap_derive::ArgEnum, Parser};
use std::{ffi::OsString, path::PathBuf, process::Command};
use xtask::process::CommandExt;

#[derive(ArgEnum, Clone, Copy)]
pub enum BlockDriver {
    None,
    Ahci,
    Nvme,
    Virtio,
    VirtioPci,
}

impl BlockDriver {
    const fn is_virtio(self) -> bool {
        matches!(self, Self::Virtio | Self::VirtioPci)
    }
}

impl core::fmt::Debug for BlockDriver {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            BlockDriver::None => "none",
            BlockDriver::Ahci => "ahci",
            BlockDriver::Nvme => "nvme",
            BlockDriver::Virtio => "virtio-blk-device",
            BlockDriver::VirtioPci => "virtio-blk-pci",
        })
    }
}

#[derive(Parser)]
pub struct Options {
    #[clap(flatten)]
    general_options: SparkBuildOptions,

    #[clap(long)]
    kernel: Option<PathBuf>,

    /// Machine to emulate.
    #[clap(long, default_value = "virt,aclint=on")]
    machine: String,

    /// Number of CPUs to emulate.
    #[clap(long, default_value = "4")]
    smp: usize,

    // RAM size in MB.
    #[clap(long, default_value = "512")]
    ram: usize,

    /// Enables debug logging to the specified location.
    #[clap(long)]
    log: bool,

    #[clap(short, long)]
    debug: bool,

    /// Stops QEMU from automatically exiting when a triple fault occurs.
    #[clap(long)]
    no_shutdown: bool,

    /// Which type of block driver to use for root drive.
    #[clap(arg_enum, long, default_value = "virtio")]
    block: BlockDriver,

    #[clap(long, default_value = "qemu-system-riscv64")]
    qemu: PathBuf,

    #[clap(last = true)]
    emu_args: Vec<OsString>,
}

impl core::ops::Deref for Options {
    type Target = SparkBuildOptions;

    fn deref(&self) -> &Self::Target {
        &self.general_options
    }
}

pub fn run(ctx: &BuildCtx, options: Options) -> anyhow::Result<()> {
    if options.target != Target::riscv_sbi {
        todo!("uefi runner");
    }

    /*
     * Make sure we run an up-to-date version of the bootloader.
     */
    crate::build::build(
        ctx,
        crate::build::Options {
            general_options: options.general_options.clone(),
        },
        crate::build::BuildCmd::Build,
    )?;

    let spark_elf = PathBuf::from(format!(
        ".hdd/spark-{}-{}.elf",
        options.target,
        if options.release { "release" } else { "debug" }
    ));
    let spark_bin = spark_elf.with_extension("bin");

    let mut qemu = Command::new(&options.qemu);

    #[rustfmt::skip]
    qemu.args([
        "-machine", &options.machine,
        "-cpu", "rv64,svpbmt=true",
        "-m", &options.ram.to_string(),
        "-smp", &options.smp.to_string(),
        "-serial", "mon:stdio",
    ]);

    if options.debug {
        qemu.args(["-s", "-S"]);
    }

    if options.log {
        qemu.args(["-d", "int,guest_errors", "-D", ".debug/qemu-log.txt"]);
    }

    match options.block {
        BlockDriver::None => {}
        BlockDriver::Ahci => {
            qemu.args([
                "-device",
                "ahci,id=ahci",
                "-device",
                "ide-hd,drive=disk0,bus=ahci.0",
            ]);
        }
        BlockDriver::Nvme => {
            qemu.args(["-device", "nvme,serial=deadbeef,drive=disk0"]);
        }
        BlockDriver::Virtio => {
            qemu.args(["-device", "virtio-blk-device,serial=deadbeef,drive=disk0"]);
        }
        BlockDriver::VirtioPci => {
            qemu.args(["-device", "virtio-blk-pci,serial=deadbeef,drive=disk0"]);
        }
    }

    if options.block.is_virtio() {
        qemu.args(["-global", "virtio-mmio.force-legacy=false"]);
    }

    qemu.args(["-drive", "id=disk0,format=raw,if=none,file=.hdd/disk0.img"]);

    qemu.arg("-kernel");
    qemu.arg(&spark_bin);

    if let Some(kernel_path) = options.kernel {
        let arg = format!("opt/org.spark/kernel,file={}", kernel_path.display());
        qemu.args(["-fw_cfg", &arg]);
    }

    qemu.args(options.emu_args).execute()?;

    Ok(())
}
