use clap::{clap_derive::ArgEnum, Parser};
use xshell::cmd;

#[derive(ArgEnum, Clone, Copy)]
pub enum BlockDriver {
    AHCI,
    NVME,
    VirtIO,
}

impl core::fmt::Debug for BlockDriver {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            BlockDriver::AHCI => "ahci",
            BlockDriver::NVME => "nvme",
            BlockDriver::VirtIO => "virtio-blk-pci",
        })
    }
}

#[derive(Parser)]
pub struct Options {
    /// Number of CPUs to emulate.
    #[clap(long, default_value = "4")]
    smp: usize,

    // RAM size in MB.
    #[clap(long, default_value = "512")]
    ram: usize,

    /// Enables debug logging to the specified location.
    #[clap(long)]
    log: bool,

    /// Stops QEMU from automatically exiting when a triple fault occurs.
    #[clap(long)]
    no_shutdown: bool,

    /// Which type of block driver to use for root drive.
    #[clap(arg_enum, long, default_value = "virt-io")]
    block: BlockDriver,
}

pub fn run(options: Options) -> Result<(), xshell::Error> {
    let shell = xshell::Shell::new()?;

    let smp_str = format!("{}", options.smp);
    let ram_str = format!("{}", options.ram);
    let block_driver_str = format!("{:?}", options.block);
    let log_str = if options.log {
        vec!["-d", "int,guest_errors", "-D", ".debug/qemu.log"]
    } else {
        vec![]
    };
    let no_shutdown_str = if options.no_shutdown {
        vec!["-no-shutdown"]
    } else {
        vec![]
    };

    cmd!(
        shell,
        "
        qemu-system-riscv64
            -no-reboot
            -machine virt
            -cpu rv64
            -smp {smp_str}
            -m {ram_str}M
            -kernel .hdd/bootloader.elf
            -serial mon:stdio
            -net none
            -drive format=raw,file=.hdd/disk0.img,id=disk1,if=none
            -device {block_driver_str},drive=disk1,serial=deadbeef
            {log_str...}
            {no_shutdown_str...}
        "
    )
    .run()?;

    Ok(())
}
