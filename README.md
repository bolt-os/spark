# Spark

Spark is a simple, 64-bit bootloader for RISC-V.

## Building

`spark` uses the `xtask` framework as its build system to gain more control over the process
than is possible with `cargo` alone. The `xtask` is aliased to `cargo xtask` for convenience.

To build `spark`, simply run `cargo xtask build [--release]` in the root of this repository.
A `.hdd` directory will be created containing the build artifacts.

## Using the Bootloader

Currently `spark` is limited to reading from NVMe drives. The only protocol currently usable
is the [`bootelf`](https://bolt-os.github.io/spark-docs/spark/proto/bootelf/index.html) protocol.

The easiest way to get up and running is using QEMU's `vvfat` driver to emulate a FAT filesystem
from a directory on your host:

```
-device nvme,serial=deadbeff,drive=disk1
-drive id=disk1,format=raw,if=none,file=fat:rw:path/to/directory
```

Alternatively, you can create a disk image to use as the backing for the drive:

```
-device nvme,serial=deadbeff,drive=disk1
-drive id=disk1,format=raw,if=none,file=path/to/disk.img
```

### Example

`spark.cfg`:
```
boot "my awesome kernel" {
    protocol = "bootelf";
    kernel-path = "boot:///boot/kernel.elf";
}
```

```
$ mkdir -p ./root/boot
$ cp path/to/kernel.elf ./root/boot/kernel.elf
$ cp path/to/spark.cfg  ./root/boot/spark.cfg
$ qemu-system-riscv64 -machine virt -cpu rv64 \
    -kernel spark-riscv-sbi-release.bin \
    -device nvme,serial=deadbeff,drive=disk1 \
    -drive id=disk1,format=raw,if=none,file=./root
```

# Running

To run it in a virtual machine make sure you have qemu installed
Just use ``cargo xtask run``
If you encounter an error with qemu try running ``cargo xtask run -- -cpu rv64``
