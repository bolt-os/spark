# Spark

`spark` is a 64-bit bootloader for RISC-V, implementing the [Limine Boot Protocol](https://github.com/limine-bootloader/limine/blob/trunk/PROTOCOL.md)
for both SBI and UEFI systems.

## Building

`spark` uses a custom build tool called `xtask` which wraps `cargo` and controls the entire
build process. To build the bootloader, simply run `cargo xtask build [--release]`.
The final build artifacts will be output into the `build/` directory.

For a full list of the available build options, run `cargo xtask build --help`.

## Using the Bootloader

Currently `spark` is limited to reading from NVMe drives.

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
    protocol = "limine";
    kernel-path = "boot:///boot/kernel.elf";
}
```

```
$ mkdir -p ./root
$ cp path/to/kernel.elf ./root/
$ cp path/to/spark.cfg  ./root/
$ qemu-system-riscv64 -machine virt -cpu rv64 \
    -kernel spark.bin \
    -device nvme,serial=deadbeff,drive=disk1 \
    -drive id=disk1,format=raw,if=none,file=./root
```

# Running

To run it in a virtual machine make sure you have qemu installed
Just use ``cargo xtask run``
If you encounter an error with qemu try running ``cargo xtask run -- -cpu rv64``
