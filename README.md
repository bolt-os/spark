# Spark

Spark is a simple, 64-bit bootloader for RISC-V.

# Building

To build spark just use ``cargo xtask build``

# Running

To run it in a virtual machine make sure you have qemu installed
Just use ``cargo xtask run``
If you encounter an error with qemu try running ``cargo xtask run -- -cpu rv64``

# Using

Using a kernel with spark isn't too complex, but can be a bit funky at first
I suggest you use Xtask to set it up

First just set up a basic xtask directory, and clone the spark repository into it.
Then set up your kernel in another directory.
For actually loading the kernel you need to go into the xtask code for running(which runs when you use `cargo xtask run`) and have qemu load spark as the kernel. Then have an NVME drive set up.

Within xtask's running code, you need to load the spark binary as a kernel, specifically the binary located at `spark/.hdd/spark-riscv-sbi-{foo}.bin`, where `{foo}` is release or debug.
But before that you need to set up a spark.cfg file for your kernel, here is an example one:
```
boot "example" {
    protocol = "bootelf";
    kernel-path = "boot:///boot/example";
}
```
Then make 2 new directores, first `drive` then `drive/boot` and copy the binary, as well as the spark.cfg file into `drive/boot`
Finally in the xtask code for running you need to add `-device nvme,serial=deadbeff,drive=disk1 -drive id=disk1,format=raw,if=none,file=fat:rw:drive/boot`
Note: At the end where it says `drive/boot` can be changed depending on where you put `drive/boot` or if you named the files differently
