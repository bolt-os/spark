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
