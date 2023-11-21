// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

.pushsection .text

.section .text._start,"ax",@progbits
.global _start
.type _start,@function
_start:
        /*
         * Load the GP register with the global pointer (provided by the linker script).
         * Linker relaxations must be disabled as they rely on GP being set.
         */
.option push
.option norelax
        lla             gp, __global_pointer$
.option pop

        lla             sp, __boot_stackp

        /*
         * Set up the trap handler.. which may or may not work before relocations
         * have been performed.
         */
        lla             t0, trap_entry
        csrw            stvec, t0

        /*
         * Initialize the FPU.
         */
        li              t0, ~(3 << 13)  // clear sstatus.FS
        csrc            sstatus, t0
        li              t0, 1 << 13     // sstatus.FS = initial
        csrs            sstatus, t0
        li              t0, 1 << 5      // fcsr.frm = rtz
        csrw            fcsr, t0

        /*
         * Clear the .bss segment.
         *
         * The linker script ensures __bss is properly aligned, and the size is at least
         * 8 bytes.
         */
        lla             t0, __bss
        lla             t1, __ebss
0:      sd              zero, (t0)
        addi            t0, t0, 8
        bltu            t0, t1, 0b

        /*
         * We may or may not have been loaded to our linked address.
         */
        mv              s0, a0
        mv              s1, a1
        lla             a0, __image_base
        lla             a1, _DYNAMIC
        call            _relocate
        bnez            a0, error
        mv              a0, s0
        mv              a1, s1

        mv              fp, zero
        call            spark_main

error:
        csrci           sstatus, 0x2
        wfi
        j               .
.size _start, . - _start


.altmacro

.macro STORE_GP_REG reg
    .if reg != 2
        sd      x\reg, (8 * \reg)(sp)
    .endif
.endm

.macro LOAD_GP_REG reg
    .if reg != 2
        ld      x\reg, (8 * \reg)(sp)
    .endif
.endm

.macro STORE_FP_REG reg
        fsd     f\reg, (8 * (32 + \reg))(sp)
.endm

.macro LOAD_FP_REG reg
        fld     f\reg, (8 * (32 + \reg))(sp)
.endm

.macro STORE_REGS
.set reg, 0
.rept 32
        STORE_GP_REG    %reg
        STORE_FP_REG    %reg
.set reg, reg + 1
.endr
.endm

.macro LOAD_REGS
.set reg, 0
.rept 32
        LOAD_GP_REG    %reg
        LOAD_FP_REG    %reg
.set reg, reg + 1
.endr
.endm


// Trap Entry Point
//
// Due to restrictions of the `stvec` CSR, this entry point must be aligned on a 4-byte boundary.
.section .text.trap_entry,"ax",@progbits
.global trap_entry
.align 4
trap_entry:
        // TODO: What if the stack is fucked?

        addi    sp, sp, -(8 * 68)
        STORE_REGS

        csrr    t0, sstatus
        csrr    t1, scause
        csrr    t2, sepc
        csrr    t3, stval
        sd      t0, (8 * (64 + 0))(sp)
        sd      t1, (8 * (64 + 1))(sp)
        sd      t2, (8 * (64 + 2))(sp)
        sd      t3, (8 * (64 + 3))(sp)

        // Prepare to enter Rust.
        // Align the stack to 8 bytes, stash the previous stack pointer (and pointer to the trap
        // frame) in a saved register that won't be clobbered by the calling convention.
        mv      s1, sp
        andi    sp, sp, ~0xf

        // Make it look as if the trapped code "called" this function.
        // This stops stack traces from missing the trapped function.
        addi    sp, sp, -16
        sd      t2, 8(sp)
        sd      fp, 0(sp)
        addi    fp, sp, 16

        mv      a0, s1
        call    trap_handler

        // Restore the old stack pointer.
        mv      sp, s1

        // It's unlikely that we return, since we don't use interrupts all traps will end up being
        // (fatal) exceptions. In case we ever do, reload `sstatus` and `sepc` so the trap handler
        // can make any necessary changes.
        ld      t0, (8 * (64 + 0))(sp)
        ld      t1, (8 * (64 + 2))(sp)
        csrw    sstatus, t0
        csrw    sepc, t1

        // Reload all the registers we saved earlier and return to the trapped code.
        LOAD_REGS
        addi    sp, sp, (8 * 68)

        sret

.popsection
