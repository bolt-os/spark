/*
 * Copyright (c) 2022 xvanc and contributors
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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

.macro _SAVE_GP_REG reg
        sd      x\reg, (8*\reg)(sp)
.endm

.macro _RESTORE_GP_REG reg
        ld      x\reg, (8*\reg)(sp)
.endm

.macro SAVE_GP_REGS
        addi    sp, sp, -(8*32)
.set reg, 0
.rept 32
        _SAVE_GP_REG    %reg
.set reg, reg + 1
.endr
.endm

.macro RESTORE_GP_REGS
.set reg, 0
.rept 32
        _RESTORE_GP_REG %reg
.set reg, reg + 1
.endr
        addi    sp, sp, 8*32
.endm

.section .text.trap_entry,"ax",@progbits
.p2align 4
.global trap_entry
trap_entry:
        SAVE_GP_REGS

        mv      s0, sp
        andi    sp, sp, ~0xf
        call    trap_handler
        mv      sp, s0

        csrr    t0, sepc
        addi    t0, t0, 4
        csrw    sepc, t0

        RESTORE_GP_REGS
        sret

.popsection
