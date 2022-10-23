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

struct TrapFrame {
    gpr: [usize; 32],
    #[cfg(target_feature = "d")]
    #[allow(dead_code)]
    fpr: [usize; 32],
    sstatus: u64,
    scause: u64,
    sepc: u64,
    stval: u64,
}

impl TrapFrame {
    fn is_exception(&self) -> bool {
        self.scause & 1 << 63 == 0
    }

    fn is_interrupt(&self) -> bool {
        !self.is_exception()
    }

    fn cause(&self) -> u64 {
        self.scause & !(1 << 63)
    }
}

fn dump_registers(tf: &TrapFrame) {
    static ABI_NAMES: &[&str] = &[
        "  ", "ra", "t0", "sp", "t1", "gp", "t2", "tp", "t3", "s0", "t4", "s1", "t5", "s2", "t6",
        "s3", "a0", "s4", "a1", "s5", "a2", "s6", "a3", "s7", "a4", "s8", "a5", "s9", "a6", "s10",
        "a7", "s11",
    ];
    static ISA_NUMS: &[u8] = &[
        0, 1, 5, 2, 6, 3, 7, 4, 28, 8, 29, 9, 30, 18, 31, 19, 10, 20, 11, 21, 12, 22, 13, 23, 14,
        24, 15, 25, 16, 26, 17, 27,
    ];

    for (isa_num, abi_name) in ISA_NUMS.chunks(2).zip(ABI_NAMES.chunks(2)) {
        println!(
            "x{: <2} {: <3}: {:#018x}    x{: <2} {: <3}: {:#018x}",
            isa_num[0],
            abi_name[0],
            tf.gpr[isa_num[0] as usize],
            isa_num[1],
            abi_name[1],
            tf.gpr[isa_num[1] as usize],
        );
    }
}

static EXCEPTION_NAMES: &[Option<&str>] = &[
    Some("instruction alignment fault"),
    Some("instruction access fault"),
    Some("illegal instruction"),
    Some("breakpoint"),
    Some("load address misaligned"),
    Some("load access fault"),
    Some("store/amo address misaligned"),
    Some("store/amo access fault"),
    Some("environment call from u-mode"),
    Some("environment call from s-mode"),
    None,
    None,
    Some("instruction page fault"),
    Some("load page fault"),
    None,
    Some("store/amo page fault"),
];

#[no_mangle]
fn trap_handler(tf: &mut TrapFrame) {
    if tf.is_interrupt() {
        log::error!("unhandled interrupt #{}", tf.cause());
        return;
    }

    if let Some(name) = EXCEPTION_NAMES.get(tf.cause() as usize).copied().flatten() {
        log::error!("{name}");
    } else {
        log::error!("unhandled exception ({})", tf.cause());
    }

    println!(
        "sstatus: {:#018x}, sepc: {:#018x}, stval: {:#018x}",
        tf.sstatus, tf.sepc, tf.stval
    );
    dump_registers(tf);

    panic!("fatal exception");
}
