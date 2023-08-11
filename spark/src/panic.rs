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

mod uw {
    use core::{ffi::c_void, ptr::addr_of_mut};

    use unwinding::abi::{UnwindContext, UnwindReasonCode, _Unwind_Backtrace};

    struct TraceData<'a, T> {
        data: &'a mut T,
        f: &'a mut dyn FnMut(&UnwindContext<'_>, &mut T) -> UnwindReasonCode,
    }

    extern "C" fn backtrace_callback<T>(
        ctx: &UnwindContext<'_>,
        data: *mut c_void,
    ) -> UnwindReasonCode {
        let data = unsafe { &mut *data.cast::<TraceData<T>>() };
        (data.f)(ctx, data.data)
    }

    pub fn backtrace<F, T>(data: &mut T, mut f: F) -> UnwindReasonCode
    where
        F: FnMut(&UnwindContext<'_>, &mut T) -> UnwindReasonCode,
    {
        let mut data = TraceData { data, f: &mut f };
        let data = addr_of_mut!(data).cast::<c_void>();

        _Unwind_Backtrace(backtrace_callback::<T>, data)
    }
}

use core::sync::atomic::{AtomicBool, Ordering};

use crate::hcf;
use spin::RwLock;

static SPARK_ELF: RwLock<Option<Vec<u8>>> = RwLock::new(None);
pub unsafe fn register_executable(elf: Vec<u8>) {
    *SPARK_ELF.write() = Some(elf);
}

#[inline]
fn reloc_offset() -> usize {
    let offset;
    unsafe {
        asm!("lla {}, __image_base", out(reg) offset, options(nomem, nostack, preserves_flags));
    }
    offset
}

#[inline(never)]
fn trace_stack() {
    use unwinding::abi::*;

    println!("----- STACK TRACE -----");

    let elf_guard = SPARK_ELF.read();
    let elf = elf_guard.as_ref().and_then(|data| elf::Elf::new(data).ok());
    let mut count = 0usize;
    uw::backtrace(&mut count, move |ctx, count| {
        let ip = _Unwind_GetIP(ctx);
        let orig_ip = ip - reloc_offset();

        print!("{count:4}: {ip:#018x} ({orig_ip:#018x}) -  ");

        if let Some(ref elf) = elf
            && let Some(symtab) = elf.symbol_table()
            && let Some(sym) = symtab.find(|sym| sym.contains_addr((ip - reloc_offset()) as _))
        {
            let name = rustc_demangle::demangle(sym.name().unwrap_or("<unknown>"));
            let offset = orig_ip - sym.value() as usize;
            println!("{name} + {offset:#x}");
        } else {
            println!("<unknown>");
        }

        *count += 1;
        UnwindReasonCode::NO_REASON
    });

    println!("-----------------------");
}

#[panic_handler]
fn rust_panic(info: &core::panic::PanicInfo) -> ! {
    println!("bootloader panic!\n{info}");

    static IN_PANIC: AtomicBool = AtomicBool::new(false);

    if IN_PANIC.swap(true, Ordering::SeqCst) {
        hcf();
    }

    trace_stack();

    hcf();
}
