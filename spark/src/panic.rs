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

use crate::hcf;
use core::sync::atomic::{AtomicBool, Ordering};
use symbol_map::Symbol;

#[inline]
fn reloc_offset() -> usize {
    let offset;
    unsafe {
        asm!("lla {}, __image_base", out(reg) offset, options(nomem, nostack, preserves_flags));
    }
    offset
}

mod symbol_map {
    use core::ptr;
    use libsa::extern_sym;

    include!("../raw_symbol_map.rs");

    impl SymbolRaw {
        pub const fn contains_addr(&self, addr: u64) -> bool {
            self.addr <= addr && addr < self.addr + self.size
        }
    }

    pub struct Symbol {
        pub name: &'static str,
        pub addr: u64,
        pub size: u64,
    }

    pub struct SymbolMap {
        symbols: &'static [SymbolRaw],
        names: &'static str,
    }

    impl SymbolMap {
        pub const fn empty() -> SymbolMap {
            Self {
                symbols: &[],
                names: "",
            }
        }

        pub fn lookup(&self, addr: u64) -> Option<Symbol> {
            self.symbols
                .iter()
                .find(|sym| sym.contains_addr(addr))
                .map(|sym| Symbol {
                    name: &self.names[sym.name as usize..][..sym.name_len as usize],
                    addr: sym.addr,
                    size: sym.size,
                })
        }
    }

    // Provide a fallback definition of the symbol map so the first link (without the generated
    // symbol map) succeeds. We use a `u64` so it will have the proper alignment. A valid
    // signature in the first 4 bytes is checked before creating a `SymbolMap`, so we don't need
    // to worry about providing a full `SymbolMapHeader`.
    #[export_name = "__symbol_map"]
    #[linkage = "weak"]
    static _DUMMY_SYMBOL_MAP: u64 = 0;

    pub fn get_symbol_map() -> SymbolMap {
        unsafe {
            let ptr = extern_sym!(__symbol_map as u8);
            if ptr.align_offset(8) != 0 {
                log::error!("unaligned symbol map: {ptr:p}");
                return SymbolMap::empty();
            }
            let magic = ptr.cast::<[u8; 4]>().read();
            if magic != MAGIC {
                log::error!("invalid symbol map: {magic:02x?}");
                return SymbolMap::empty();
            }
            // SAFETY: At this point we can assume the symbol map was properly linked
            // and it is safe to construct the symbol map without any further checks.
            let header = &*ptr.cast::<SymbolMapHeader>();
            let symbols = &*ptr::from_raw_parts::<[SymbolRaw]>(
                ptr.add(header.symbols_offset as usize).cast(),
                header.symbols_len as usize,
            );
            let names = &*ptr::from_raw_parts::<str>(
                ptr.add(header.strings_offset as usize).cast(),
                header.strings_len as usize,
            );
            SymbolMap { symbols, names }
        }
    }
}

#[inline(never)]
fn trace_stack() {
    use unwinding::abi::*;

    println!("----- STACK TRACE -----");

    let symbol_map = symbol_map::get_symbol_map();

    let mut count = 0usize;
    uw::backtrace(&mut count, move |ctx, count| {
        let ip = _Unwind_GetIP(ctx);
        let orig_ip = ip - reloc_offset();

        print!("{count:4}: {ip:#018x} ({orig_ip:#018x}) -  ");

        if let Some(Symbol { name, addr, .. }) = symbol_map.lookup(orig_ip as u64) {
            let offset = orig_ip - addr as usize;
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
