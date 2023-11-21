// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

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
    use anyhow::anyhow;
    use core::slice;
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
    global_asm!(
        r#"
        .pushsection .rodata.__dummy_symbol_map,"a",@progbits
        .weak __symbol_map
        .weak __symbol_map_size
        .p2align 3
        __symbol_map:
        .4byte 0
        .popsection
        "#
    );

    pub fn get_symbol_map() -> anyhow::Result<SymbolMap> {
        unsafe {
            let ptr = extern_sym!(__symbol_map as u8);
            let len = extern_sym!(__symbol_map_size).addr();

            if ptr.align_offset(8) != 0 {
                return Err(anyhow!("unaligned symbol map: {ptr:p}"));
            }

            let magic = ptr.cast::<[u8; 4]>().read();
            if magic != MAGIC {
                return Err(anyhow!("invalid symbol map: {magic:02x?}"));
            }

            let header = ptr.cast::<SymbolMapHeader>().read();

            if header.strings_offset as usize + header.strings_len as usize > len {
                return Err(anyhow!("invalid size: {len}"));
            }

            let symbols = slice::from_raw_parts(
                ptr.add(header.symbols_offset as usize).cast(),
                header.symbols_len as usize,
            );
            let names = core::str::from_utf8_unchecked(slice::from_raw_parts(
                ptr.add(header.strings_offset as usize),
                header.strings_len as usize,
            ));

            Ok(SymbolMap { symbols, names })
        }
    }
}

#[inline(never)]
pub fn trace_stack() -> anyhow::Result<()> {
    use unwinding::abi::*;

    println!("----- STACK TRACE -----");

    let symbol_map = symbol_map::get_symbol_map()?;

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

    Ok(())
}

#[panic_handler]
fn rust_panic(info: &core::panic::PanicInfo) -> ! {
    println!("bootloader panic!\n{info}");

    static IN_PANIC: AtomicBool = AtomicBool::new(false);

    if IN_PANIC.swap(true, Ordering::SeqCst) {
        hcf();
    }

    if let Err(error) = trace_stack() {
        println!("failed to get stack trace: {error}");
    }

    hcf();
}
