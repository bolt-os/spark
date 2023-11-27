/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

#![no_std]

#[cfg(feature = "xtask")]
mod generate;
#[cfg(feature = "xtask")]
pub use generate::*;

use core::{mem::size_of, ptr};
use libsa::endian::{u32_le, u64_le};

pub const MAGIC: [u8; 8] = *b"SPARKSYM";

#[repr(C, align(8))]
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "bytemuck", derive(bytemuck::Pod, bytemuck::Zeroable))]
pub struct SymbolMapHeader {
    pub magic: [u8; 8],
    reserved0: u32,
    pub total_size: u32_le,
    pub table_offset: u32_le,
    pub table_len: u32_le,
    pub string_offset: u32_le,
    pub string_len: u32_le,
}

impl SymbolMapHeader {
    pub fn total_size(&self) -> usize {
        self.total_size.get() as usize
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
#[cfg_attr(feature = "bytemuck", derive(bytemuck::Pod, bytemuck::Zeroable))]
pub struct RawSymbol {
    pub addr: u64_le,
    pub size: u64_le,
    pub name: u32_le,
    pub name_len: u32_le,
}

pub struct SymbolMap<'sym> {
    symbols: &'sym [RawSymbol],
    strings: &'sym str,
}

impl<'sym> SymbolMap<'sym> {
    pub fn new(bytes: &'sym [u8]) -> Result<SymbolMap<'sym>, &'static str> {
        if bytes.as_ptr().align_offset(8) != 0 {
            return Err("unaligned");
        }
        if bytes.len() < size_of::<SymbolMapHeader>() {
            return Err("buffer too small");
        }

        let header = unsafe { &*bytes.as_ptr().cast::<SymbolMapHeader>() };

        if header.magic != MAGIC {
            return Err("invalid magic");
        }
        if bytes.len() < header.total_size() {
            return Err("buffer too small");
        }

        let offset = header.table_offset.get() as usize;
        let len = header.table_len.get() as usize;
        let size = len * size_of::<RawSymbol>();
        let symbols = bytes[offset..][..size].as_ptr().cast::<RawSymbol>();
        let symbols = unsafe { &*ptr::slice_from_raw_parts(symbols, len) };

        let offset = header.string_offset.get() as usize;
        let len = header.string_len.get() as usize;
        let Ok(strings) = core::str::from_utf8(&bytes[offset..][..len]) else {
            return Err("non UTF-8 string table");
        };

        Ok(SymbolMap { symbols, strings })
    }

    pub fn symbols(&self) -> impl Iterator<Item = Symbol<'sym>> {
        self.symbols
            .iter()
            .map(|raw| Symbol::from_raw(self.strings, raw))
    }

    pub fn lookup(&self, addr: u64) -> Option<Symbol<'sym>> {
        let index = match self
            .symbols
            .binary_search_by_key(&addr, |raw| raw.addr.get())
        {
            Ok(index) => index,
            Err(index) => index.saturating_sub(1),
        };
        let raw = &self.symbols[index];
        if raw.addr <= addr && addr < raw.addr + raw.size {
            Some(Symbol::from_raw(self.strings, raw))
        } else {
            None
        }
    }
}

pub struct Symbol<'sym> {
    pub name: &'sym str,
    pub addr: u64,
    pub size: u64,
}

impl<'sym> Symbol<'sym> {
    fn from_raw(strings: &'sym str, raw: &'sym RawSymbol) -> Symbol<'sym> {
        let name = &strings[raw.name.get() as usize..][..raw.name_len.get() as usize];
        Symbol {
            name,
            addr: raw.addr.get(),
            size: raw.size.get(),
        }
    }
}
