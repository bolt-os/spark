// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

pub const MAGIC: [u8; 4] = *b"SPRK";

#[repr(C, align(8))]
#[derive(Debug)]
pub struct SymbolMapHeader {
    pub magic: [u8; 4],
    pub _reserved: u32,
    pub symbols_offset: u32,
    pub symbols_len: u32,
    pub strings_offset: u32,
    pub strings_len: u32,
}

#[repr(C)]
pub struct SymbolRaw {
    pub addr: u64,
    pub size: u64,
    pub name: u32,
    pub name_len: u32,
}
