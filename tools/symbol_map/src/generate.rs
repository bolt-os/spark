/*
 * Copyright (c) 2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

extern crate alloc;
extern crate std;

use crate::{RawSymbol, SymbolMapHeader, MAGIC};
use alloc::{boxed::Box, format, string::String, vec};
use core::{fmt, mem::size_of};
use elf::{Elf, SymbolKind};
use libsa::endian::u32_le;
use std::{fs, io, path::Path};

#[derive(Debug)]
pub enum Error {
    /// Invalid ELF file
    InvalidElf(&'static str),
    /// The executable does not contain a symbol table
    NoSymbolTable,
    /// An individual symbol name is too large
    SymbolTooBig,
    /// The generated symbol map would be too large
    MapTooBig,

    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Error {
        Error::Io(error)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidElf(error) => write!(f, "invalid ELF: {error}"),
            Error::NoSymbolTable => write!(f, "no symbol table"),
            Error::SymbolTooBig => write!(f, "symbol name too large"),
            Error::MapTooBig => write!(f, "generated symbol map would be too large"),
            Error::Io(error) => write!(f, "io error: {error}"),
        }
    }
}

impl std::error::Error for Error {}

#[inline(always)]
pub fn generate<P: AsRef<Path>>(path: P) -> Result<Box<[u8]>, Error> {
    generate_(path.as_ref())
}

fn generate_(path: &Path) -> Result<Box<[u8]>, Error> {
    let file_data = fs::read(path)?;
    let elf = Elf::new(&file_data).map_err(Error::InvalidElf)?;
    let symbol_table = elf.symbol_table().ok_or(Error::NoSymbolTable)?;
    let string_table = elf.string_table().ok_or(Error::NoSymbolTable)?;

    let mut table = vec![];
    let mut string = String::new();

    for symbol in symbol_table
        .symbols()
        .filter(|s| s.kind() == SymbolKind::Func)
    {
        if let Some(name) = string_table.get_string(symbol.name_index()) {
            let name = format!("{:#}", rustc_demangle::demangle(name));
            let name_offset = string.len();
            string.push_str(&name);
            table.push(RawSymbol {
                addr: symbol.value().into(),
                size: symbol.size().into(),
                name: u32::try_from(name_offset)
                    .map_err(|_| Error::MapTooBig)?
                    .into(),
                name_len: u32::try_from(name.len())
                    .map_err(|_| Error::SymbolTooBig)?
                    .into(),
            })
        }
    }

    table.sort_unstable_by_key(|s| s.addr);

    let hdr_offset = 0;
    let hdr_size = size_of::<SymbolMapHeader>();
    let tab_offset = hdr_offset + hdr_size;
    let tab_size = table.len() * size_of::<RawSymbol>();
    let str_offset = tab_offset + tab_size;
    let str_size = string.len();
    let total_size = str_offset + str_size;

    if total_size > 0x100000000 {
        return Err(Error::MapTooBig);
    }

    let header = SymbolMapHeader {
        magic: MAGIC,
        total_size: u32_le::new(total_size as u32),
        reserved0: 0,
        table_offset: u32_le::new(tab_offset as u32),
        table_len: u32_le::new(table.len() as u32),
        string_offset: u32_le::new(str_offset as u32),
        string_len: u32_le::new(string.len() as u32),
    };

    let mut buf = vec![0u8; total_size].into_boxed_slice();

    let header_buf = bytemuck::from_bytes_mut(&mut buf[..hdr_size]);
    *header_buf = header;

    let table_buf = bytemuck::cast_slice_mut(&mut buf[tab_offset..][..tab_size]);
    table_buf.copy_from_slice(&table);

    buf[str_offset..].copy_from_slice(string.as_bytes());

    Ok(buf)
}
