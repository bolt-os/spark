/*
 * Copyright (c) 2023 xvanc and contributors
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

use crate::{fs::FileType, io, size_of};

pub const ENTRY_SIZE: usize = 32;

#[derive(Debug)]
pub struct DirEntry {
    pub file_type: FileType,
    pub cluster: u32,
    pub size: u64,
}

bitflags::bitflags! {
    #[repr(transparent)]
    pub struct DirAttrs : u8 {
        const READ_ONLY = 0x01;
        const HIDDEN    = 0x02;
        const SYSTEM    = 0x04;
        const VOLUME_ID = 0x08;
        const DIRECTORY = 0x10;
        const ARCHIVE   = 0x20;
        const LONG_NAME = 0x0f;
    }
}

impl From<DirAttrs> for FileType {
    fn from(attrs: DirAttrs) -> Self {
        if attrs.contains(DirAttrs::DIRECTORY) {
            FileType::Directory
        } else {
            FileType::Regular
        }
    }
}

impl DirAttrs {
    pub const fn new(bits: u8) -> DirAttrs {
        Self::from_bits_retain(bits)
    }
}

#[repr(C)]
pub struct ShortDirEntry {
    name: [u8; 8],
    extension: [u8; 3],
    attrs: DirAttrs,
    _reserved: u8,
    creation_time_s: u8,
    creation_time: u16,
    creation_date: [u8; 2],
    accessed_date: [u8; 2],
    cluster_hi: u16,
    modification_time: u16,
    modification_date: [u8; 2],
    cluster_lo: u16,
    pub size: u32,
}

impl ShortDirEntry {
    pub fn name(&self) -> String {
        let mut name = String::new();
        for c in self.name {
            if c == b' ' {
                break;
            }
            name.push(c as char);
        }
        if self.extension[0] != b' ' {
            name.push('.');
            for c in self.extension {
                if c == b' ' {
                    break;
                }
                name.push(c as char);
            }
        }
        name
    }

    pub const fn cluster(&self) -> u32 {
        ((self.cluster_hi as u32) << 16) | self.cluster_lo as u32
    }
}

#[repr(C)]
pub struct LongDirEntry {
    order: u8,
    name1: [u8; 10], // 1..=5
    attrs: u8,
    _reserved: u8,
    checksum: u8,
    name2: [u8; 12], // 6..=11
    cluster_lo: u16,
    name3: [u8; 4], // 12..=13
}

impl LongDirEntry {
    pub fn name_piece_raw(&self) -> [u16; 13] {
        let mut buf = [0u16; 13];

        unsafe {
            buf.as_mut_ptr()
                .cast::<u8>()
                .add(0)
                .copy_from(self.name1.as_ptr(), 10);
            buf.as_mut_ptr()
                .cast::<u8>()
                .add(10)
                .copy_from(self.name2.as_ptr(), 12);
            buf.as_mut_ptr()
                .cast::<u8>()
                .add(22)
                .copy_from(self.name3.as_ptr(), 2);
        }

        buf
    }
}

const _: () = {
    assert!(size_of!(ShortDirEntry) == ENTRY_SIZE);
    assert!(size_of!(LongDirEntry) == ENTRY_SIZE);
};

pub struct LongName {
    buf: [u16; 256],
    offset: usize,
}

impl LongName {
    pub fn new() -> LongName {
        Self {
            buf: [0; 256],
            offset: 256,
        }
    }

    fn len(&self) -> usize {
        256 - self.offset
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn as_slice(&self) -> &[u16] {
        if self.is_empty() {
            &[]
        } else {
            &self.buf[self.offset..]
        }
    }

    pub fn push(&mut self, piece: &[u16]) -> io::Result<()> {
        if piece.len() > self.offset {
            return Err(io::Error::NameTooLong);
        }
        self.offset -= piece.len();
        self.buf[self.offset..][..piece.len()].copy_from_slice(piece);
        Ok(())
    }

    fn clear(&mut self) {
        self.offset = 256;
    }

    fn to_string(&self) -> Option<String> {
        String::from_utf16(self.as_slice()).ok()
    }

    pub fn finish(&mut self) -> Option<String> {
        let string = self.to_string()?;
        self.clear();
        Some(string)
    }
}
