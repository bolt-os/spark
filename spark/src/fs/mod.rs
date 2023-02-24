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

use crate::{dev::block::Volume, io, size_of};
use alloc::sync::Arc;
use libsa::extern_sym;

/// A filesystem driver compiled into the bootloader
#[repr(C)]
pub struct FilesystemDriver {
    name: &'static str,

    /// "Mount" an instance of this filesystem onto the provided [`Volume`]
    ///
    /// If successful, this function returns a [`File`] object for the root directory
    /// of the filesystem.
    mount: fn(volume: &Arc<Volume>) -> io::Result<Box<dyn File>>,
}

pub trait File: Send {
    fn open(&mut self, path: &str) -> io::Result<Box<dyn File>>;
    fn size(&mut self) -> u64;
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize>;
    fn position(&mut self) -> u64;

    fn read_to_end(&mut self) -> io::Result<Vec<u8>> {
        let size = (self.size() - self.position()) as usize;
        let mut buf = vec![0; size];
        let mut read = 0;
        while read < size {
            read += self.read(&mut buf[read..])?;
        }
        Ok(buf)
    }
}

fn filesystem_drivers() -> &'static [FilesystemDriver] {
    let data = extern_sym!(__start_fs_drivers as FilesystemDriver);
    let len = (extern_sym!(__stop_fs_drivers).addr() - data.addr()) / size_of!(FilesystemDriver);
    unsafe { core::slice::from_raw_parts(data, len) }
}

pub fn mount(volume: &Arc<Volume>) -> io::Result<Box<dyn File>> {
    for driver in filesystem_drivers() {
        match (driver.mount)(volume) {
            Ok(file) => return Ok(file),
            Err(io::Error::Unsupported) => continue,
            Err(err) => return Err(err),
        }
    }
    Err(io::Error::NotFound)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FileType {
    Regular,
    Directory,
}
