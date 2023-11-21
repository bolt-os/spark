// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use crate::{dev::block::Volume, io, size_of};
use alloc::sync::Arc;
use libsa::extern_sym;

mod fat;

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
    fn volume(&self) -> &Arc<Volume>;

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
