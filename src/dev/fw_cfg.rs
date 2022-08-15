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

use self::mmio::{DmaPacket, MmioWindow};
use core::{cell::OnceCell, fmt, ptr::addr_of};
use libsa::{
    endian::{BigEndianU16, BigEndianU32, BigEndianU64},
    volatile::Volatile,
};

#[repr(C)]
#[derive(Clone)]
pub struct File {
    size: BigEndianU32,
    sel: BigEndianU16,
    rsvd: u16,
    name: [u8; 56],
}

impl File {
    pub const fn size(&self) -> usize {
        self.size.get() as _
    }

    pub fn name(&self) -> Option<&str> {
        let mut len = 0;
        while len < self.name.len() {
            if self.name[len] == 0 {
                break;
            }
            len += 1;
        }
        core::str::from_utf8(&self.name[..len]).ok()
    }
}

impl fmt::Debug for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut dbg = f.debug_struct("File");

        dbg.field("size", &self.size());
        dbg.field("sel", &self.sel);

        if let Some(valid_str) = self.name() {
            dbg.field("name", &valid_str);
        } else {
            dbg.field("name", &self.name);
        }

        dbg.finish()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    BadPointer,
    DmaError,
}

pub struct FwCfg {
    window: MmioWindow,
    files: OnceCell<Vec<File>>,
}

impl FwCfg {
    pub fn new(ptr: *mut u8) -> Result<FwCfg, Error> {
        Ok(FwCfg {
            window: MmioWindow::new(ptr)?,
            files: OnceCell::new(),
        })
    }

    /// Returns a list of all available files
    pub fn files(&self) -> &[File] {
        self.files.get_or_init(|| unsafe {
            self.window.write_ctrl(0x19.into());
            (0..self.window.read::<BigEndianU32>().get())
                .map(|_| self.window.read())
                .collect()
        })
    }

    /// Search for a file at the given `path`
    pub fn lookup(&self, path: &str) -> Option<&File> {
        self.files().iter().find(|f| f.name() == Some(path))
    }

    /// Read the contents of a file into a buffer
    pub fn read_file(&self, file: &File) -> Result<Vec<u8>, Error> {
        let mut buf = Box::<[u8]>::new_uninit_slice(file.size());

        unsafe {
            self.dma_command(
                Some(file.sel),
                DmaCommand::SELECT | DmaCommand::READ,
                file.size.get(),
                buf.as_mut_ptr().addr() as _,
            )?;

            Ok(buf.assume_init().into())
        }
    }

    unsafe fn dma_command(
        &self,
        sel: Option<BigEndianU16>,
        cmd: DmaCommand,
        length: u32,
        address: u64,
    ) -> Result<(), Error> {
        let control = ((sel.unwrap_or_default().get() as u32) << 16) | cmd.bits();
        let packet = DmaPacket {
            control: Volatile::new(BigEndianU32::new(control)),
            length: BigEndianU32::new(length),
            address: BigEndianU64::new(address),
        };

        // Issue the command by writing the address of the DmaPacket to
        // the DMA Control Register.
        self.window
            .write_dma_ctrl(BigEndianU64::new(addr_of!(packet).addr() as _));

        // Wait for completion or error
        //
        // Currently QEMU completes all commands immediately, so we likely won't wait
        // at all, but fw_cfg may become asynchronous in the future making this necessary.
        loop {
            let ctrl = packet.control.read().get();

            if ctrl & 0x1 != 0 {
                return Err(Error::DmaError);
            }
            if ctrl == 0 {
                break;
            }
        }

        Ok(())
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    struct DmaCommand : u32 {
        const ERROR     = 1 << 0;
        const READ      = 1 << 1;
        const SKIP      = 1 << 2;
        const SELECT    = 1 << 3;
        const WRITE     = 1 << 4;
    }
}

mod mmio {
    use super::Error;
    use core::{cell::UnsafeCell, mem::MaybeUninit, ptr::NonNull};
    use libsa::{
        endian::{BigEndianU16, BigEndianU32, BigEndianU64},
        volatile::Volatile,
    };

    #[repr(C)]
    struct MmioWindowInner {
        data: UnsafeCell<u64>,
        control: Volatile<BigEndianU16>,
        dma_control: Volatile<BigEndianU64>,
    }

    pub struct MmioWindow {
        inner: NonNull<MmioWindowInner>,
    }

    impl MmioWindow {
        pub fn new(window_ptr: *mut u8) -> Result<MmioWindow, Error> {
            let window_ptr = window_ptr.cast::<MmioWindowInner>();

            if !window_ptr.is_aligned() {
                return Err(Error::BadPointer);
            }

            Ok(MmioWindow {
                inner: NonNull::new(window_ptr).ok_or(Error::BadPointer)?,
            })
        }

        fn inner(&self) -> &MmioWindowInner {
            unsafe { self.inner.as_ref() }
        }

        pub unsafe fn read_u8(&self) -> u8 {
            self.inner().data.get().cast::<u8>().read_volatile()
        }

        pub unsafe fn read_bytes_raw(&self, dst: *mut u8, size: usize) {
            let mut written = 0;
            while written < size {
                dst.add(written).write(self.read_u8());
                written += 1;
            }
        }

        pub unsafe fn read<T>(&self) -> T {
            let mut uninit = MaybeUninit::<T>::uninit();
            self.read_bytes_raw(uninit.as_mut_ptr().cast(), core::mem::size_of::<T>());
            uninit.assume_init()
        }

        pub unsafe fn write_ctrl(&self, value: BigEndianU16) {
            self.inner().control.write(value);
        }

        pub unsafe fn write_dma_ctrl(&self, value: BigEndianU64) {
            self.inner().dma_control.write(value);
        }
    }

    #[repr(C)]
    pub struct DmaPacket {
        pub control: Volatile<BigEndianU32>,
        pub length: BigEndianU32,
        pub address: BigEndianU64,
    }
}
