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

pub mod pmm;
pub mod vmm;

#[macro_export]
macro_rules! pages_for {
    ($size:expr) => {
        ($size as usize + $crate::vmm::PAGE_SIZE - 1) / $crate::vmm::PAGE_SIZE
    };
    ($size:expr, $page_size:expr) => {{
        let page_size = $page_size;
        ($size as usize + (page_size - 1)) / page_size
    }};
    (type $t:ty $(, $page_size:expr)?) => {
        pages_for!(::core::mem::size_of::<$t>() $(, $page_size)?)
    };
}

#[macro_export]
macro_rules! page_offset {
    ($x:expr) => {
        $x & ($crate::vmm::PAGE_SIZE - 1)
    };
}

#[macro_export]
macro_rules! page_align_down {
    ($x:expr) => {
        $x & !($crate::vmm::PAGE_SIZE - 1)
    };
}

#[macro_export]
macro_rules! page_align_up {
    ($x:expr) => {
        ($x + $crate::vmm::PAGE_SIZE - 1) & !($crate::vmm::PAGE_SIZE - 1)
    };
}

#[macro_export]
macro_rules! size_of {
    ($t:ty) => {
        ::core::mem::size_of::<$t>()
    };
}

/// Used to ensure volatile reads and writes to the memory wrapped by the cell.
#[repr(transparent)]
pub struct VolatileCell<T>(core::cell::UnsafeCell<T>);

impl<T> VolatileCell<T> {
    /// Volatile writes to the memory the cell wraps.
    pub fn write(&self, value: T) {
        // SAFETY: The cell owns the memory as T.
        unsafe { self.0.get().write_volatile(value) };
    }

    /// Volatile reads from the memory the cell wraps.
    pub fn read(&self) -> T {
        // SAFETY: The cell owns the memory as T.
        unsafe { self.0.get().read_volatile() }
    }
}

/// Represents a 64 bit pointer, of which its low and high bits are split in 32-bit-aligned volatile memory.
#[repr(C)]
pub struct VolatileSplitPtr<T> {
    low: VolatileCell<u32>,
    high: VolatileCell<u32>,
    marker: core::marker::PhantomData<T>,
}

impl<T> VolatileSplitPtr<T> {
    /// Sets the low and high components of the split pointer.
    pub fn set_ptr(&self, low: u32, high: u32) {
        self.low.write(low);
        self.high.write(high);
    }

    /// Reads the low and high pointer bits separately, and constructs a pointer from them.
    #[inline]
    pub fn get_ptr_mut(&self) -> *mut T {
        let low = self.low.read();
        let high = self.high.read();

        (((high as u64) << 32) | (low as u64)) as *mut T
    }
}
