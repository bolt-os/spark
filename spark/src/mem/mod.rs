// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

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

pub unsafe fn cast_slice<T>(buf: &[u8]) -> &T {
    debug_assert!(buf.len() >= size_of!(T));
    let ptr = buf.as_ptr().cast::<T>();
    debug_assert!(ptr.is_aligned());
    &*ptr
}
