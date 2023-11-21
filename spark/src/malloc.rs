// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use core::{alloc::GlobalAlloc, ptr};
#[cfg(sbi)]
use {
    crate::{pages_for, pmm, vmm::PAGE_SIZE},
    core::cmp::Ordering,
};

#[cfg(uefi)]
struct BootServicesAllocator;

#[cfg(uefi)]
unsafe impl GlobalAlloc for BootServicesAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        use uefi::table::MemoryType;
        let bs = uefi::boot_services();
        match bs.allocate_pool(MemoryType::LOADER_DATA, layout.size()) {
            Ok(ptr) => ptr,
            Err(err) => {
                log::error!("{err:?}");
                ptr::null_mut()
            }
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: core::alloc::Layout) {
        let bs = uefi::boot_services();
        if let Err(err) = bs.free_pool(ptr) {
            log::error!("{err:?}");
            panic!();
        }
    }
}

#[cfg(uefi)]
#[global_allocator]
static MALLOC: BootServicesAllocator = BootServicesAllocator;

#[cfg(sbi)]
struct BadButGoodEnoughAllocator;

#[cfg(sbi)]
unsafe impl GlobalAlloc for BadButGoodEnoughAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        assert!(layout.align() <= PAGE_SIZE);

        let num_frames = pages_for!(layout.size());
        pmm::alloc_frames(num_frames).map_or_else(ptr::null_mut, |addr| {
            let ptr = addr as *mut u8;
            ptr.write_bytes(0, num_frames * PAGE_SIZE);
            ptr
        })
    }

    unsafe fn realloc(
        &self,
        ptr: *mut u8,
        layout: core::alloc::Layout,
        new_size: usize,
    ) -> *mut u8 {
        let old_frames = pages_for!(layout.size());
        let new_frames = pages_for!(new_size);

        match new_frames.cmp(&old_frames) {
            Ordering::Equal => ptr,
            Ordering::Less => {
                let free_frames = old_frames - new_frames;
                let free_base = ptr as usize + PAGE_SIZE * new_frames;

                pmm::free_frames(free_base, free_frames);

                ptr
            }
            Ordering::Greater => pmm::alloc_frames(new_frames).map_or_else(ptr::null_mut, |addr| {
                let new_ptr = addr as *mut u8;
                new_ptr.copy_from(ptr, layout.size());
                pmm::free_frames(ptr as usize, old_frames);
                new_ptr
            }),
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: core::alloc::Layout) {
        pmm::free_frames(ptr as usize, pages_for!(layout.size()));
    }
}

#[cfg(sbi)]
#[global_allocator]
static MALLOC: BadButGoodEnoughAllocator = BadButGoodEnoughAllocator;
