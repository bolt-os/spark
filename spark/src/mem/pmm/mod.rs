/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

mod freelist_allocator;
mod init_ranges;

use crate::vmm::PAGE_SIZE;
use core::sync::atomic::{AtomicUsize, Ordering};
#[cfg(sbi)]
use {
    crate::{page_align_down, page_align_up, pages_for, size_of, sys::fdt},
    freelist_allocator::FreelistAllocator,
    init_ranges::InitRanges,
    libsa::extern_sym,
    spin::Mutex,
};

pub static PHYSMAP: Mutex<FreelistAllocator> = Mutex::new(FreelistAllocator::new());
pub static MAX_PHYS_ADDR: AtomicUsize = AtomicUsize::new(0);

/// Allocate frames of physical memory
pub fn alloc_frames(num_frames: usize) -> Option<usize> {
    alloc_frames_impl(num_frames)
}

#[cfg(uefi)]
fn alloc_frames_impl(num_frames: usize) -> Option<usize> {
    use uefi::table::{AllocPagesType, MemoryType};
    let bs = uefi::boot_services();
    bs.allocate_pages(AllocPagesType::Any, MemoryType::LOADER_DATA, num_frames)
        .map(|addr| addr as usize)
        .ok()
}

#[cfg(sbi)]
fn alloc_frames_impl(num_frames: usize) -> Option<usize> {
    let mut physmap = PHYSMAP.lock();
    physmap.allocate(num_frames * PAGE_SIZE, PAGE_SIZE)
}

/// Free physical frames of memory
///
/// # Safety
///
/// The memory being freed must have previously been allocated by a call
/// to [`alloc_frames()`] or [`alloc_frames_aligned()`]. The only exception to
/// this is when the physical memory allocator is first initialized.
///
/// # Panics
///
/// On UEFI this function may panic if the firmware returns an error.
pub unsafe fn free_frames(base: usize, num_frames: usize) {
    free_frames_impl(base, num_frames);
}

#[cfg(uefi)]
unsafe fn free_frames_impl(base: usize, num_frames: usize) {
    let bs = uefi::boot_services();
    bs.free_pages(base as u64, num_frames).unwrap();
}

#[cfg(sbi)]
unsafe fn free_frames_impl(base: usize, num_frames: usize) {
    let mut physmap = PHYSMAP.lock();
    physmap.deallocate(base, num_frames * PAGE_SIZE);
}

/// Generate a Limine memory map and exit boot services.
///
/// # Panics
///
/// This function may panic if it fails to allocate memory for the map or fails to exit
/// boot services.
pub fn generate_limine_memory_map(vmspace: &mut super::vmm::AddressSpace) -> limine::MemoryMap {
    generate_limine_memory_map_impl(vmspace)
}

#[cfg(all(uefi, feature = "proto-limine"))]
fn generate_limine_memory_map_impl(vmspace: &mut super::vmm::AddressSpace) -> limine::MemoryMap {
    use core::mem::MaybeUninit;
    use uefi::table::MemoryDescriptor;

    let bs = uefi::boot_services();

    // Get the size of the memory map.
    let map_info = bs.get_memory_map_info().unwrap();

    // Allocate buffers for everything we need:
    // - the memory map itself
    // - the Limine memory map entries
    // - the pointers to the Limine entries
    let buffer = Box::leak(vec![0; map_info.buffer_size].into_boxed_slice());
    let len = map_info.buffer_size / map_info.descriptor_size;
    let limine_entries = Box::leak(Box::<[limine::MemoryMapEntry]>::new_uninit_slice(len));
    let limine_ptrs = Box::leak(Box::new_uninit_slice(len));

    // Get the memory map.
    let map_info = bs.get_memory_map(buffer, map_info.map_key).unwrap();

    // Exit boot services.
    if let Err(status) = bs.exit_boot_services(uefi::image_handle(), map_info.map_key) {
        panic!("failed to exit boot services: {status:?}");
    }

    // Translate the UEFI memory map into a Limine memory map.
    let mut i = 0;
    let mut offset = 0;
    loop {
        if offset + map_info.descriptor_size >= map_info.buffer_size {
            break;
        }

        let efi_entry = unsafe {
            &*buffer[offset..][..map_info.descriptor_size]
                .as_ptr()
                .cast::<MemoryDescriptor>()
        };

        let limine_entry = limine::MemoryMapEntry::new(
            efi_entry.phys as usize,
            efi_entry.num_pages as usize * PAGE_SIZE,
            efi_entry.kind.into(),
        );

        unsafe {
            let ptr = limine_entries
                .as_mut_ptr()
                .add(i)
                .cast::<limine::MemoryMapEntry>();
            ptr.write(limine_entry);

            limine_ptrs
                .as_mut_ptr()
                .add(i)
                .write(MaybeUninit::new(vmspace.direct_map_ptr_mut(ptr)));
        }

        i += 1;
        offset += map_info.descriptor_size;
    }

    unsafe {
        let ptr = MaybeUninit::slice_assume_init_mut(limine_ptrs).as_mut_ptr();
        limine::MemoryMap::new(ptr, i)
    }
}

#[cfg(all(sbi, feature = "proto-limine"))]
fn generate_limine_memory_map_impl(vmspace: &mut super::vmm::AddressSpace) -> limine::MemoryMap {
    let map = PHYSMAP.lock();

    let needed = pages_for!((size_of!(usize) + size_of!(limine::MemoryMapEntry)) * map.len());
    drop(map);

    let buffer = alloc_frames(needed).unwrap() as *mut u8;

    let map = PHYSMAP.lock();

    unsafe {
        let num_entries = map.entries().count();
        let len = map.len();
        assert_eq!(num_entries, len);

        let pointers = buffer.cast::<*mut limine::MemoryMapEntry>();
        let entries = buffer
            .add(size_of!(*mut limine::MemoryMapEntry) * map.len())
            .cast::<limine::MemoryMapEntry>();

        buffer.write_bytes(0, needed * PAGE_SIZE);

        for (i, tag) in map.entries().enumerate() {
            println!("{:#010x} {:#010x}", tag.base, tag.size);
            let entry = entries.add(i);

            pointers.add(i).write(vmspace.direct_map_ptr_mut(entry));
            entry.write(limine::MemoryMapEntry::new(
                tag.base,
                tag.size,
                limine::MemoryKind::Usable,
            ));

            println!("{:#010x?}", *entry);
        }

        limine::MemoryMap::new(
            pointers.with_addr(vmspace.higher_half_start() + pointers.addr()),
            map.len(),
        )
    }
}

const MAX_INIT_RANGES: usize = 64;

/// Initialize the physical memory allocator from the information in the Device Tree
#[cfg(sbi)]
pub fn init() {
    let mut init_ranges = InitRanges::<MAX_INIT_RANGES>::new();
    let mut max_phys_addr = 0;

    let fdt = fdt::get_fdt();

    // Add all `/memory*` nodes.
    for node in fdt
        .root()
        .children()
        .filter(|node| node.name.starts_with("memory"))
    {
        for reg in node.reg().into_iter().flatten().filter_map(Result::ok) {
            let end = reg.addr + reg.size;
            max_phys_addr = max_phys_addr.max(end as usize);
            init_ranges.insert(reg.addr as usize, reg.size as usize);
        }
    }

    MAX_PHYS_ADDR.store(max_phys_addr, Ordering::Relaxed);

    // Remove all `/reserved-memory` nodes.
    for node in fdt
        .find_node("/reserved-memory")
        .into_iter()
        .flat_map(|node| node.children())
    {
        for reg in node.reg().into_iter().flatten().filter_map(Result::ok) {
            init_ranges.remove(reg.addr as usize, reg.size as usize);
        }
    }

    // Remove all entries in the memory reservation block.
    for entry in fdt.memory_reservations {
        init_ranges.remove(entry.addr.get() as usize, entry.size.get() as usize);
    }

    // Remove the DTB.
    init_ranges.remove(fdt.as_ptr().addr(), fdt.total_size());

    // Remove the bootloader itself.
    let spark_start = extern_sym!(__image_base).addr();
    let spark_size = extern_sym!(__image_size).addr();
    init_ranges.remove(spark_start, spark_size);

    // Initialize PMM.
    let mut physmap = PHYSMAP.lock();
    for range in init_ranges.ranges() {
        let base = page_align_up!(range.base);
        let size = page_align_down!(range.base + range.size - 1) - base;
        unsafe { physmap.add_region(base, size) };
    }
}
