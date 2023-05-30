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

use crate::vmm::PAGE_SIZE;
use core::sync::atomic::{AtomicUsize, Ordering};
#[cfg(sbi)]
use {
    crate::{page_align_down, page_align_up, pages_for, size_of},
    // core::,
    libsa::extern_sym,
};

#[cfg(sbi)]
mod ranges {
    #[derive(Clone, Copy)]
    pub struct InitRange {
        /// base physical address
        base: usize,
        /// number of pages
        size: usize,
    }

    impl core::fmt::Debug for InitRange {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_struct("Range")
                .field("base", &format_args!("{:#018x}", self.base))
                .field("size", &format_args!("{:#018x}", self.size))
                .field("end", &format_args!("{:#018x}", (self.base + self.size)))
                .finish()
        }
    }

    impl InitRange {
        fn empty() -> InitRange {
            Self { base: 0, size: 0 }
        }

        pub fn start(&self) -> usize {
            self.base
        }

        pub fn end(&self) -> usize {
            self.base + self.size
        }

        fn overlaps_with(&self, other: &InitRange) -> bool {
            other.start() <= self.end() && other.end() >= self.start()
        }

        fn contains(&self, other: &InitRange) -> bool {
            self.start() <= other.start() && other.end() <= self.end()
        }
    }

    pub struct InitRanges {
        pub ranges: [InitRange; Self::MAX_RANGES],
        pub len: usize,
    }

    impl InitRanges {
        const MAX_RANGES: usize = 256;

        pub fn new() -> InitRanges {
            InitRanges {
                ranges: [InitRange::empty(); Self::MAX_RANGES],
                len: 0,
            }
        }

        pub fn ranges(&self) -> &[InitRange] {
            &self.ranges[..self.len]
        }

        pub fn insert(&mut self, base: usize, size: usize) {
            let insert_range = InitRange { base, size };

            if self
                .ranges()
                .iter()
                .any(|range| range.overlaps_with(&insert_range))
            {
                panic!("overlapping free memory ranges");
            }

            let insertion_point = self
                .ranges
                .partition_point(|range| insert_range.start() < range.start());

            debug_assert!(insertion_point <= self.len);

            if insertion_point > 0 {
                let prev = &mut self.ranges[insertion_point - 1];
                if prev.end() == insert_range.start() {
                    prev.size += insert_range.size;
                    return;
                }
            }

            if self.len > 0 && insertion_point < self.len - 1 {
                let next = &mut self.ranges[insertion_point + 1];
                if insert_range.end() == next.start() {
                    next.size += insert_range.size;
                    next.base = insert_range.base;
                    return;
                }
            }

            assert!(insertion_point < Self::MAX_RANGES, "too many memory ranges");

            println!("insert: {insert_range:?}");
            self.ranges[insertion_point] = insert_range;
            self.len += 1;
        }

        pub fn remove(&mut self, base: usize, size: usize) {
            let remove_range = InitRange { base, size };
            println!("remove: {remove_range:?}");

            let (index, from_range) = self.ranges[..self.len]
                .iter_mut()
                .enumerate()
                .find(|(_, range)| range.contains(&remove_range))
                .expect("msg;");

            if base == from_range.base {
                from_range.size -= size;
                from_range.base = base + size;
                return;
            }

            if base + size == from_range.end() {
                from_range.size -= size;
                return;
            }

            assert!(self.len < Self::MAX_RANGES, "too many memory ranges");

            let new_range = InitRange {
                base: remove_range.end(),
                size: from_range.end() - remove_range.end(),
            };

            from_range.size = remove_range.start() - from_range.start();

            let insertion_point = index + 1;
            self.ranges
                .copy_within(insertion_point..self.len, insertion_point + 1);
            self.ranges[insertion_point] = new_range;
            self.len += 1;
        }
    }
}

#[cfg(sbi)]
mod physmap {
    use crate::vmm::PAGE_SIZE;
    use core::ptr;
    use spin::mutex::SpinMutex;

    #[repr(C)]
    #[derive(Debug)]
    pub struct Region {
        pub base: usize,
        pub num_frames: usize,
        prev: *mut Region,
        pub next: *mut Region,
    }

    impl Region {
        const fn end(&self) -> usize {
            self.base + PAGE_SIZE * self.num_frames
        }
    }

    /// A doubly-linked list of usable memory ranges stored intrusively in the memory it describes
    #[repr(C)]
    #[derive(Debug)]
    pub struct FreeList {
        pub head: *mut Region,
        tail: *mut Region,
        pub len: usize,
    }

    impl FreeList {
        const fn new() -> FreeList {
            Self {
                head: ptr::null_mut(),
                tail: ptr::null_mut(),
                len: 0,
            }
        }

        unsafe fn remove_region(&mut self, region: *mut Region) {
            assert!(!region.is_null());

            let prev = (*region).prev;
            let next = (*region).next;

            if prev.is_null() {
                self.head = next;
            } else {
                (*prev).next = next;
            }
            if next.is_null() {
                self.tail = prev;
            } else {
                (*next).prev = prev;
            }

            self.len -= 1;
        }

        unsafe fn insert_region(&mut self, mut base: usize, mut num_frames: usize) {
            let mut prev = self.tail;
            let mut next = ptr::null_mut();
            while !prev.is_null() {
                if (*prev).base < base {
                    break;
                }
                next = prev;
                prev = (*prev).prev;
            }

            if !prev.is_null() && (*prev).end() == base {
                base = (*prev).base;
                num_frames += (*prev).num_frames;
                prev = (*prev).prev;
            }
            if !next.is_null() {
                let end = base + PAGE_SIZE * num_frames;
                if end == (*next).base {
                    num_frames += (*next).num_frames;
                    next = (*next).next;
                }
            }

            let new = base as *mut Region;
            new.write(Region {
                base,
                num_frames,
                prev,
                next,
            });

            if prev.is_null() {
                self.head = new;
            } else {
                (*prev).next = new;
            }
            if next.is_null() {
                self.tail = new;
            } else {
                (*next).prev = new;
            }

            self.len += 1;
        }
    }

    unsafe impl Send for FreeList {}

    pub static PHYSMAP: SpinMutex<FreeList> = SpinMutex::new(FreeList::new());

    /// Allocate frames of physical memory
    pub fn alloc_frames(num_frames: usize) -> Option<usize> {
        let mut physmap = PHYSMAP.lock();
        let mut region = physmap.tail;

        while !region.is_null() {
            unsafe {
                if (*region).num_frames >= num_frames {
                    (*region).num_frames -= num_frames;
                    if (*region).num_frames == 0 {
                        physmap.remove_region(region);
                    }
                    return Some((*region).base + PAGE_SIZE * (*region).num_frames);
                }
                region = (*region).prev;
            }
        }

        None
    }

    /// Free physical frames of memory
    ///
    /// # Safety
    ///
    /// The memory being freed must have previously been allocated by a call
    /// to [`alloc_frames()`] or [`alloc_frames_aligned()`]. The only exception to
    /// this is when the physical memory allocator is first initialized.
    pub unsafe fn free_frames(base: usize, num_frames: usize) {
        PHYSMAP.lock().insert_region(base, num_frames);
    }

    /// Prints the current list of free physical frames
    pub fn print() {
        let physmap = PHYSMAP.lock();
        let mut region = physmap.head;
        unsafe {
            while !region.is_null() {
                println!(
                    "  {:#018x} -> {:#018x}, {} pages",
                    (*region).base,
                    (*region).end(),
                    (*region).num_frames
                );
                region = (*region).next;
            }
        }
    }

    // /// Allocate frames of physical memory aligned to a specific boundary
    // ///
    // /// # Panics
    // ///
    // /// This function will panic if `align` is not a power of two.
    // pub fn alloc_frames_aligned(num_frames: usize, align: usize, low_memory: bool) -> Option<usize> {
    //     assert!(align.is_power_of_two());
    //     // let align = page_align_up!(align);
    //
    //     let mut physmap = PHYSMAP.lock();
    //     let mut region = physmap.head;
    //
    //     // while let Some(mut region) = regionp {
    //     while !region.is_null() {
    //         // let region = unsafe { region.as_mut() };
    //
    //         if region.num_frames >= num_frames {
    //             // how far would we have to increment the base address to make it aligned
    //             let align_offset = align.wrapping_sub(region.base) & (align - 1);
    //
    //             if align_offset == 0 {
    //                 region.num_frames -= num_frames;
    //                 let base = if region.num_frames == 0 {
    //                     physmap.remove_region(region);
    //                     region.base
    //                 } else {
    //                     let base = region.base;
    //                     region.base += num_frames * PAGE_SIZE;
    //                     base
    //                 };
    //
    //                 return Some(base);
    //             }
    //
    //             // are there still enough frames if we increment the base?
    //             let align_frames = align_offset / PAGE_SIZE;
    //
    //             if region.num_frames < align_frames + num_frames {
    //                 continue;
    //             }
    //
    //             // are we execeeding a `low_memory` requirement?
    //             let alloc_base = region.base + align_frames * PAGE_SIZE;
    //             if low_memory && alloc_base + num_frames * PAGE_SIZE > u32::MAX as usize {
    //                 // the list is sorted, if this node exceeds the limit the rest will too.
    //                 return None;
    //             }
    //
    //             // yes, pull those frames and split the entry
    //             let new_base = region.base + PAGE_SIZE * (align_frames + num_frames);
    //             let new_size = region.end() - new_base;
    //
    //             let new_node = unsafe {
    //                 let ptr = new_base as *mut Region;
    //
    //                 ptr.write(Region {
    //                     base: new_base,
    //                     num_frames: new_size / PAGE_SIZE,
    //                     next: region.next,
    //                     prev: regionp,
    //                 });
    //
    //                 NonNull::new_unchecked(ptr)
    //             };
    //
    //             region.num_frames = align_frames;
    //             region.next = Some(new_node);
    //
    //             if let Some(mut next) = region.next {
    //                 unsafe { next.as_mut().prev = Some(new_node) };
    //             } else {
    //                 physmap.tail = Some(new_node);
    //             }
    //
    //             return Some(alloc_base);
    //         }
    //
    //         regionp = region.next;
    //     }
    //
    //     None
    // }
}

// #[cfg(sbi)]
pub static MAX_PHYS_ADDR: AtomicUsize = AtomicUsize::new(0);

#[cfg(sbi)]
pub use physmap::{alloc_frames, free_frames};

/// Allocate frames of physical memory
#[cfg(uefi)]
pub fn alloc_frames(num_frames: usize) -> Option<usize> {
    use uefi::table::{AllocPagesType, MemoryType};
    let bs = uefi::boot_services();
    bs.allocate_pages(AllocPagesType::Any, MemoryType::LOADER_DATA, num_frames)
        .map(|addr| addr as usize)
        .ok()
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
#[cfg(uefi)]
pub unsafe fn free_frames(base: usize, num_frames: usize) {
    let bs = uefi::boot_services();
    bs.free_pages(base as u64, num_frames).unwrap();
}

/// Generate a Limine memory map and exit boot services.
///
/// # Panics
///
/// This function may panic if it fails to allocate memory for the map or fails to exit
/// boot services.
#[cfg(all(uefi, feature = "proto-limine"))]
pub fn generate_limine_memory_map(vmspace: &mut super::vmm::AddressSpace) -> limine::MemoryMap {
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

#[cfg(sbi)]
pub fn generate_limine_memory_map(vmspace: &mut super::vmm::AddressSpace) -> limine::MemoryMap {
    let map = physmap::PHYSMAP.lock();

    let needed = pages_for!((size_of!(usize) + size_of!(limine::MemoryMapEntry)) * map.len);
    drop(map);

    let buffer = alloc_frames(needed).unwrap() as *mut u8;

    let map = physmap::PHYSMAP.lock();

    unsafe {
        let pointers = buffer.cast::<*mut limine::MemoryMapEntry>();
        let entries = buffer
            .add(size_of!(*mut limine::MemoryMapEntry) * map.len)
            .cast::<limine::MemoryMapEntry>();

        buffer.write_bytes(0, needed * PAGE_SIZE);

        let mut i = 0;
        let mut region = map.head;
        while !region.is_null() {
            let entry = entries.add(i);
            pointers
                .add(i)
                .write(entry.with_addr(vmspace.higher_half_start() + entry.addr()));

            entry.write(limine::MemoryMapEntry::new(
                (*region).base,
                (*region).num_frames * PAGE_SIZE,
                limine::MemoryKind::Usable,
            ));

            region = (*region).next;
            i += 1;
        }

        limine::MemoryMap::new(
            pointers.with_addr(vmspace.higher_half_start() + pointers.addr()),
            i,
        )
    }
}

/// Initialize the physical memory allocator from the information in the Device Tree
#[cfg(all(not(uefi), feature = "fdt"))]
pub fn init_from_fdt(fdt: &fdt::Fdt, dtb_ptr: *const u8) {
    let mut initmap = ranges::InitRanges::new();
    let mut max_phys_addr = 0;

    for node in fdt.find_all_nodes("/memory") {
        if let Some(regions) = node.reg() {
            for region in regions {
                let base = region.starting_address as usize;
                let size = region.size.unwrap_or_default();

                let end = base + size - 1;
                if end > max_phys_addr {
                    max_phys_addr = end;
                }

                initmap.insert(base, size);
            }
        }
    }

    MAX_PHYS_ADDR.store(max_phys_addr, Ordering::Relaxed);

    if let Some(reserved_memory_node) = fdt.find_node("/reserved-memory") {
        for node in reserved_memory_node.children() {
            if let Some(regions) = node.reg() {
                for region in regions {
                    let base = region.starting_address as usize;
                    let size = region.size.unwrap_or_default();

                    let end = base + size - 1;
                    if end > MAX_PHYS_ADDR.load(Ordering::Relaxed) {
                        MAX_PHYS_ADDR.store(end, Ordering::Relaxed);
                    }

                    initmap.remove(base, size);
                }
            }
        }
    }

    for region in fdt.memory_reservations() {
        let base = region.address() as usize;
        let size = region.size();

        let end = base + size - 1;
        if end > MAX_PHYS_ADDR.load(Ordering::Relaxed) {
            MAX_PHYS_ADDR.store(end, Ordering::Relaxed);
        }

        initmap.remove(base, size);
    }

    initmap.remove(dtb_ptr as _, fdt.total_size());

    let spark_start = extern_sym!(__spark_start).addr();
    let spark_size = extern_sym!(__spark_end).addr() - spark_start;
    initmap.remove(spark_start, spark_size);

    for range in initmap.ranges() {
        let start = page_align_up!(range.start());
        let size = page_align_down!(range.end() - 1) - start;

        unsafe { free_frames(start, size / PAGE_SIZE) };
    }
}
