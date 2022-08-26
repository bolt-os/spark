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

use crate::{page_align_up, vmm::PAGE_SIZE};
use core::{
    ptr,
    sync::atomic::{AtomicUsize, Ordering},
};
use spin::mutex::SpinMutex;

pub static MAX_PHYS_ADDR: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy)]
struct InitRange {
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

    fn start(&self) -> usize {
        self.base
    }

    fn end(&self) -> usize {
        self.base + self.size
    }

    fn overlaps_with(&self, other: &InitRange) -> bool {
        other.start() <= self.end() && other.end() >= self.start()
    }

    fn contains(&self, other: &InitRange) -> bool {
        self.start() <= other.start() && other.end() <= self.end()
    }
}

struct InitRanges {
    ranges: [InitRange; Self::MAX_RANGES],
    len: usize,
}

impl InitRanges {
    const MAX_RANGES: usize = 256;

    fn new() -> InitRanges {
        InitRanges {
            ranges: [InitRange::empty(); Self::MAX_RANGES],
            len: 0,
        }
    }

    fn ranges(&self) -> &[InitRange] {
        &self.ranges[..self.len]
    }

    fn insert(&mut self, base: usize, size: usize) {
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

        if insertion_point >= self.ranges.len() {
            panic!("too many memory ranges!");
        }

        println!("insert: {insert_range:?}");
        self.ranges[insertion_point] = insert_range;
        self.len += 1;
    }

    fn remove(&mut self, base: usize, size: usize) {
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

        if self.len == Self::MAX_RANGES {
            panic!("");
        }

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

static PHYSMAP: SpinMutex<spark::FreeList> = SpinMutex::new(spark::FreeList::new_empty());

/// Hand off the list of free physical frames to the kernel
///
/// # Safety
///
/// This should be one of the very last things that is called before control is passed
/// to the kernel. All memory allocations will fail after this function returns.
pub unsafe fn handoff() -> spark::FreeList {
    let mut physmap = PHYSMAP.lock();
    core::mem::take(&mut *physmap)
}

/// Allocate frames of physical memory
pub fn alloc_frames(num_frames: usize) -> Option<usize> {
    let mut physmap = PHYSMAP.lock();
    let mut regionp = physmap.tail;

    while let Some(mut region) = regionp {
        let region = unsafe { region.as_mut() };

        if region.num_frames >= num_frames {
            region.num_frames -= num_frames;

            if region.num_frames == 0 {
                physmap.remove_region(region);
            }

            return Some(region.base + PAGE_SIZE * region.num_frames);
        }

        regionp = region.prev;
    }

    None
}

/// Allocate frames of physical memory aligned to a specific boundary
pub fn alloc_frames_aligned(num_frames: usize, align: usize) -> Option<usize> {
    assert!(align.is_power_of_two());
    let align = page_align_up!(align);

    let mut physmap = PHYSMAP.lock();
    let mut regionp = physmap.head;

    while let Some(mut region) = regionp {
        let region = unsafe { region.as_mut() };

        if region.num_frames >= num_frames {
            // how far would we have to increment the base address to make it aligned
            let align_offset = align.wrapping_sub(region.base) & (align - 1);

            if align_offset == 0 {
                region.num_frames -= num_frames;
                let base = if region.num_frames == 0 {
                    physmap.remove_region(region);
                    region.base
                } else {
                    let base = region.base;
                    region.base += num_frames;
                    base
                };

                return Some(base);
            }

            // are there still enough pages if we increment the base?

            // pull those pages out

            println!("align_offset: {align_offset:#018x}");
            todo!();
        }

        regionp = region.next;
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
    let mut regionp = physmap.head;

    while let Some(region) = regionp {
        let region = unsafe { region.as_ref() };
        println!(
            "  {:#018x} -> {:#018x}, {} pages",
            region.base(),
            region.end(),
            region.num_frames
        );
        regionp = region.next;
    }
}

/// Initialize the physical memory allocator from the information in the Device Tree
pub fn init_from_fdt(fdt: &fdt::Fdt, dtb_ptr: *const u8) {
    let mut initmap = InitRanges::new();
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

    extern "C" {
        static __spark_start: u8;
        static __spark_end: u8;
    }

    let spark_start = unsafe { ptr::addr_of!(__spark_start).addr() };
    let spark_size = unsafe { ptr::addr_of!(__spark_end).addr() - spark_start };

    initmap.remove(spark_start, spark_size);

    for range in initmap.ranges() {
        let start = (range.base + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        let end = (range.base + range.size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        let size = end - start;
        unsafe { free_frames(start, size / PAGE_SIZE) };
    }
}
