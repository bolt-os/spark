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

use crate::{pages_for, pmm, pmm::MAX_PHYS_ADDR};
use alloc::collections::LinkedList;
use core::{cmp, sync::atomic::Ordering};

pub const PAGE_SIZE: usize = 4096;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum PagingMode {
    Sv39 = 8,
    Sv48 = 9,
    #[allow(dead_code)]
    Sv57 = 10,
}

impl PagingMode {
    pub const fn levels(self) -> usize {
        match self {
            PagingMode::Sv39 => 3,
            PagingMode::Sv48 => 4,
            PagingMode::Sv57 => 5,
        }
    }

    pub const fn higher_half_start(self) -> usize {
        match self {
            PagingMode::Sv39 => 0xffffffc000000000,
            PagingMode::Sv48 => 0xffff800000000000,
            PagingMode::Sv57 => 0xff00000000000000,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Mapping {
    virt: usize,
    npgs: usize,
}

impl Mapping {
    pub fn start(&self) -> usize {
        self.virt
    }

    pub fn end(&self) -> usize {
        (self.virt + PAGE_SIZE * self.npgs) - 1
    }

    fn overlaps_with(&self, other: &Mapping) -> bool {
        other.start() <= self.end() && other.end() >= self.start()
    }
}

/// A virtual address space
pub struct AddressSpace {
    root: usize,
    mode: PagingMode,
    asid: u16,
    maps: LinkedList<Mapping>,
}

/// Allocate a new page table
///
/// All page table allocations should use this function as it ensures the memory
/// is zero-initialized. Page tables containing uninitialized entries is not fun.
fn allocate_page_table() -> usize {
    let paddr = pmm::alloc_frames(1).unwrap();
    unsafe {
        (paddr as *mut u8).write_bytes(0, PAGE_SIZE);
    }
    paddr
}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MapError {
    /// The requested mapping overlaps with an existing mapping in the address space
    OverlappingMappings,
    /// A physical or virtual address was not aligned to the relevant page size
    MisalignedAddr,
}

pub fn invalidate_page(vaddr: usize, asid: u16) {
    unsafe {
        asm!("sfence.vma {}, {}", in(reg) vaddr, in(reg) asid, options(nostack, preserves_flags));
    }
}

impl AddressSpace {
    /// Create a new, empty, virtual address space
    pub fn new(paging_mode: PagingMode) -> AddressSpace {
        Self {
            root: 0,
            mode: paging_mode,
            asid: 0,
            maps: LinkedList::new(),
        }
    }

    pub const fn paging_mode(&self) -> PagingMode {
        self.mode
    }

    pub fn mappings(&self) -> impl Iterator<Item = &Mapping> {
        self.maps.iter()
    }

    /// Switch to this address space
    ///
    /// # Safety
    ///
    /// The caller must ensure that itself and any data it intended to access are properly
    /// mapped into this address space.
    pub unsafe fn switch_to(&self) {
        let satp = ((self.mode as usize) << 60) | ((self.asid as usize) << 44) | (self.root >> 12);
        asm!("csrw satp, {}", in(reg) satp, options(nostack, preserves_flags));
    }

    pub fn higher_half_start(&self) -> usize {
        self.paging_mode().higher_half_start()
    }

    fn insert_mapping(&mut self, mapping: Mapping) -> Result<(), MapError> {
        if self.maps.iter().any(|m| m.overlaps_with(&mapping)) {
            return Err(MapError::OverlappingMappings);
        }

        self.maps.push_back(mapping);

        Ok(())
    }

    pub fn map_pages(
        &mut self,
        virt: usize,
        phys: usize,
        size: usize,
        flag: MapFlags,
    ) -> Result<(), MapError> {
        debug_assert!(
            (flag & (MapFlags::HUGE2M | MapFlags::HUGE1G))
                .bits()
                .count_ones()
                < 2
        );
        let page_size = flag.page_size();

        // Make sure the addresses are aligned to the requested page size
        if !pow2_is_aligned(virt, page_size) || !pow2_is_aligned(phys, page_size) {
            return Err(MapError::MisalignedAddr);
        }

        // Add to the list of mappings. This will fail if it overlaps with an existing one.
        self.insert_mapping(Mapping {
            virt,
            npgs: pages_for!(size),
        })?;

        // Map each page
        for i in 0..pages_for!(size, page_size) {
            self.map_page(virt + page_size * i, phys + page_size * i, flag);
        }

        Ok(())
    }

    fn map_page(&mut self, virt: usize, phys: usize, flag: MapFlags) {
        if self.root == 0 {
            self.root = allocate_page_table();
        }

        let mut table = unsafe { &mut *(self.root as *mut PageTable) };

        for x in (0..self.mode.levels()).rev() {
            let index = virt >> (12 + 9 * x) & 0x1ff;
            let entry = &mut table.entries[index];

            if x == flag.map_level() {
                assert!(!entry.is_present());
                assert!(!entry.is_mapped());
                entry.set_address(phys);
                entry.set_flags(flag);
                entry.set_present(true);
                return;
            }

            if !entry.is_present() {
                entry.set_address(allocate_page_table());
                entry.set_present(true);
            }

            table = unsafe { &mut *(entry.get_address() as *mut PageTable) };
        }

        unreachable!();
    }
}

pub const fn pow2_is_aligned(addr: usize, align: usize) -> bool {
    debug_assert!(align.is_power_of_two());
    addr & (align - 1) == 0
}

bitflags::bitflags! {
    pub struct MapFlags : u32 {
        const READ  = 1 << 0;
        const WRITE = 1 << 1;
        const EXEC  = 1 << 2;
        const USER  = 1 << 3;

        const HUGE2M = 1 << 4;
        const HUGE1G = 1 << 5;

        const RWX   = Self::READ.bits | Self::WRITE.bits | Self::EXEC.bits;
    }
}

impl MapFlags {
    fn map_level(&self) -> usize {
        if self.contains(MapFlags::HUGE1G) {
            2
        } else if self.contains(MapFlags::HUGE2M) {
            1
        } else {
            0
        }
    }

    fn page_size(&self) -> usize {
        if self.contains(MapFlags::HUGE1G) {
            0x40000000
        } else if self.contains(MapFlags::HUGE2M) {
            0x200000
        } else {
            0x1000
        }
    }
}

impl From<elf::SegmentFlags> for MapFlags {
    fn from(sflg: elf::SegmentFlags) -> Self {
        let mut mflg = MapFlags::empty();

        mflg.set(MapFlags::READ, sflg.contains(elf::SegmentFlags::READ));
        mflg.set(MapFlags::WRITE, sflg.contains(elf::SegmentFlags::WRITE));
        mflg.set(MapFlags::EXEC, sflg.contains(elf::SegmentFlags::EXEC));

        mflg
    }
}

#[repr(C, align(0x1000))]
struct PageTable {
    entries: [PageTableEntry; 512],
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct PageTableEntry(usize);

impl PageTableEntry {
    const PPN_MASK: usize = 0x003ffffffffffc00;

    fn is_present(self) -> bool {
        self.0 & 0x1 != 0
    }

    fn set_present(&mut self, val: bool) {
        if val {
            self.0 |= 0x1;
        } else {
            self.0 &= !0x1;
        }
    }

    fn set_address(&mut self, addr: usize) {
        let ppn = addr >> 12;
        self.0 = (self.0 & !Self::PPN_MASK) | (ppn << 10);
    }

    fn get_address(self) -> usize {
        (self.0 & Self::PPN_MASK) << 2
    }

    fn is_mapped(self) -> bool {
        self.0 & 0xe != 0
    }

    fn set_readable(&mut self, val: bool) {
        if val {
            self.0 |= 0x02;
        } else {
            self.0 &= !0x02;
        }
    }

    fn set_writable(&mut self, val: bool) {
        if val {
            self.0 |= 0x04;
        } else {
            self.0 &= !0x04;
        }
    }

    fn set_executable(&mut self, val: bool) {
        if val {
            self.0 |= 0x08;
        } else {
            self.0 &= !0x08;
        }
    }

    fn set_user(&mut self, val: bool) {
        if val {
            self.0 |= 0x10;
        } else {
            self.0 &= !0x10;
        }
    }

    fn set_flags(&mut self, flag: MapFlags) {
        self.set_readable(flag.contains(MapFlags::READ));
        self.set_writable(flag.contains(MapFlags::WRITE));
        self.set_executable(flag.contains(MapFlags::EXEC));
        self.set_user(flag.contains(MapFlags::USER));
    }
}

pub fn init(vmspace: &mut AddressSpace) {
    let pmap_size = cmp::max(0x100000000, MAX_PHYS_ADDR.load(Ordering::Relaxed) + 1);
    let hhdm_base = vmspace.paging_mode().higher_half_start();

    println!("Initializing {:?} paging.", vmspace.paging_mode());

    vmspace
        .map_pages(0, 0, pmap_size, MapFlags::RWX)
        .expect("failed to create identity map");
    vmspace
        .map_pages(hhdm_base, 0, pmap_size, MapFlags::RWX)
        .expect("failed to create higher-half map");

    unsafe { vmspace.switch_to() };
}

pub fn init_from_fdt(fdt: &fdt::Fdt, hartid: usize) -> AddressSpace {
    /*
     * Determine the paging mode supported by the BSP, we assumme the
     * other cores we're interested in will support the same.
     *
     * Default to Sv48 if available, falling back to Sv39.
     * Eventually we'll want to enable Sv57 if requested by the bootloader, which
     * means we need to read the config file before setting up paging.
     */
    let paging_mode = match fdt
        .find_all_nodes("/cpus/cpu")
        .find(|node| node.property("reg").and_then(|prop| prop.as_usize()) == Some(hartid))
        .expect("no matching FDT node for the BSP??")
        .property("mmu-type")
        .and_then(|prop| prop.as_str())
        .expect("BSP has no `mmu-type` property")
    {
        "riscv,sv39" => PagingMode::Sv39,
        "riscv,sv48" | "riscv,sv57" => PagingMode::Sv48,
        _ => panic!("unknown mmu-type"),
    };

    let mut vmspace = AddressSpace::new(paging_mode);

    let pmap_size = cmp::max(0x100000000, MAX_PHYS_ADDR.load(Ordering::Relaxed) + 1);
    let hhdm_base = vmspace.paging_mode().higher_half_start();

    println!("Initializing {:?} paging.", vmspace.paging_mode());

    vmspace
        .map_pages(0, 0, pmap_size, MapFlags::RWX | MapFlags::HUGE1G)
        .expect("failed to create identity map");
    vmspace
        .map_pages(hhdm_base, 0, pmap_size, MapFlags::RWX | MapFlags::HUGE1G)
        .expect("failed to create higher-half map");

    unsafe { vmspace.switch_to() };

    vmspace
}
