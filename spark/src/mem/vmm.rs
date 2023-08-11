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

use crate::{
    dev::fdt,
    pages_for,
    pmm::{self},
    BOOT_HART_ID,
};
use alloc::collections::LinkedList;
use core::{
    ptr,
    sync::atomic::{AtomicUsize, Ordering},
};

pub const PAGE_SHIFT: u32 = 12;
pub const PAGE_SIZE: usize = 1 << PAGE_SHIFT;

#[repr(usize)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PagingMode {
    Sv39 = 8,
    Sv48 = 9,
    #[allow(dead_code)]
    Sv57 = 10,
}

impl PagingMode {
    pub const fn levels(self) -> usize {
        self as usize - 5
    }

    pub const fn max_level(self) -> usize {
        self.levels() - 1
    }

    pub const fn higher_half_start(self) -> usize {
        match self {
            PagingMode::Sv39 => 0xffffffc000000000,
            PagingMode::Sv48 => 0xffff800000000000,
            PagingMode::Sv57 => 0xff00000000000000,
        }
    }

    pub fn max_page_size(self) -> usize {
        static PAGE_SIZES: [usize; 5] =
            [0x1000, 0x200000, 0x40000000, 0x8000000000, 0x1000000000000];
        PAGE_SIZES[self.max_level()]
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
    #[allow(clippy::linkedlist)]
    maps: LinkedList<Mapping>,
    direct_map_base: usize,
}

/// Allocate a new page table
///
/// All page table allocations should use this function as it ensures the memory
/// is zero-initialized. Page tables containing uninitialized entries is not fun.
fn allocate_page_table() -> usize {
    let paddr = pmm::alloc_frames(1).unwrap();

    // New page tables must be zeroed to prevent bogus translations.
    // SAFETY: `paddr` is valid for `PAGE_SIZE` bytes.
    unsafe { ptr::write_bytes(paddr as *mut u8, 0, PAGE_SIZE) };

    paddr
}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MapError {
    /// The requested mapping overlaps with an existing mapping in the address space
    OverlappingMappings,
    /// A physical or virtual address was not aligned to the relevant page size
    MisalignedAddr,
    /// Invalid flags were specified to `map_pages()`
    InvalidFlags,
}

pub fn invalidate_page(vaddr: usize, asid: u16) {
    // SAFETY: It's fine.
    unsafe {
        asm!("sfence.vma {}, {}", in(reg) vaddr, in(reg) asid, options(nostack, preserves_flags));
    }
}

#[repr(u64)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum MemoryType {
    PMA = 0 << 61,
    NC = 1 << 61,
    IO = 2 << 61,
}

impl Default for MemoryType {
    fn default() -> Self {
        Self::PMA
    }
}

impl AddressSpace {
    /// Create a new, empty, virtual address space
    pub const fn new(paging_mode: PagingMode, direct_map_base: usize) -> AddressSpace {
        Self {
            root: 0,
            mode: paging_mode,
            asid: 0,
            maps: LinkedList::new(),
            direct_map_base,
        }
    }

    pub const fn paging_mode(&self) -> PagingMode {
        self.mode
    }

    pub fn mappings(&self) -> impl Iterator<Item = &Mapping> {
        self.maps.iter()
    }

    pub fn satp(&self) -> usize {
        ((self.mode as usize) << 60) | ((self.asid as usize) << 44) | (self.root >> 12)
    }

    /// Switch to this address space
    ///
    /// # Safety
    ///
    /// The caller must ensure that itself and any data it intended to access are properly
    /// mapped into this address space.
    pub unsafe fn switch_to(&self) {
        #[cfg(target_arch = "riscv64")]
        asm!("csrw satp, {}", in(reg) self.satp(), options(nostack, preserves_flags));
    }

    pub const fn direct_map_base(&self) -> usize {
        self.direct_map_base
    }

    pub fn direct_map_ptr<T: ?Sized>(&self, ptr: *const T) -> *const T {
        ptr.with_addr(self.direct_map_base + ptr.addr())
    }

    pub fn direct_map_ptr_mut<T: ?Sized>(&self, ptr: *mut T) -> *mut T {
        ptr.with_addr(self.direct_map_base + ptr.addr())
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

    /// Map pages into this virtual address space
    ///
    /// The pages will be mapped with the default memory type.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the requested mapping cannot be satisfied (ie. it would overlap with
    /// an existing mapping), or if invalid arguments are specified.
    pub fn map_pages(
        &mut self,
        virt: usize,
        phys: usize,
        size: usize,
        flag: MapFlags,
    ) -> Result<(), MapError> {
        self.map_pages_with_type(virt, phys, size, flag, MemoryType::default())
    }

    /// Map pages into this virtual address space with the specified [`MemoryType`]
    ///
    /// # Errors
    ///
    /// Returns `Err` if the requested mapping cannot be satisfied (ie. it would overlap with
    /// an existing mapping), or if invalid arguments are specified.
    pub fn map_pages_with_type(
        &mut self,
        virt: usize,
        phys: usize,
        size: usize,
        flag: MapFlags,
        pbmt: MemoryType,
    ) -> Result<(), MapError> {
        // `HUGE2M` and `HUGE1G` are mutually exclusive.
        if flag.contains(MapFlags::HUGE2M) && flag.contains(MapFlags::HUGE1G) {
            return Err(MapError::InvalidFlags);
        }

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
            self.map_page(virt + page_size * i, phys + page_size * i, flag, pbmt);
        }

        Ok(())
    }

    fn map_page(&mut self, virt: usize, phys: usize, flag: MapFlags, pbmt: MemoryType) {
        if self.root == 0 {
            self.root = allocate_page_table();
        }

        // SAFETY: The address is not null, and was allocated from `allocate_page_table()`
        // which always returns a valid, empty page table.
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
                entry.set_type(pbmt);
                return;
            }

            if !entry.is_present() {
                entry.set_address(allocate_page_table());
                entry.set_present(true);
            }

            assert!(!entry.is_mapped());

            // SAFETY: We only ever set the present flag to true when the entry
            // contains a valid address. We also ensured that this entry points
            // to another page table, and not a physical frame.
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
    #[derive(Clone, Copy, Debug)]
    pub struct MapFlags : u32 {
        const READ  = 1 << 0;
        const WRITE = 1 << 1;
        const EXEC  = 1 << 2;
        const USER  = 1 << 3;

        const HUGE2M = 1 << 4;
        const HUGE1G = 1 << 5;

        const RWX   = Self::READ.bits() | Self::WRITE.bits() | Self::EXEC.bits();
    }
}

impl MapFlags {
    fn map_level(self) -> usize {
        if self.contains(MapFlags::HUGE1G) {
            2
        } else {
            usize::from(self.contains(MapFlags::HUGE2M))
        }
    }

    fn page_size(self) -> usize {
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
    fn from(sgmt_flags: elf::SegmentFlags) -> Self {
        let mut map_flags = MapFlags::empty();

        map_flags.set(MapFlags::READ, sgmt_flags.contains(elf::SegmentFlags::READ));
        map_flags.set(
            MapFlags::WRITE,
            sgmt_flags.contains(elf::SegmentFlags::WRITE),
        );
        map_flags.set(MapFlags::EXEC, sgmt_flags.contains(elf::SegmentFlags::EXEC));

        map_flags
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

    const PBMT_MASK: usize = 3 << 61;

    fn set_type(&mut self, pbmt: MemoryType) {
        self.0 = (self.0 & !Self::PBMT_MASK) | pbmt as usize;
    }

    fn set_flags(&mut self, flag: MapFlags) {
        self.set_readable(flag.contains(MapFlags::READ));
        self.set_writable(flag.contains(MapFlags::WRITE));
        self.set_executable(flag.contains(MapFlags::EXEC));
        self.set_user(flag.contains(MapFlags::USER));
    }
}

/// Get the system's maximum paging mode from the device tree.
///
/// # Panics
///
/// This function may panic if the device tree does not contain a node for the BSP or if
/// the node does not contain an `mmu-type` property.
fn get_max_paging_mode_fdt(fdt: &::fdt::Fdt) -> PagingMode {
    use ::fdt::node::NodeProperty;

    let hartid = BOOT_HART_ID.load(Ordering::Relaxed);

    /*
     * Determine the paging mode supported by the BSP, we assumme the
     * other cores we're interested in will support the same.
     */
    let paging_mode = match fdt
        .find_all_nodes("/cpus/cpu")
        .find(|node| node.property("reg").and_then(NodeProperty::as_usize) == Some(hartid))
        .expect("no matching FDT node for the BSP??")
        .property("mmu-type")
        .and_then(NodeProperty::as_str)
        .expect("BSP has no `mmu-type` property")
    {
        "riscv,sv39" => PagingMode::Sv39,
        "riscv,sv48" => PagingMode::Sv48,
        "riscv,sv57" => PagingMode::Sv57,
        _ => panic!("unknown mmu-type"),
    };

    paging_mode
}

/// Determine the system's maximum paging mode by trial-and-error
fn detect_max_paging_mode() -> PagingMode {
    // Writes to `satp` with an invalid MODE field have no effect.
    // We can determine the maximum paging mode by trying to
    // enable each mode, starting with the maximum we support (Sv57) and
    // working our way down until we are successful.
    let mut table = PageTable {
        entries: [PageTableEntry(0); 512],
    };
    let mut paging_mode = PagingMode::Sv57;
    loop {
        // We must guarantee that at least this code will be identity-mapped if and when
        // we are successful. Mapping the entire lower-half hopefully does this for us.
        //
        // XXX: Theoretically a system could exist with RAM so high up that we cannot
        // identity-map it into the Sv39 address space and theoretically we could be
        // running there. Too bad, so sad.
        let mut addr = 0;
        for entry in &mut table.entries[..256] {
            *entry = PageTableEntry((addr >> 2) | 0xf);
            addr += paging_mode.max_page_size();
        }

        let satp = (paging_mode as usize) << 60 | (table.entries.as_ptr().addr() >> 12);
        let new: usize;
        unsafe {
            asm!(
                "csrw satp, {0}",
                "csrr {0}, satp",
                "csrw satp, zero",
                inout(reg) satp => new,
                options(nostack),
            );
        }

        if new == satp {
            break paging_mode;
        }

        match paging_mode {
            PagingMode::Sv57 => paging_mode = PagingMode::Sv48,
            PagingMode::Sv48 => paging_mode = PagingMode::Sv39,
            PagingMode::Sv39 => panic!("paging is not supported"),
        }
    }
}

/// Returns the maximum paging mode supported by the system
pub fn get_max_paging_mode() -> PagingMode {
    static MAX_PAGING_MODE: AtomicUsize = AtomicUsize::new(!0);
    let mode = MAX_PAGING_MODE.load(Ordering::Relaxed);
    if mode != !0 {
        // SAFETY: The value is only ever changed from !0 to a valid discriminant.
        return unsafe { core::mem::transmute(mode) };
    }

    #[cfg(feature = "fdt")]
    if let Some(fdt) = fdt::get_fdt() {
        let mode = get_max_paging_mode_fdt(fdt);
        MAX_PAGING_MODE.store(mode as _, Ordering::Relaxed);
        return mode;
    }

    let mode = detect_max_paging_mode();
    MAX_PAGING_MODE.store(mode as _, Ordering::Relaxed);
    mode
}
//
// /// Initialize a virtual address space with the requested paging mode
// ///
// /// # Panics
// ///
// /// This function will panic if the initial mappings (identity map and HHDM) cannot be created; or
// /// if `paging_mode` is not supported. Use [`get_max_paging_mode()`] to determine the maximum
// /// supported mode.
// pub fn init(paging_mode: PagingMode, direct_map_base: usize) -> AddressSpace {
//     assert!(
//         paging_mode <= get_max_paging_mode(),
//         "unsupported paging mode"
//     );
//
//     let mut vmspace = AddressSpace::new(paging_mode, direct_map_base);
//
//     let pmap_size = cmp::max(0x100000000, MAX_PHYS_ADDR.load(Ordering::Relaxed) + 1);
//
//     log::info!("initializing {:?} paging.", vmspace.paging_mode());
//
//     vmspace
//         .map_pages(0, 0, pmap_size, MapFlags::RWX | MapFlags::HUGE1G)
//         .expect("failed to create identity map");
//     vmspace
//         .map_pages(direct_map_base, 0, pmap_size, MapFlags::RWX | MapFlags::HUGE1G)
//         .expect("failed to create higher-half map");
//
//     vmspace
// }
