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
    page_align_down, page_align_up, pages_for, pmm, size_of,
    vmm::{AddressSpace, MapError},
};
use core::cmp;
use elf::{DynTag, Elf, Rela, RelocKind, Segment, SegmentKind};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LoadError {
    LowerHalfSegment,
    OverlappingSegments,
    NoSegments,
    TruncatedSegment,
}

pub struct Rtld<'a, 'elf> {
    pub elf: &'a Elf<'elf>,
    load_segments: Vec<Segment<'elf, 'elf>>,
    link_base: usize,
    // link_end: usize,
    image_size: usize,
    pub image_base: usize,
    flags: RtldFlags,
    reloc_offset: usize,
}

bitflags::bitflags! {
    struct RtldFlags : u32 {
        const IMAGE_LOADED = 1 << 0;
    }
}

impl<'elf, 'a: 'elf> Rtld<'a, 'elf> {
    pub fn new(elf: &'a Elf<'elf>) -> Result<Rtld<'a, 'elf>, LoadError> {
        let mut load_segments = vec![];
        let mut link_base = usize::MAX;
        let mut link_end = usize::MIN;
        // let mut dynamic = None;
        // let mut tls = None;

        for segment in elf.segments() {
            match segment.kind() {
                SegmentKind::Load => {
                    let base = segment.virtual_address() as usize;
                    let end = base + segment.mem_size();

                    if segment.mem_size() < segment.file_size() {
                        return Err(LoadError::TruncatedSegment);
                    }

                    link_base = cmp::min(link_base, base);
                    link_end = cmp::max(link_end, end);
                    load_segments.push(segment);
                }
                // SegmentKind::Dynamic => {
                //     assert!(dynamic.is_none());
                //     dynamic = Some(segment);
                // }
                // SegmentKind::Tls => {
                //     assert!(tls.is_none());
                //     tls = Some(segment);
                // }
                _ => continue,
            }
        }

        if load_segments.is_empty() {
            return Err(LoadError::NoSegments);
        }

        let image_size = link_end - link_base;

        Ok(Self {
            elf,
            load_segments,
            link_base,
            image_size,
            image_base: 0,
            reloc_offset: 0,
            flags: RtldFlags::empty(),
        })
    }

    pub fn set_relocation_offset(&mut self, offset: usize) {
        assert!(
            !self.flags.contains(RtldFlags::IMAGE_LOADED),
            "the relocation offset cannot be changed once the image has been loaded"
        );
        self.reloc_offset = offset;
    }

    pub fn check_ptr<T>(&self, ptr: *const T) -> bool {
        let obj_start = ptr.addr();
        let obj_end = obj_start + size_of!(T);
        let img_start = self.image_base;
        let img_end = img_start + self.image_size;

        img_start <= obj_start && obj_end <= img_end
    }

    pub fn reloc(&self, addr: usize) -> usize {
        self.reloc_offset.wrapping_add(addr)
    }

    pub fn reloc_signed(&self, addr: isize) -> usize {
        self.reloc_offset.wrapping_add_signed(addr)
    }

    pub fn map_image(&mut self, vmspace: &mut AddressSpace) -> Result<(), LoadError> {
        assert!(self.flags.contains(RtldFlags::IMAGE_LOADED));
        self.flags |= RtldFlags::IMAGE_LOADED;

        for segment in &self.load_segments {
            let virt = segment.virtual_address() as usize;
            let phys = self.image_base + (virt - self.link_base);
            let virt_p = page_align_down!(virt);
            let phys_p = page_align_down!(phys);

            if virt < vmspace.higher_half_start() {
                return Err(LoadError::LowerHalfSegment);
            }

            vmspace
                .map_pages(
                    virt_p,
                    phys_p,
                    page_align_up!(virt + segment.mem_size()) - virt_p,
                    segment.flags().into(),
                )
                .map_err(|err| match err {
                    MapError::OverlappingMappings => LoadError::OverlappingSegments,
                    MapError::InvalidFlags => panic!("this shouldn't happen"),
                    MapError::MisalignedAddr => unreachable!(), /* they damn well better be aligned */
                })
                .unwrap();
        }

        Ok(())
    }

    pub fn load_base(&self) -> usize {
        assert!(self.flags.contains(RtldFlags::IMAGE_LOADED));
        self.reloc(self.link_base)
    }

    pub fn load_image(&mut self) {
        assert!(!self.flags.contains(RtldFlags::IMAGE_LOADED));
        self.flags |= RtldFlags::IMAGE_LOADED;

        self.image_base = pmm::alloc_frames(pages_for!(self.image_size)).unwrap();

        for segment in &self.load_segments {
            let virt = segment.virtual_address() as usize;
            let phys = self.image_base + (virt - self.link_base);

            unsafe {
                core::ptr::copy_nonoverlapping(
                    segment.file_data().as_ptr(),
                    phys as *mut u8,
                    segment.file_size(),
                );
                core::ptr::write_bytes(
                    (phys + segment.file_size()) as *mut u8,
                    0,
                    segment.mem_size() - segment.file_size(),
                );
            }
        }
    }

    /// Convert a virtual address within the object to a physical address
    ///
    /// This is required for the Limine protocol to access the requests before we know
    /// the paging mode that will be used.
    pub fn to_image_ptr(&self, addr: usize) -> usize {
        assert!(self.flags.contains(RtldFlags::IMAGE_LOADED));
        let x = self.image_base + (addr - self.link_base);
        log::trace!("{addr:#018x} -> {x:#018x}");
        x
    }

    pub fn relocation_table(&self) -> Option<&'elf [Rela]> {
        self.elf.dynamic_table().and_then(|dyntab| {
            let mut addr = None;
            let mut size = None;
            let mut count = None;

            for entry in dyntab.table_raw() {
                match entry.tag() {
                    DynTag::Rela => addr = Some(entry.value()),
                    DynTag::RelaSize => size = Some(entry.value()),
                    DynTag::RelaCount => count = Some(entry.value()),
                    DynTag::RelaEnt => assert_eq!(entry.value(), size_of!(Rela)),
                    _ => {}
                }
            }

            let addr = self.image_base + (addr? - self.link_base);
            let len = size? / size_of!(Rela);
            if let Some(count) = count {
                assert_eq!(len, count);
            }

            unsafe { Some(core::slice::from_raw_parts(addr as *const _, len)) }
        })
    }

    pub fn do_relocations(&self) {
        let Some(relocation_table) = self.relocation_table() else { return };

        for reloc_entry in relocation_table {
            let location = self.reloc(reloc_entry.offset as usize);

            match reloc_entry.kind() {
                RelocKind::RISCV_NONE => {}
                RelocKind::RISCV_RELATIVE => {
                    let value = self.reloc_signed(reloc_entry.addend as isize);
                    unsafe { *(location as *mut usize) = value };
                }
                // RelocKind::RISCV_IRELATIVE => object.has_ifuncs = true,
                _ => panic!(),
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn _relocate(reloc_slide: usize, mut dyntab: *const elf::Dyn) -> usize {
    const RELOC_ERROR: usize = 1 << 63;
    const RELOC_OK: usize = 0;

    let relocation_table = unsafe {
        let mut table_addr = None;
        let mut table_size = None;
        let mut entry_size = None;

        loop {
            let entry = dyntab.read();

            match entry.tag() {
                DynTag::Null => break,
                DynTag::Rela => table_addr = Some(entry.value()),
                DynTag::RelaSize => table_size = Some(entry.value()),
                DynTag::RelaEnt => entry_size = Some(entry.value()),
                _ => {}
            }

            dyntab = dyntab.add(1);
        }

        if table_addr.is_none() && entry_size.is_none() {
            // There are no relocations
            return RELOC_OK;
        }

        let Some(table_addr) = table_addr else { return RELOC_ERROR };
        let Some(table_size) = table_size else { return RELOC_ERROR };
        let Some(entry_size) = entry_size else { return RELOC_ERROR };

        if entry_size != size_of!(elf::Rela) {
            return RELOC_ERROR;
        }

        let data = reloc_slide.wrapping_add(table_addr) as *const elf::Rela;
        let len = table_size / entry_size;

        core::slice::from_raw_parts(data, len)
    };

    for relocation in relocation_table {
        match relocation.kind() {
            RelocKind::RISCV_NONE => {}
            RelocKind::RISCV_RELATIVE => {
                let target = reloc_slide.wrapping_add(relocation.offset as usize);
                let value = reloc_slide.wrapping_add_signed(relocation.addend as isize);
                unsafe { *(target as *mut usize) = value };
            }
            _ => return RELOC_ERROR,
        }
    }

    RELOC_OK
}
