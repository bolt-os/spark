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
use core::{cell::OnceCell, cmp};
use elf::{DynTag, Elf, Rela, RelocKind, SegmentKind};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum LoadError {
    LowerHalfSegment,
    OverlappingSegments,
    NoSegments,
}

pub struct Rtld<'elf> {
    object: &'elf elf::Elf<'elf>,
    pub link_base: usize,
    /// Physical address at which the object's image was loaded
    pub image_base: usize,
    /// Total size of the image, in bytes
    pub image_size: usize,
    /// Virtual address at which the object's image was loaded
    pub load_base: usize,
    /// Relocation slide applied to the object (general relocation/KASLR)
    ///
    /// This value is the difference between the address at which the object was linked
    /// and the actual address at which is was loaded (`load_base`).
    pub reloc_base: usize,
    relocation_table: OnceCell<Option<&'elf [Rela]>>,
    has_ifuncs: bool,
}

impl<'elf> core::ops::Deref for Rtld<'elf> {
    type Target = elf::Elf<'elf>;

    fn deref(&self) -> &Self::Target {
        self.object
    }
}

impl<'elf> Rtld<'elf> {
    pub fn image_as_slice(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.image_base as *const u8, self.image_size) }
    }

    fn reloc_addr(&self, addr: usize) -> usize {
        self.reloc_base.wrapping_add(addr)
    }

    pub fn relocation_table(&self) -> Option<&'elf [Rela]> {
        *self.relocation_table.get_or_init(|| {
            self.object.dynamic_table().and_then(|dyntab| {
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
        })
    }

    pub fn is_relocatable(&self) -> bool {
        self.object.file_type() == elf::ElfType::Dyn
    }

    pub fn allocate_tls(&self, _hartid: usize, vmspace: &mut AddressSpace) -> usize {
        let Some(sgmt) = self.segments().find(|sgmt| sgmt.kind() == SegmentKind::Tls) else { return 0 };

        let tls_image = unsafe {
            let ptr = (vmspace.higher_half_start() as *mut u8)
                .add(pmm::alloc_frames(pages_for!(sgmt.mem_size())).unwrap());
            core::slice::from_raw_parts_mut(ptr, sgmt.mem_size())
        };

        tls_image[..sgmt.file_size()].copy_from_slice(sgmt.file_data());
        tls_image[sgmt.file_size()..].fill(0);

        tls_image.as_ptr().addr()
    }
}

/// Load an ELF executable into a virtual address space
///
/// This function loads the ELF executable `elf` into the virtual address space specified
/// by `vmspace`. If present, all relocations will be resolved and the Thread Local Storage
/// image will be initialized.
pub fn load_object<'elf>(
    elf: &'elf Elf,
    vmspace: &mut AddressSpace,
) -> Result<Rtld<'elf>, LoadError> {
    let load_headers = elf
        .segments()
        .filter(|sgmt| sgmt.kind() == SegmentKind::Load)
        .collect::<Vec<_>>();

    if load_headers.is_empty() {
        return Err(LoadError::NoSegments);
    }

    let mut link_base = usize::MAX;
    let mut link_end = usize::MIN;

    for sgmt in load_headers.iter() {
        let sgmt_start = sgmt.virtual_address() as usize;
        let sgmt_end = sgmt.virtual_address() as usize + sgmt.mem_size();

        if sgmt_start < vmspace.higher_half_start() {
            return Err(LoadError::LowerHalfSegment);
        }

        link_base = cmp::min(link_base, sgmt_start);
        link_end = cmp::max(link_end, sgmt_end);
    }

    // Now that we know where the object linked to and how big it is, we can allocate
    // memory for the image and decide whether or not to relocate it.

    let image_size = link_end - link_base;
    let image_base = pmm::alloc_frames(pages_for!(image_size)).unwrap();

    // `load_base` is where the object actually gets loaded,
    // KASLR would make this different from `link_base`.
    let load_base = link_base;

    // `reloc_base` is the difference between them, used for relocations.
    let reloc_base = load_base.wrapping_sub(link_base);

    // Copy each Load segment's data into the allocated image.
    for sgmt in load_headers {
        let virt_addr = reloc_base.wrapping_add(sgmt.virtual_address() as usize);
        let phys_addr = image_base + (virt_addr - load_base);
        let virt_page = page_align_down!(virt_addr);
        let phys_page = page_align_down!(phys_addr);

        unsafe {
            let sgmt_data = core::slice::from_raw_parts_mut(phys_addr as *mut u8, sgmt.mem_size());

            sgmt_data[..sgmt.file_size()].copy_from_slice(sgmt.file_data());
            sgmt_data[sgmt.file_size()..].fill(0);
        }

        vmspace
            .map_pages(
                virt_page,
                phys_page,
                page_align_up!(virt_addr + sgmt.mem_size()) - virt_page,
                sgmt.flags().into(),
            )
            .map_err(|err| match err {
                MapError::OverlappingMappings => LoadError::OverlappingSegments,
            })?;
    }

    let mut rtld_object = Rtld {
        object: elf,
        load_base,
        reloc_base,
        link_base,
        image_base,
        image_size,
        relocation_table: OnceCell::new(),
        has_ifuncs: false,
    };

    if rtld_object.is_relocatable() {
        resolve_relocations(&mut rtld_object);
    }

    if rtld_object.has_ifuncs {
        resolve_ifuncs(&mut rtld_object);
    }

    Ok(rtld_object)
}

fn resolve_relocations(object: &mut Rtld) {
    let Some(relocation_table) = object.relocation_table() else { return };

    for reloc_entry in relocation_table {
        let location = object.reloc_addr(reloc_entry.offset as usize);

        match reloc_entry.kind() {
            RelocKind::Relative => {
                let value = object.reloc_addr(reloc_entry.addend as usize);
                unsafe { *(location as *mut usize) = value };
            }
            RelocKind::Irelative => object.has_ifuncs = true,
            _ => panic!(),
        }
    }
}

type IfuncResolver = unsafe extern "C" fn() -> usize;

/// Resolve indirect function relocations
///
/// Relocating indirect functions is similar to normal relative relocations, except the
/// computed address instead points to a function in the loaded binary which will return
/// the actual address for the relocation.
///
/// Because the relocation is resolved by the object itself, all other relocations must
/// be resolved before these are.
fn resolve_ifuncs(object: &mut Rtld) {
    let Some(relocation_table) = object.relocation_table() else { return };

    for reloc_entry in relocation_table
        .iter()
        .filter(|r| r.kind() == RelocKind::Irelative)
    {
        let location = object.reloc_addr(reloc_entry.offset as usize) as *mut usize;
        let resolv = object.reloc_addr(reloc_entry.addend as usize) as *const IfuncResolver;
        unsafe { *location = (*resolv)() };
    }
}
