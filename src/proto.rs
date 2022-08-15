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

use core::ffi::c_char;

use alloc::ffi::CString;
use limine::LiminePtr;

use crate::{rtld, size_of, vmm};

#[allow(clippy::enum_variant_names)]
enum Request<'a> {
    BootloaderInfoRequest(&'a limine::BootloaderInfoRequest),
    StackSizeRequest(&'a limine::StackSizeRequest),
    HhdmRequest(&'a limine::HhdmRequest),
    // Terminal
    FramebufferRequest(&'a limine::FramebufferRequest),
    // FiveLevelPaging
    SmpRequest(&'a limine::SmpRequest),
    MemoryMapRequest(&'a limine::MemoryMapRequest),
    // EntryPoint
    KernelFileRequest(&'a limine::KernelFileRequest),
    ModulesRequest(&'a limine::ModulesRequest),
    // Rsdp
    // SmBios
    // EfiSystemTable
    // Boot Time
    KernelAddressRequest(&'a limine::KernelAddressRequest),
}

fn leak_to_hhdm<T>(vmspace: &vmm::AddressSpace, b: Box<T>) -> *mut T {
    let ptr = Box::leak(b) as *mut T;
    ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
}

fn box_and_leak_to_hhdm<T>(vmspace: &vmm::AddressSpace, x: T) -> *mut T {
    leak_to_hhdm(vmspace, Box::new(x))
}

fn handle_request(
    request: Request,
    _hartid: usize,
    object: &rtld::Rtld,
    vmspace: &vmm::AddressSpace,
    _fdt: &fdt::Fdt,
) {
    match request {
        Request::BootloaderInfoRequest(req) => {
            let brand = {
                let s = CString::new("Spark").unwrap();
                let leaked = Box::leak(s.into_boxed_c_str());
                let ptr = leaked as *mut _ as *mut c_char;
                ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
            };

            let version = {
                let s = CString::new(env!("CARGO_PKG_VERSION")).unwrap();
                let leaked = Box::leak(s.into_boxed_c_str());
                let ptr = leaked as *mut _ as *mut c_char;
                ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
            };

            let response = box_and_leak_to_hhdm(vmspace, unsafe {
                limine::BootloaderInfo::new(
                    LiminePtr::new_unchecked(brand),
                    LiminePtr::new_unchecked(version),
                )
            });

            unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
        }
        Request::HhdmRequest(req) => {
            let response =
                box_and_leak_to_hhdm(vmspace, limine::Hhdm::new(vmspace.higher_half_start()));
            unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
        }
        Request::KernelAddressRequest(req) => {
            let response = {
                let r = limine::KernelAddress::new(object.image_base, object.load_base);
                let leaked = Box::leak(Box::new(r));
                let ptr = leaked as *mut limine::KernelAddress;
                ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
            };

            unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
        }
        _ => todo!(),
    }
}

pub fn handle_requests(
    hartid: usize,
    object: &rtld::Rtld,
    vmspace: &vmm::AddressSpace,
    fdt: &fdt::Fdt,
) {
    if let Some(sect) = object
        .sections()
        .find(|sect| sect.name() == Some(".limine_reqs"))
    {
        sect.table::<*mut [u64; 2]>()
            .iter()
            .map(|ptr| unsafe {
                assert!(ptr.read() == [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b]);
                let ident = ptr.add(1).read();

                macro_rules! match_request {
                    ($name:ident) => {
                        Request::$name(&*ptr.cast::<limine::$name>())
                    };
                }

                match ident {
                    [0xf55038d8e2a1202f, 0x279426fcf5f59740] => {
                        match_request!(BootloaderInfoRequest)
                    }
                    [0x224ef0460a8e8926, 0xe1cb0fc25f46ea3d] => match_request!(StackSizeRequest),
                    [0x48dcf1cb8ad2b852, 0x63984e959a98244b] => match_request!(HhdmRequest),
                    // Terminal
                    [0x9d5827dcd881dd75, 0xa3148604f6fab11b] => match_request!(FramebufferRequest),
                    // FiveLevelPaging
                    [0x95a67b819a1b857e, 0xa0b61b723b6a73e0] => match_request!(SmpRequest),
                    [0x67cf3d9d378a806f, 0xe304acdfc50c3c62] => match_request!(MemoryMapRequest),
                    // EntryPoint
                    [0xad97e90e83f1ed67, 0x31eb5d1c5ff23b69] => match_request!(KernelFileRequest),
                    [0x3e7e279702be32af, 0xca1c4f3bd1280cee] => match_request!(ModulesRequest),
                    // Rsdp
                    // SmBios
                    // EfiSystemTable
                    // BootTime
                    [0x71ba76863cc55f63, 0xb2644a48c516a487] => {
                        match_request!(KernelAddressRequest)
                    }
                    _ => todo!("{ident:#018x?}"),
                }
            })
            .for_each(|request| {
                handle_request(request, hartid, object, vmspace, fdt);
            });
    } else {
        let image = object.image_as_slice();

        let mut offset = image.as_ptr().align_offset(8);
        while offset + 7 < image.len() {
            let id0 = u64::from_le_bytes(image[offset..][..8].try_into().unwrap());
            if id0 != 0xc7b1dd30df4c8b88 {
                offset += 8;
                continue;
            }
            let id1 = u64::from_le_bytes(image[offset + 8..][..8].try_into().unwrap());
            if id1 != 0x0a82e883a194f07b {
                offset += 8;
                continue;
            }
            let ident = [
                u64::from_le_bytes(image[offset + 16..][..8].try_into().unwrap()),
                u64::from_le_bytes(image[offset + 24..][..8].try_into().unwrap()),
            ];

            macro_rules! match_request {
                ($name:ident) => {{
                    let req_size = size_of!(limine::$name);
                    let req =
                        unsafe { &*image[offset..][..req_size].as_ptr().cast::<limine::$name>() };
                    Request::$name(req)
                }};
            }

            let request = match ident {
                [0xf55038d8e2a1202f, 0x279426fcf5f59740] => match_request!(BootloaderInfoRequest),
                [0x224ef0460a8e8926, 0xe1cb0fc25f46ea3d] => match_request!(StackSizeRequest),
                [0x48dcf1cb8ad2b852, 0x63984e959a98244b] => match_request!(HhdmRequest),
                // Terminal
                [0x9d5827dcd881dd75, 0xa3148604f6fab11b] => match_request!(FramebufferRequest),
                // FiveLevelPaging
                [0x95a67b819a1b857e, 0xa0b61b723b6a73e0] => match_request!(SmpRequest),
                [0x67cf3d9d378a806f, 0xe304acdfc50c3c62] => match_request!(MemoryMapRequest),
                // EntryPoint
                [0xad97e90e83f1ed67, 0x31eb5d1c5ff23b69] => match_request!(KernelFileRequest),
                [0x3e7e279702be32af, 0xca1c4f3bd1280cee] => match_request!(ModulesRequest),
                // Rsdp
                // SmBios
                // EfiSystemTable
                // BootTime
                [0x71ba76863cc55f63, 0xb2644a48c516a487] => match_request!(KernelAddressRequest),
                _ => todo!("{ident:#018x?}"),
            };

            handle_request(request, hartid, object, vmspace, fdt);

            offset += 48;
        }
    }
}
