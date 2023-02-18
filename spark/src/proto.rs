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

use alloc::ffi::CString;
use core::{ffi::c_char, sync::atomic::Ordering};
use limine::LiminePtr;

use crate::{rtld, vmm};

#[derive(Debug)]
pub enum LimineError {
    BadRequest(RequestError),
}

impl From<RequestError> for LimineError {
    fn from(err: RequestError) -> Self {
        Self::BadRequest(err)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum RequestError {
    Unrecognized,
    BadIdent,
    Alignment,
    Bounds,
}

#[derive(Clone, Copy)]
#[allow(clippy::enum_variant_names)]
enum Request<'a> {
    BootloaderInfoRequest(&'a limine::BootloaderInfoRequest),
    StackSizeRequest(&'a limine::StackSizeRequest),
    HhdmRequest(&'a limine::HhdmRequest),
    // Terminal
    FramebufferRequest(&'a limine::FramebufferRequest),
    FiveLevelPagingRequest(&'a limine::FiveLevelPagingRequest),
    SmpRequest(&'a limine::SmpRequest),
    MemoryMapRequest(&'a limine::MemoryMapRequest),
    EntryPointRequest(&'a limine::EntryPointRequest),
    KernelFileRequest(&'a limine::KernelFileRequest),
    ModulesRequest(&'a limine::ModulesRequest),
    RsdpRequest(&'a limine::RsdpRequest),
    SmbiosRequest(&'a limine::SmbiosRequest),
    EfiSystemTableRequest(&'a limine::EfiSystemTableRequest),
    BootTimeRequest(&'a limine::BootTimeRequest),
    KernelAddressRequest(&'a limine::KernelAddressRequest),
    DtbRequest(&'a limine::DtbRequest),
}

#[repr(transparent)]
#[derive(Clone, Copy)]
struct Ident([u64; 2]);

impl Request<'_> {
    /// Attempt to create a new `Request` from a pointer
    fn new_from_ptr<'a>(ptr: *const u8, object: &rtld::Rtld) -> Result<Request<'a>, RequestError> {
        #[repr(C)]
        #[derive(Clone, Copy, Debug)]
        struct RequestHeader {
            anchor: [u64; 2],
            ident: [u64; 2],
        }

        #[allow(clippy::cast_ptr_alignment)] // Alignment is checked below.
        let header_ptr = ptr.cast::<RequestHeader>();

        if !header_ptr.is_aligned() {
            return Err(RequestError::Alignment);
        }
        if !object.check_image_ptr(header_ptr) {
            return Err(RequestError::Bounds);
        }

        // SAFETY: We checked that the pointer is aligned and in bounds.
        let header = unsafe { header_ptr.read() };

        if header.anchor != [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b] {
            return Err(RequestError::BadIdent);
        }

        macro_rules! match_request {
            ($name:ident) => {{
                let ptr = ptr.cast::<limine::$name>();

                if !ptr.is_aligned() {
                    return Err(RequestError::Alignment);
                }
                if !object.check_image_ptr(ptr) {
                    return Err(RequestError::Bounds);
                }

                // SAFETY: We checked that the pointer is *still* aligned and within bounds
                // after being casted to the concrete type.
                Request::$name(unsafe { &*ptr })
            }};
        }

        Ok(match header.ident {
            [0xf55038d8e2a1202f, 0x279426fcf5f59740] => match_request!(BootloaderInfoRequest),
            [0x224ef0460a8e8926, 0xe1cb0fc25f46ea3d] => match_request!(StackSizeRequest),
            [0x48dcf1cb8ad2b852, 0x63984e959a98244b] => match_request!(HhdmRequest),
            [0x9d5827dcd881dd75, 0xa3148604f6fab11b] => match_request!(FramebufferRequest),
            [0x95a67b819a1b857e, 0xa0b61b723b6a73e0] => match_request!(SmpRequest),
            [0x67cf3d9d378a806f, 0xe304acdfc50c3c62] => match_request!(MemoryMapRequest),
            [0xad97e90e83f1ed67, 0x31eb5d1c5ff23b69] => match_request!(KernelFileRequest),
            [0x3e7e279702be32af, 0xca1c4f3bd1280cee] => match_request!(ModulesRequest),
            [0x71ba76863cc55f63, 0xb2644a48c516a487] => match_request!(KernelAddressRequest),
            [0xc8ac59310c2b0844, 0xa68d0c7265d38878] => todo!("TerminalRequest"),
            [0x94469551da9b3192, 0xebe5e86db7382888] => match_request!(FiveLevelPagingRequest),
            [0x13d86c035a1cd3e1, 0x2b0caa89d8f3026a] => match_request!(EntryPointRequest),
            [0xc5e77b6b397e7b43, 0x27637845accdcf3c] => match_request!(RsdpRequest),
            [0x9e9046f11e095391, 0xaa4a520fefbde5ee] => match_request!(SmbiosRequest),
            [0x5ceba5163eaaf6d6, 0x0a6981610cf65fcc] => match_request!(EfiSystemTableRequest),
            [0x502746e184c088aa, 0xfbc5ec83e6327893] => match_request!(BootTimeRequest),
            [0xb40ddb48fb54bac7, 0x545081493f81ffb7] => match_request!(DtbRequest),
            _ => {
                println!(
                    "limine: unrecognized request: {:#018x}, {:#018x}",
                    header.ident[0], header.ident[1]
                );
                return Err(RequestError::Unrecognized);
            }
        })
    }
}

fn report_error(err: RequestError) {
    match err {
        RequestError::BadIdent => todo!(),
        RequestError::Alignment => todo!(),
        _ => todo!(),
    }
}

fn get_requests_from_section<'a>(object: &rtld::Rtld) -> Option<Vec<Request<'a>>> {
    let mut requests = vec![];

    for &ptr in unsafe { object.find_section(".limine_reqs")?.table::<*mut u8>() } {
        let ptr = ptr.with_addr(object.image_base + (ptr.addr() - object.load_base));
        match Request::new_from_ptr(ptr, object) {
            Ok(req) => requests.push(req),
            Err(RequestError::Unrecognized) => { /* ignore unrecognized requests */ }
            Err(err) => report_error(err),
        }
    }

    Some(requests)
}

fn get_requests_from_image<'a>(object: &rtld::Rtld) -> Vec<Request<'a>> {
    let mut requests = vec![];
    let image = object.image_as_slice();

    let mut offset = image.as_ptr().align_offset(8);
    while offset + 32 <= image.len() {
        let ptr = image[offset..].as_ptr();

        match Request::new_from_ptr(ptr, object) {
            Ok(req) => requests.push(req),
            Err(RequestError::BadIdent) => { /* not a request */ }
            Err(err) => report_error(err),
        }

        offset += 8;
    }

    requests
}

fn leak_to_hhdm<T>(vmspace: &vmm::AddressSpace, b: Box<T>) -> *mut T {
    let ptr = Box::leak(b) as *mut T;
    ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
}

fn box_and_leak_to_hhdm<T>(vmspace: &vmm::AddressSpace, x: T) -> *mut T {
    leak_to_hhdm(vmspace, Box::new(x))
}

pub fn handle_requests(
    hartid: usize,
    object: &rtld::Rtld,
    vmspace: &vmm::AddressSpace,
    fdt: &fdt::Fdt,
) {
    let requests =
        get_requests_from_section(object).unwrap_or_else(|| get_requests_from_image(object));

    log::info!(
        "{} request{}",
        requests.len(),
        if requests.len() == 1 { "" } else { "s" },
    );

    for request in requests {
        use Request::*;

        match request {
            BootloaderInfoRequest(req) => {
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
            HhdmRequest(req) => {
                let response =
                    box_and_leak_to_hhdm(vmspace, limine::Hhdm::new(vmspace.higher_half_start()));
                unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
            }
            KernelAddressRequest(req) => {
                let response = {
                    let r = limine::KernelAddress::new(object.image_base, object.load_base);
                    let leaked = Box::leak(Box::new(r));
                    let ptr = leaked as *mut limine::KernelAddress;
                    ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
                };

                unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
            }
            DtbRequest(req) => {
                let response = leak_to_hhdm(
                    vmspace,
                    Box::new(limine::Dtb::new(crate::DTB_PTR.load(Ordering::Relaxed))),
                );

                unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
            }
            _ => todo!(),
        }
    }
}
