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
    config::{Entry, Value},
    fs::File,
    rtld::Rtld,
    vmm,
};
use alloc::ffi::CString;
use anyhow::Result;
use core::ffi::c_char;
use elf::Elf;
use limine::LiminePtr;

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
    fn new_from_ptr<'a>(ptr: *const u8) -> Result<Request<'a>, RequestError> {
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

fn leak_to_hhdm<T: ?Sized>(vmspace: &vmm::AddressSpace, b: Box<T>) -> *mut T {
    let ptr = Box::leak(b) as *mut T;
    ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
}

fn box_and_leak_to_hhdm<T>(vmspace: &vmm::AddressSpace, x: T) -> *mut T {
    leak_to_hhdm(vmspace, Box::new(x))
}

fn handle_requests(object: &mut Rtld, vmspace: &vmm::AddressSpace, requests: &[Request<'static>]) {
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
                log::info!("HHDM");
                let response =
                    box_and_leak_to_hhdm(vmspace, limine::Hhdm::new(vmspace.higher_half_start()));
                unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
            }
            KernelAddressRequest(req) => {
                log::info!("KERN AADDR");
                let response = {
                    let r = limine::KernelAddress::new(object.image_base, object.load_base());
                    let leaked = Box::leak(Box::new(r));
                    let ptr = leaked as *mut limine::KernelAddress;
                    ptr.with_addr(vmspace.higher_half_start() + ptr.addr())
                };

                unsafe { req.set_response(LiminePtr::new_unchecked(response)) };
            }

            _req => {}
        }
    }
}

const COMMON_REQUEST_MAGIC: [u8; 16] = [
    0x88, 0x8b, 0x4c, 0xdf, 0x30, 0xdd, 0xb1, 0xc7, 0x7b, 0xf0, 0x94, 0xa1, 0x83, 0xe8, 0x82, 0x0a,
];

/// Returns a list of the virtual addresses of the requests
fn get_request_pointers(object: &Rtld) -> Vec<Request<'static>> {
    if let Some(section) = object.elf.find_section(".limine_reqs") {
        // SAFETY: It be what it be, we have no way to validate this.
        unsafe { section.table::<usize>() }
            .iter()
            .map(|addr| {
                let addr = object.to_image_ptr(*addr);
                Request::new_from_ptr(addr as *const u8).unwrap()
            })
            .collect()
    } else {
        let mut requests = vec![];
        for segment in object.elf.segments() {
            for (i, chunk) in segment.file_data().array_chunks::<16>().enumerate() {
                if *chunk == COMMON_REQUEST_MAGIC {
                    let addr = segment.virtual_address() as usize + i * 16;
                    requests.push(
                        Request::new_from_ptr(object.to_image_ptr(addr) as *const _).unwrap(),
                    );
                }
            }
        }
        requests
    }
}

pub fn main(mut fs: Box<dyn File>, config: &Entry) -> anyhow::Result<!> {
    let config = parse_config_entry(config);

    let kernel_path = config
        .kernel_path
        .strip_prefix("boot://")
        .expect("only `boot://` paths are currently supported");
    let kernel_data = fs
        .open(kernel_path)
        .expect("failed to open path")
        .read_to_end()
        .expect("failed to read kernel file");
    let kernel_elf = Elf::new(&kernel_data).unwrap();

    let mut rtld = Rtld::new(&kernel_elf).unwrap();
    rtld.load_image();

    let requests = get_request_pointers(&rtld);
    let want_5lvl_paging = requests
        .iter()
        .any(|req| matches!(&req, Request::FiveLevelPagingRequest(_)));

    let mut vmspace = vmm::init_from_fdt(want_5lvl_paging);
    rtld.map_image(&mut vmspace).unwrap();
    rtld.do_relocations();

    handle_requests(&mut rtld, &vmspace, &requests);

    let stack = leak_to_hhdm(&vmspace, vec![0u8; 64 * 1024].into_boxed_slice());
    let stack_ptr = stack.addr() + 64 * 1024;
    log::info!("SPINUP");

    fn get_global_ptr(elf: &Elf) -> Option<usize> {
        let symbol_table = elf.symbol_table()?;
        let symbol = symbol_table.find(|sym| sym.name() == Some("__global_pointer$"))?;
        Some(symbol.value() as usize)
    }

    unsafe {
        vmspace.switch_to();
        spinup(
            rtld.reloc(rtld.elf.entry_point() as _),
            stack_ptr,
            get_global_ptr(&kernel_elf).unwrap_or_default(),
            0,
        );
    }
}

#[derive(Debug)]
pub struct ConfigEntry<'src> {
    pub kernel_path: &'src str,
    pub kaslr: bool,
    pub cmdline: Option<&'src str>,
    pub modules: Vec<ModuleConfigEntry<'src>>,
}

#[derive(Debug)]
pub struct ModuleConfigEntry<'src> {
    pub path: &'src str,
    pub cmdline: Option<&'src str>,
}

fn parse_config_entry<'src>(boot_entry: &Entry<'src>) -> ConfigEntry<'src> {
    let mut kernel_path = None;
    let mut kaslr = false;
    let mut cmdline = None;
    let mut modules = vec![];

    for param in &boot_entry.params {
        match param.key {
            "kernel-path" => match param.value {
                Value::String(path) => kernel_path = Some(path),
                _ => panic!(),
            },
            "cmdline" => match param.value {
                Value::String(s) => cmdline = Some(s),
                _ => panic!(),
            },
            "kaslr" => match param.value {
                Value::Bool(value) => kaslr = value,
                _ => panic!(),
            },
            "protocol" => {}
            _ => panic!(),
        }
    }

    for entry in &boot_entry.entries {
        assert!(entry.key == "module");

        let mut module_path = None;
        for param in &entry.params {
            match param.key {
                "path" => match param.value {
                    Value::String(path) => module_path = Some(path),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        }

        modules.push(ModuleConfigEntry {
            path: module_path.unwrap(),
            cmdline: entry.name,
        });
    }

    ConfigEntry {
        kernel_path: kernel_path.unwrap(),
        kaslr,
        cmdline,
        modules,
    }
}

/// Pass off control to the kernel
///
/// All registers should be cleared to zero (we need to keep one to hold the jump address),
/// disable interrupts and clear the `stvec` CSR.
#[naked]
unsafe extern "C" fn spinup(
    entry_point: usize,
    stack_ptr: usize,
    global_ptr: usize,
    tp: usize,
) -> ! {
    asm!(
        r#"
            mv      t0, a0      // entry point
            mv      sp, a1      // stack pointer
            mv      gp, a2      // global pointer
            mv      tp, a3      // thread pointer

            mv      a0, zero
            mv      a1, zero
            mv      a2, zero
            mv      a3, zero
            mv      a4, zero
            mv      a5, zero
            mv      a6, zero
            mv      a7, zero
            mv      s0, zero
            mv      s1, zero
            mv      s2, zero
            mv      s3, zero
            mv      s4, zero
            mv      s5, zero
            mv      s6, zero
            mv      s7, zero
            mv      s8, zero
            mv      s9, zero
            mv      s10, zero
            mv      s11, zero
            mv      t1, zero
            mv      t2, zero
            mv      t3, zero
            mv      t4, zero
            mv      t5, zero
            mv      t6, zero

            mv      ra, zero

            csrci   sstatus, 0x2
            csrw    sie, zero
            csrw    stvec, zero
            csrw    sscratch, zero

            jr      t0
        "#,
        options(noreturn)
    )
}
