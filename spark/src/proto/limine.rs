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
    dev,
    fs::File,
    pmm::{self, MAX_PHYS_ADDR},
    rtld::Rtld,
    size_of, smp,
    vmm::{self, PagingMode},
    BOOT_HART_ID,
};
use alloc::ffi::CString;
use core::{cmp, ffi::c_char, sync::atomic::Ordering};
use elf::Elf;
use limine::{
    BootTime, BootTimeRequest, BootloaderInfo, BootloaderInfoRequest, Dtb, DtbRequest,
    EfiSystemTable, EfiSystemTableRequest, EntryPoint, EntryPointRequest, FramebufferRequest,
    Framebuffers, Hhdm, HhdmRequest, KernelAddress, KernelAddressRequest, KernelFile,
    KernelFileRequest, MemoryMap, MemoryMapRequest, Modules, ModulesRequest, PagingModeRequest,
    PagingModeResponse, PagingModeResponseFlags, Rsdp, RsdpRequest, Smbios, SmbiosRequest, Smp,
    SmpFlags, SmpInfo, SmpRequest, StackSize, StackSizeRequest, Uuid,
};
use memoffset::offset_of;

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

const REQUEST_ANCHOR: [usize; 2] = [0xc7b1dd30df4c8b88, 0x0a82e883a194f07b];
const DEFAULT_STACK_SIZE: usize = 64 * 1024;

#[derive(Default)]
struct Requests<'a> {
    count: usize,
    bootloader_info: Option<&'a BootloaderInfoRequest>,
    stack_size: Option<&'a StackSizeRequest>,
    direct_map: Option<&'a HhdmRequest>,
    framebuffer: Option<&'a FramebufferRequest>,
    smp: Option<&'a SmpRequest>,
    memory_map: Option<&'a MemoryMapRequest>,
    entry_point: Option<&'a EntryPointRequest>,
    kernel_file: Option<&'a KernelFileRequest>,
    modules: Option<&'a ModulesRequest>,
    rsdp: Option<&'a RsdpRequest>,
    smbios: Option<&'a SmbiosRequest>,
    efi_system_table: Option<&'a EfiSystemTableRequest>,
    boot_time: Option<&'a BootTimeRequest>,
    kernel_addr: Option<&'a KernelAddressRequest>,
    device_tree: Option<&'a DtbRequest>,
    paging_mode: Option<&'a PagingModeRequest>,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct RequestHeader {
    anchor: [usize; 2],
    ident: [usize; 2],
}

unsafe fn request_from_ptr(rtld: &Rtld, requests: &mut Requests, addr: usize, limine_reqs: bool) {
    let ptr = rtld.to_image_ptr(addr) as *const u8;
    let header = &*ptr.cast::<RequestHeader>();

    if header.anchor != REQUEST_ANCHOR {
        if limine_reqs {
            log::error!("ignoring `.limine_reqs` request ({addr:#x}) with invalid anchor");
        } else {
            unreachable!();
        }
    }

    fn check<'a, T: 'a>(rtld: &Rtld, req: &mut Option<&'a T>, new: &'a T, name: &str) -> bool {
        if let Some(chosen) = *req {
            let chosen = chosen as *const T as usize;
            let chosen = rtld.load_base() + (chosen - rtld.image_base);
            let new = new as *const T as usize;
            let new = rtld.load_base() + (new - rtld.image_base);
            log::warn!("ignoring duplicate request ({new:#x}) for {name}");
            log::warn!("using first request found ({chosen:#x})");
            false
        } else {
            *req = Some(new);
            true
        }
    }

    macro_rules! m {
        ($type:ty, $name:ident) => {{
            requests.count += check(
                rtld,
                &mut requests.$name,
                &*ptr.cast::<<$type as limine::Feature>::Request>(),
                stringify!($type),
            ) as usize;
        }};
    }
    match header.ident {
        [0xf55038d8e2a1202f, 0x279426fcf5f59740] => m!(BootloaderInfo, bootloader_info),
        [0x224ef0460a8e8926, 0xe1cb0fc25f46ea3d] => m!(StackSize, stack_size),
        [0x48dcf1cb8ad2b852, 0x63984e959a98244b] => m!(Hhdm, direct_map),
        [0x9d5827dcd881dd75, 0xa3148604f6fab11b] => m!(Framebuffers, framebuffer),
        [0x95a67b819a1b857e, 0xa0b61b723b6a73e0] => m!(Smp, smp),
        [0x67cf3d9d378a806f, 0xe304acdfc50c3c62] => m!(MemoryMap, memory_map),
        [0xad97e90e83f1ed67, 0x31eb5d1c5ff23b69] => m!(KernelFile, kernel_file),
        [0x3e7e279702be32af, 0xca1c4f3bd1280cee] => m!(Modules, modules),
        [0x71ba76863cc55f63, 0xb2644a48c516a487] => m!(KernelAddress, kernel_addr),
        [0x13d86c035a1cd3e1, 0x2b0caa89d8f3026a] => m!(EntryPoint, entry_point),
        [0xc5e77b6b397e7b43, 0x27637845accdcf3c] => m!(Rsdp, rsdp),
        [0x9e9046f11e095391, 0xaa4a520fefbde5ee] => m!(Smbios, smbios),
        [0x5ceba5163eaaf6d6, 0x0a6981610cf65fcc] => m!(EfiSystemTable, efi_system_table),
        [0x502746e184c088aa, 0xfbc5ec83e6327893] => m!(BootTime, boot_time),
        [0xb40ddb48fb54bac7, 0x545081493f81ffb7] => m!(Dtb, device_tree),
        [0x95c1a0edab0944cb, 0xa4e5cb3842f7488a] => m!(PagingModeResponse, paging_mode),
        [0x94469551da9b3192, 0xebe5e86db7382888] => {
            log::warn!("the 5-level paging request is x86_64-specific and will be ignored");
            log::warn!("use the paging mode request instead");
        }
        id => log::error!("unknown request detected: {id:016x?}"),
    }
}

/// Returns a list of the virtual addresses of the requests
fn get_request_pointers(rtld: &Rtld, requests: &mut Requests) {
    if let Some(section) = rtld.elf.find_section(".limine_reqs") {
        let limine_reqs = unsafe { section.table::<usize>() };
        for &addr in limine_reqs {
            unsafe { request_from_ptr(rtld, requests, addr, true) };
        }
    } else {
        // Scan the executable for requests. They will be at 8-byte alignments.
        for segment in rtld.elf.segments() {
            for (i, chunk) in segment
                .file_data()
                .array_windows::<16>()
                .enumerate()
                .step_by(8)
            {
                if *chunk == REQUEST_ANCHOR.map(usize::to_ne_bytes).flatten() {
                    let addr = segment.virtual_address() as usize + i;
                    unsafe { request_from_ptr(rtld, requests, addr, false) };
                }
            }
        }
    }
}

const _: () = {
    assert!(limine::PagingMode::Sv39 as u8 == PagingMode::Sv39 as u8);
    assert!(limine::PagingMode::Sv48 as u8 == PagingMode::Sv48 as u8);
    assert!(limine::PagingMode::Sv57 as u8 == PagingMode::Sv57 as u8);
};

impl From<limine::PagingMode> for PagingMode {
    fn from(value: limine::PagingMode) -> Self {
        match value {
            limine::PagingMode::Sv39 => PagingMode::Sv39,
            limine::PagingMode::Sv48 => PagingMode::Sv48,
            limine::PagingMode::Sv57 => PagingMode::Sv57,
        }
    }
}
impl From<PagingMode> for limine::PagingMode {
    fn from(value: PagingMode) -> Self {
        match value {
            PagingMode::Sv39 => limine::PagingMode::Sv39,
            PagingMode::Sv48 => limine::PagingMode::Sv48,
            PagingMode::Sv57 => limine::PagingMode::Sv57,
        }
    }
}

// fn ptr_to_hhdm<T: ?Sized>(vmspace: &vmm::AddressSpace, ptr: *mut T) -> *mut T {
//     vmspace.direct_map_ptr_mut(ptr)
// }

fn leak_hhdm_boxed<T: ?Sized>(vmspace: &vmm::AddressSpace, b: Box<T>) -> *mut T {
    vmspace.direct_map_ptr_mut(Box::leak(b))
}

fn leak_hhdm<T>(vmspace: &vmm::AddressSpace, val: T) -> *mut T {
    leak_hhdm_boxed(vmspace, Box::new(val))
}

fn leak_hhdm_vec<T>(vmspace: &vmm::AddressSpace, v: Vec<T>) -> *mut [T] {
    leak_hhdm_boxed(vmspace, v.into_boxed_slice())
}

fn leak_hhdm_cstr(vmspace: &vmm::AddressSpace, s: &str) -> *mut c_char {
    let cstring = Box::leak(CString::new(s).unwrap().into_boxed_c_str());
    vmspace.direct_map_ptr_mut(cstring.as_ptr().cast_mut())
}

#[allow(clippy::too_many_lines)]
pub fn main(mut fs: Box<dyn File>, config: &Entry) -> anyhow::Result<!> {
    let config = parse_config_entry(config);

    let kernel_path = config
        .kernel_path
        .strip_prefix("boot://")
        .expect("only `boot://` paths are currently supported");

    log::info!("loading kernel: {kernel_path}");

    let mut kernel_file = fs.open(kernel_path).expect("failed to open path");
    let kernel_data = kernel_file
        .read_to_end()
        .expect("failed to read kernel file")
        .into_boxed_slice();
    let kernel_elf = Elf::new(&kernel_data).unwrap();

    let mut rtld = Rtld::new(&kernel_elf).unwrap();
    rtld.load_image();

    let mut requests = Requests::default();
    get_request_pointers(&rtld, &mut requests);
    log::info!(
        "{} request{}",
        requests.count,
        if requests.count == 1 { "" } else { "s" }
    );

    let max_paging_mode = vmm::get_max_paging_mode();
    let mut paging_mode = PagingMode::Sv39;

    if let Some(req) = requests.paging_mode {
        let req_mode = req.mode.into();

        paging_mode = cmp::min(max_paging_mode, req_mode);
        if paging_mode != req.mode.into() {
            log::info!(
                "requested paging mode ({:?}) not supported, using {:?}",
                req_mode,
                paging_mode
            );
        }
    }

    let direct_map_base = if config.kaslr {
        log::warn!("todo: need entropy");
        paging_mode.higher_half_start()
    } else {
        paging_mode.higher_half_start()
    };

    let mut vmspace = vmm::AddressSpace::new(paging_mode, direct_map_base);
    let pmap_size = cmp::max(0x100000000, MAX_PHYS_ADDR.load(Ordering::Relaxed) + 1);

    log::info!("initializing {:?} paging.", vmspace.paging_mode());

    vmspace
        .map_pages(0, 0, pmap_size, vmm::MapFlags::RWX | vmm::MapFlags::HUGE1G)
        .expect("failed to create identity map");
    vmspace
        .map_pages(
            direct_map_base,
            0,
            pmap_size,
            vmm::MapFlags::RWX | vmm::MapFlags::HUGE1G,
        )
        .expect("failed to create higher-half map");

    rtld.map_image(&mut vmspace).unwrap();
    rtld.do_relocations();

    // Paging Mode
    if let Some(req) = requests.paging_mode {
        let resp = PagingModeResponse::new(paging_mode.into(), PagingModeResponseFlags::empty());
        unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
    }

    // Boot Info
    if let Some(req) = requests.bootloader_info {
        let brand = leak_hhdm_cstr(&vmspace, env!("CARGO_PKG_NAME"));
        let version = leak_hhdm_cstr(&vmspace, env!("CARGO_PKG_VERSION"));

        unsafe {
            let resp = BootloaderInfo::new(brand, version);
            req.set_response(leak_hhdm(&vmspace, resp));
        }
    }

    // Direct Map
    if let Some(req) = requests.direct_map {
        let resp = Hhdm::new(vmspace.direct_map_base());
        unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
    }

    // Device Tree
    if let Some(req) = requests.device_tree {
        let dtb = dev::fdt::DTB_PTR.load(Ordering::Relaxed);
        let resp = Dtb::new(vmspace.direct_map_ptr_mut(dtb));
        unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
    }

    // Framebuffer
    if let Some(_req) = requests.framebuffer {
        log::warn!("TODO: FramebufferRequest");
    }

    // RSDP
    #[cfg(all(feature = "acpi", uefi))]
    if let Some(req) = requests.rsdp {
        if let Some(ptr) = dev::acpi::get_rsdp() {
            let resp = limine::Rsdp::new(ptr);
            unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
        }
    }

    // SMBIOS
    if let Some(_req) = requests.smbios {
        log::warn!("TODO: SmbiosRequest");
    }

    // EFI System Table
    #[cfg(uefi)]
    if let Some(req) = requests.efi_system_table {
        let addr = uefi::system_table();
        let resp = limine::EfiSystemTable {
            revision: 0,
            addr: addr as *const _ as *mut u8,
        };
        unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
    }

    // Modules
    if let Some(req) = requests.modules {
        let mut modules = vec![];
        for module in config.modules {
            let path = module
                .path
                .strip_prefix("boot://")
                .expect("only `boot://` paths are currently supported");

            log::debug!("loading module `{path}`");

            let mut file = match fs.open(path) {
                Ok(file) => file,
                Err(err) => {
                    log::error!("failed to open module `{path}`: {err:?}");
                    continue;
                }
            };
            let data = match file.read_to_end() {
                Ok(data) => data.into_boxed_slice(),
                Err(err) => {
                    log::error!("failed to read module `{path}`: {err:?}");
                    continue;
                }
            };

            let data = leak_hhdm_boxed(&vmspace, data);
            let path = leak_hhdm_cstr(&vmspace, path);
            let cmdline = module.cmdline.map(|s| leak_hhdm_cstr(&vmspace, s));
            let file = unsafe {
                limine::File::new(
                    data.as_mut_ptr(),
                    data.len(),
                    path,
                    cmdline,
                    0,
                    0,
                    0,
                    0,
                    0,
                    Uuid::default(),
                    Uuid::default(),
                    Uuid::default(),
                )
            };

            modules.push(leak_hhdm(&vmspace, file));
        }

        let modules = leak_hhdm_vec(&vmspace, modules);
        unsafe {
            let resp = Modules::new(modules.as_mut_ptr(), modules.len());
            req.set_response(leak_hhdm(&vmspace, resp));
        }
    }

    // Entry Point
    let entry_point = if let Some(entry_point_req) = requests.entry_point {
        let resp = EntryPoint::new();
        unsafe { entry_point_req.set_response(leak_hhdm(&vmspace, resp)) };
        entry_point_req.entry as usize
    } else {
        rtld.reloc(rtld.elf.entry_point() as _)
    };

    // Kernel Address
    if let Some(req) = requests.kernel_addr {
        let resp = KernelAddress::new(rtld.image_base, rtld.load_base());
        unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
    }

    // Kernel File
    if let Some(req) = requests.kernel_file {
        let data = leak_hhdm_boxed(&vmspace, kernel_data);
        let path = leak_hhdm_cstr(&vmspace, kernel_path);
        let cmdline = config.cmdline.map(|s| leak_hhdm_cstr(&vmspace, s));
        unsafe {
            // XXX: Identifiers!!
            let file = limine::File::new(
                data.as_mut_ptr(),
                data.len(),
                path,
                cmdline,
                0,
                0,
                0,
                0,
                0,
                Uuid::default(),
                Uuid::default(),
                Uuid::default(),
            );
            let resp = KernelFile::new(leak_hhdm(&vmspace, file));
            req.set_response(leak_hhdm(&vmspace, resp));
        }
    }

    // Boot Time
    let boot_time_response = if let Some(req) = requests.boot_time {
        let phys_leaked = Box::leak(Box::new(BootTime::new(0)));
        unsafe { req.set_response(vmspace.direct_map_ptr_mut(phys_leaked)) };
        Some(phys_leaked)
    } else {
        None
    };

    // Stack Size
    let stack_size = if let Some(req) = requests.stack_size {
        let resp = StackSize::new();
        unsafe { req.set_response(leak_hhdm(&vmspace, resp)) };
        cmp::max(req.stack_size, DEFAULT_STACK_SIZE)
    } else {
        DEFAULT_STACK_SIZE
    };

    // fn get_global_ptr(elf: &Elf) -> Option<usize> {
    //     let symbol_table = elf.symbol_table()?;
    //     let symbol = symbol_table.find(|sym| sym.name() == Some("__global_pointer$"))?;
    //     Some(symbol.value() as usize)
    // }
    // let global_ptr = get_global_ptr(&kernel_elf).unwrap_or_default();
    let global_ptr = 0;

    // SMP
    if let Some(req) = requests.smp {
        let bsp_hartid = BOOT_HART_ID.load(Ordering::Relaxed);

        let mut cpus = vec![];
        for cpu in smp::cpus() {
            let smp::Cpu {
                processor_uid,
                hartid,
            } = cpu;

            if hartid == bsp_hartid {
                // Skip this hart.
                continue;
            }
            println!("smp candidate: hartid {hartid}");

            let info = leak_hhdm(&vmspace, SmpInfo::new(processor_uid, hartid));

            let stack = vec![0u8; stack_size].into_boxed_slice();
            let stack = Box::into_raw(stack);
            let stack = stack.cast::<u8>();
            let stack_ptr = stack.addr() + (stack_size - size_of!(ApPayload));
            unsafe {
                stack
                    .add(stack_size - size_of!(ApPayload))
                    .cast::<ApPayload>()
                    .write(ApPayload {
                        satp: vmspace.satp(),
                        gp: global_ptr,
                        sp: vmspace.direct_map_base() + stack.addr() + stack_size,
                        smp_info: info,
                    });
            };

            if let Err(err) = sbi::hsm::hart_start(hartid, ap_spinup as usize, stack_ptr) {
                log::error!("smp: failed to start ap (hartid = {hartid}): {err:?}");
                // XXX: Unleak `smp` and free it.
                continue;
            }

            cpus.push(info);
        }

        let cpus = leak_hhdm_vec(&vmspace, cpus);
        unsafe {
            let resp = Smp::new(SmpFlags::empty(), bsp_hartid, cpus.as_mut_ptr(), cpus.len());
            req.set_response(leak_hhdm(&vmspace, resp));
        }
    }

    let stack = leak_hhdm_boxed(&vmspace, vec![0u8; stack_size].into_boxed_slice());

    if let Some(_boot_time) = boot_time_response {
        log::warn!("TODO: boot time request");
    }

    // Drop all memory we can before generating the memory map.
    // TODO: Make this cleaner/generic.
    // XXX: This faults????
    // *crate::dev::block::DISKS.write() = vec![];
    drop(fs);

    // Memory Map
    println!("memor");
    if let Some(req) = requests.memory_map {
        let mut response = Box::<MemoryMap>::new_uninit();
        let memory_map = pmm::generate_limine_memory_map(&mut vmspace);
        println!("MEMOERY");
        unsafe {
            response.as_mut_ptr().write(memory_map);
            req.set_response(leak_hhdm_boxed(&vmspace, response.assume_init()));
        }
    }
    println!("done");

    // ===================================================
    //      NO MEMORY ALLOCATION AFTER THIS POINT !!!
    // ===================================================

    let stack_ptr = stack.addr() + stack_size;
    unsafe {
        vmspace.switch_to();
        spinup(entry_point, stack_ptr, global_ptr);
    }
}

macro_rules! zero_regs {
    () => {
        "
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
            mv      tp, zero
            mv      ra, zero
        "
    };
}

/// Pass off control to the kernel
///
/// All registers should be cleared to zero (we need to keep one to hold the jump address),
/// disable interrupts and clear the `stvec` CSR.
#[naked]
unsafe extern "C" fn spinup(entry_point: usize, stack_ptr: usize, global_ptr: usize) -> ! {
    asm!(
        "
            mv      t0, a0      // entry point
            mv      sp, a1      // stack pointer
            mv      gp, a2      // global pointer
            mv      a0, zero
        ",
        zero_regs!(),
        "
            csrci   sstatus, 0x2
            csrw    sie, zero
            csrw    stvec, zero
            csrw    sscratch, zero

            jr      t0
        ",
        options(noreturn)
    )
}

#[repr(C)]
struct ApPayload {
    satp: usize,
    gp: usize,
    sp: usize,
    smp_info: *mut SmpInfo,
}

#[naked]
unsafe extern "C" fn ap_spinup() -> ! {
    asm!(
        "
            ld      a0, {ap_smp_info}(a1)
            ld      sp, {ap_sp}(a1)
            ld      gp, {ap_gp}(a1)
            ld      t0, {ap_satp}(a1)
            csrw    satp, t0
            csrci   sstatus, 0x2 // SIE
            csrw    sie, zero
            csrw    stvec, zero
            csrw    sscratch, zero
        ",
        zero_regs!(),
        "
        1:
            // pause
            .insn i 0x0F, 0, x0, x0, 0x010
            fence   w, r
            ld      t0, 24(a0)
            beqz    t0, 1b
            jr      t0
            unimp
        ",
        ap_satp = const offset_of!(ApPayload, satp),
        ap_gp = const offset_of!(ApPayload, gp),
        ap_sp = const offset_of!(ApPayload, sp),
        ap_smp_info = const offset_of!(ApPayload, smp_info),
        options(noreturn)
    );
}

#[cfg(notyet)]
fn create_memory_map_and_exit_boot_services(vmspace: &vmm::AddressSpace) -> MemoryMap {
    //     pmm::exit_boot_services(|descriptor| {
    //
    //     });

    let boot_services = uefi::system_table().boot_services();
    let mut tries = 5;

    loop {
        let mut memory_map_size = 0;
        let mut map_key = 0;
        let mut descriptor_size = 0;
        let mut descriptor_version = 0;

        match (boot_services.get_memory_map)(
            &mut memory_map_size,
            ptr::null_mut(),
            &mut map_key,
            &mut descriptor_size,
            &mut descriptor_version,
        ) {
            Status::BUFFER_TOO_SMALL => {}
            status => status.to_result(()).unwrap(),
        }

        let num_entries = memory_map_size / descriptor_size;

        let limine_entries = Box::leak(Box::new_uninit_slice(num_entries));
        let limine_ptrs = Box::leak(Box::new_uninit_slice(num_entries));
        let buffer = Box::leak(Box::<[u8]>::new_uninit_slice(memory_map_size));

        (boot_services.get_memory_map)(
            &mut memory_map_size,
            buffer.as_mut_ptr().cast(),
            &mut map_key,
            &mut descriptor_size,
            &mut descriptor_version,
        )
        .to_result(())
        .unwrap();

        if let Err(status) =
            (boot_services.exit_boot_services)(uefi::image_handle(), map_key).to_result(())
        {
            if tries > 0 {
                tries -= 1;
                continue;
            }
            panic!("failed to exit boot services: {status:?}");
        }

        let buffer = unsafe { MaybeUninit::slice_assume_init_ref(buffer) };
        let mut offset = 0;
        let mut len = 0;
        loop {
            if offset + descriptor_size >= memory_map_size {
                break;
            }

            let uefi_entry = unsafe {
                &*buffer[offset..][..descriptor_size]
                    .as_ptr()
                    .cast::<MemoryDescriptor>()
            };
            let limine_entry = MemoryMapEntry::new(
                uefi_entry.phys as usize,
                uefi_entry.num_pages as usize * PAGE_SIZE,
                uefi_entry.kind.into(),
            );

            unsafe {
                let ptr = limine_entries.as_mut_ptr().add(len);
                ptr.write(MaybeUninit::new(limine_entry));
                limine_ptrs.as_mut_ptr().add(len).write(MaybeUninit::new(
                    vmspace.direct_map_ptr_mut(ptr.cast::<MemoryMapEntry>()),
                ));
            }

            len += 1;
            offset += descriptor_size;
        }

        unsafe {
            let ptr = MaybeUninit::slice_assume_init_mut(limine_ptrs).as_mut_ptr();
            return MemoryMap::new(ptr, len);
        }
    }
}
