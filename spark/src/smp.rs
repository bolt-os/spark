// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

#[cfg(uefi)]
use crate::dev;
#[cfg(feature = "fdt")]
use crate::sys::fdt;

pub struct Cpu {
    pub hartid: usize,
    pub processor_uid: u32,
}

pub fn cpus() -> Vec<Cpu> {
    #[cfg(all(feature = "acpi", uefi))]
    if let Some(madt) = dev::acpi::get_table::<acpi::madt::Madt>() {
        return unsafe {
            acpi::madt::iter_madt(madt)
                .filter_map(|entry| match entry {
                    acpi::madt::Entry::RiscvIntc {
                        processor_uid,
                        hartid,
                        ..
                    } => Some(Cpu {
                        processor_uid,
                        hartid: hartid as usize,
                    }),
                    _ => None,
                })
                .collect()
        };
    }

    #[cfg(feature = "fdt")]
    if let Some(fdt) = fdt::try_get_fdt() {
        let mut cpus = vec![];
        for node in fdt.cpus() {
            let Ok(reg) = node.reg_by_index(0) else {
                continue;
            };
            cpus.push(Cpu {
                hartid: reg.addr as _,
                processor_uid: 0,
            });
        }
        return cpus;
    }

    panic!();
}
