use crate::dev;

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
    if let Some(fdt) = dev::fdt::get_fdt() {
        return fdt
            .cpus()
            .map(|cpu| Cpu {
                hartid: cpu.ids().first(),
                processor_uid: 0,
            })
            .collect();
    }

    panic!();
}
