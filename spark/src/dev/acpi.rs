#![cfg(all(feature = "acpi", uefi))]

use uefi::table::TableGuid;

static mut RSDP: Option<*mut u8> = None;
static mut ROOT: Option<::acpi::RootTable<Bridge>> = None;

#[derive(Clone, Copy, Debug)]
struct Bridge;

impl ::acpi::Bridge for Bridge {
    fn map(&self, phys: usize, _size: usize) -> usize {
        phys
    }

    fn remap(&self, virt: usize, _new_size: usize) -> usize {
        virt
    }

    fn unmap(&self, _virt: usize) {}
}

pub fn init() {
    let config_table = uefi::system_table().config_table();
    let Some(root_ptr) = config_table
        .get_table(TableGuid::ACPI_20)
        .or_else(|| config_table.get_table(TableGuid::ACPI))
        else { return };

    unsafe {
        let ptr = root_ptr.cast();
        RSDP = Some(ptr);
        ROOT = Some(::acpi::RootTable::new(ptr, Bridge));
    }
}

pub fn get_rsdp() -> Option<*mut u8> {
    unsafe { RSDP }
}

pub fn get_table<T: ::acpi::Sdt>() -> Option<*const T> {
    let root = unsafe { ROOT.as_ref().unwrap() };
    root.get_table::<T>()
}
