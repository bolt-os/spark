#![cfg(feature = "acpi")]

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

pub fn init(rsdp: *mut u8) {
    unsafe {
        RSDP = Some(rsdp);
        ROOT = Some(::acpi::RootTable::new(rsdp, Bridge));
    }
}

pub fn get_rsdp() -> Option<*mut u8> {
    unsafe { RSDP }
}

pub fn get_table<T: ::acpi::Sdt>() -> Option<*const T> {
    let root = unsafe { ROOT.as_ref()? };
    root.get_table::<T>()
}
