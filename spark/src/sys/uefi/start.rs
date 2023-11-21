// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use crate::{dev, io, sys::fdt, BOOT_HART_ID};
use core::sync::atomic::Ordering;
use uefi::{
    proto::riscv::RiscvBoot,
    table::{SystemTable, TableGuid},
    Handle,
};

global_asm!(include_str!("locore.s"), options(raw));

global_asm!(
    r#"
    .section .data.sbat
    sbat:
        .ascii  "sbat,1,SBAT Version,sbat,1,https://github.com/rhboot/shim/blob/main/SBAT.md\n"
    "#,
    concat!(
        r#".ascii  "spark,1,Spark,spark,"#,
        env!("CARGO_PKG_VERSION"),
        r#",https://github.com/bolt-os/spark\n""#,
    ),
    "__sbat_endv:",
);

#[no_mangle]
extern "C" fn spark_main(image: Handle, system_table: &'static SystemTable) -> ! {
    unsafe { uefi::bootstrap(image, system_table) };

    io::init();
    println!();

    // Print the address we've been loaded to for easier debugging.
    let image_base: usize;
    unsafe {
        asm!("lla {}, __image_base", out(reg) image_base, options(nomem, nostack));
    }
    log::debug!("image base: {image_base:#x}");

    let boot_services = uefi::boot_services();

    let mut riscv_boot_proto = boot_services
        .first_protocol::<RiscvBoot>()
        .expect("risc-v boot protocol is not available");

    let hartid = riscv_boot_proto
        .get_boot_hartid()
        .expect("failed to get bsp's hart id");
    BOOT_HART_ID.store(hartid, Ordering::Relaxed);

    let config_table = system_table.config_table();

    if let Some(ptr) = config_table.get_table(TableGuid::ACPI_20) {
        dev::acpi::init(ptr.cast());
    } else if let Some(ptr) = config_table.get_table(TableGuid::ACPI) {
        dev::acpi::init(ptr.cast());
    }

    if let Some(ptr) = config_table.get_table(TableGuid::DEVICE_TREE) {
        unsafe { fdt::init(ptr.cast()) };
    }

    dev::init();

    crate::main();
}
