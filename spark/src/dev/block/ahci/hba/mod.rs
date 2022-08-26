use core::cell::SyncUnsafeCell;

use crate::mem::{VolatileCell, VolatileSplitPtr};

pub struct Memory {
    host_capability: VolatileCell<u32>,
    global_host_control: VolatileCell<u32>,
    interrupt_status: VolatileCell<u32>,
    ports_implemented: VolatileCell<u32>,
    version: VolatileCell<u32>,
    ccc_control: VolatileCell<u32>,
    ccc_ports: VolatileCell<u32>,
    enclosure_management_location: VolatileCell<u32>,
    enclosure_management_control: VolatileCell<u32>,
    host_capabilities_extended: VolatileCell<u32>,
    bios_handoff_control_status: VolatileCell<u32>,
    _reserved0: [u8; 0x74],
    _vendor0: [u8; 0x60],
    ports: [Port; 32],
}

impl Memory {
    pub fn ports_implemented(&self) -> [bool; 32] {
        let ports_impl = self.ports_implemented.read();
        let ports_impl_slice = [false; 32];

        for index in 0..32 {
            if (ports_impl & (1 << index)) > 0 {
                ports_impl_slice[index] = true;
            }
        }

        ports_impl_slice
    }
}

#[repr(C, packed)]
pub struct PRDT {
    data_addr: u64,
    _rsvd0: u32,
    bits: u32,
}

impl PRDT {
    const EMPTY: Self = Self {
        data_addr: 0,
        _rsvd0: 0,
        bits: 0,
    };
}

#[repr(C, packed)]
pub struct HostToDevice {
    ty: u8,
    bits1: u8,
    command: u8,
    feature_low: u8,
    lba0: u8,
    lba1: u8,
    lba2: u8,
    device: u8,
    lba3: u8,
    lba4: u8,
    lba5: u8,
    feature_high: u8,
    sector_count_low: u8,
    sector_count_high: u8,
    iso_cmd_compl: u8,
    control: u8,
    _rsvd0: [u8; 4],
}

#[repr(C, packed)]
pub struct Command {
    bits: u16,
    prd_table_len: u16,
    prd_byte_count: u32,
    cmd_tbl_addr: u64,
}

#[repr(C, align(128))]
struct CommandTable {
    fis: HostToDevice,
    rsvd0: [u8; 128 - core::mem::size_of::<HostToDevice>()],
    prdt: [PRDT; 10],
}

static COMMAND_TBL: SyncUnsafeCell<CommandTable> = SyncUnsafeCell::new(CommandTable {
    fis: HostToDevice {
        ty: 0x27,      // host to device
        bits1: 1 << 7, // `command` bit
        command: 0x25, // read DMA
        feature_low: 0,
        lba0: 0,
        lba1: 0,
        lba2: 0,
        device: 1 << 6, // LBA mode
        lba3: 0,
        lba4: 0,
        lba5: 0,
        feature_high: 0,
        sector_count_low: 0,
        sector_count_high: 0,
        iso_cmd_compl: 0,
        control: 0,
        _rsvd0: [0u8; 4],
    },
    rsvd0: [0u8; 128 - core::mem::size_of::<HostToDevice>()],
    prdt: [PRDT::EMPTY; 10],
});

pub struct Port {
    cmd_list_ptr: VolatileSplitPtr<Command>,
    fis_list_ptr: VolatileSplitPtr<HostToDevice>,
    int_status: VolatileCell<u32>,
    int_enable: VolatileCell<u32>,
    cmd_status: VolatileCell<u32>,
    _rsvd0: [u8; 4],
    task_file_data: VolatileCell<u32>,
    signature: VolatileCell<u32>,
    sata_status: VolatileCell<u32>,
    sata_control: VolatileCell<u32>,
    sata_error: VolatileCell<u32>,
    sata_active: VolatileCell<u32>,
    command_issue: VolatileCell<u32>,
    sata_notify: VolatileCell<u32>,
    fis_switch_control: VolatileCell<u32>,
    _rsvd1: [u8; 11],
    _vendor0: [u8; 4],
}

impl Port {
    // SAFETY: This function assumes the port it belongs to is the only one being actively utilized.
    pub unsafe fn read(
        &self,
        sector_base: usize,
        sector_count: core::num::NonZeroU16,
    ) -> &mut [u8] {
        const SATA_STATUS_READY: u32 = (1 << 8) | (3 << 0);
        const ATA_PORT_CLASS: u32 = 0x00000101;
        const ATA_DEV_BUSY: u8 = 0x80;
        const ATA_DEV_DRQ: u8 = 0x08;

        assert_eq!(
            (self.sata_status.read() & SATA_STATUS_READY),
            SATA_STATUS_READY,
            "AHCI device must be in a proper ready state"
        );
        assert!(
            self.signature.read() == ATA_PORT_CLASS,
            "AHCI device is not a supported class"
        );

        // Wait for pending port tasks to complete.
        while (self.task_file_data.read() & ((ATA_DEV_BUSY | ATA_DEV_DRQ) as u32)) > 0 {
            core::hint::spin_loop();
        }

        // Clear interrupts
        self.int_status.write(0);

        // Get first command
        let command = &mut *self.cmd_list_ptr.get_ptr_mut();
        command.bits = (core::mem::size_of::<HostToDevice>() / core::mem::size_of::<u32>()) as u16;
        command.cmd_tbl_addr = COMMAND_TBL.get() as usize as u64;
        command.prd_table_len = 1;
        command.prd_byte_count = 0;

        let cmd_tbl = &mut *COMMAND_TBL.get();
        let fis = &mut cmd_tbl.fis;
        fis.lba0 = (sector_base >> 0) as u8;
        fis.lba1 = (sector_base >> 8) as u8;
        fis.lba2 = (sector_base >> 16) as u8;
        fis.lba3 = (sector_base >> 24) as u8;
        fis.lba4 = (sector_base >> 32) as u8;
        fis.lba5 = (sector_base >> 40) as u8;
        fis.sector_count_low = sector_count.get() as u8;
        fis.sector_count_high = (sector_count.get() >> 8) as u8;

        // TODO don't just assume 512b sector size, read from Identify packet
        const SECTOR_SIZE: usize = 512;
        let buffer_base = crate::mem::pmm::alloc_frames(
            (((sector_count.get() as usize) * SECTOR_SIZE) + 0xFFF) / 0x1000,
        )
        .expect("out of memory allocating space for SATA read buffer");
        let prdts = &mut cmd_tbl.prdt;
        let sectors_per_prdt = ((2_usize.pow(21) * 2) / SECTOR_SIZE) as u16;

        let mut buffer_offset = buffer_base;
        let mut remaining_sectors = sector_count.get();
        for prdt in prdts {
            let prdt_sector_count = core::cmp::min(sectors_per_prdt, remaining_sectors);
            let prdt_byte_count = (prdt_sector_count as usize) * SECTOR_SIZE;

            prdt.data_addr = buffer_offset as u64;
            prdt.bits = prdt_byte_count as u32;

            remaining_sectors -= prdt_sector_count;
            buffer_offset += prdt_byte_count;

            if remaining_sectors == 0 {
                break;
            }
        }

        self.command_issue.write(1);

        while (self.command_issue.read() & 1) > 0 {
            core::hint::spin_loop();

            // TODO check for errors
        }

        core::slice::from_raw_parts_mut(
            buffer_base as *mut _,
            (sector_count.get() as usize) * SECTOR_SIZE,
        )
    }
}
