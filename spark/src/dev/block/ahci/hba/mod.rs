use core::cell::SyncUnsafeCell;
use libsa::{
    endian::{LittleEndianU16, LittleEndianU32, LittleEndianU64},
    volatile::{Volatile, VolatileSplitPtr},
};

#[repr(C)]
pub struct Memory {
    host_capability: Volatile<u32>,
    global_host_control: Volatile<u32>,
    interrupt_status: Volatile<u32>,
    ports_implemented: Volatile<u32>,
    version: Volatile<u32>,
    ccc_control: Volatile<u32>,
    ccc_ports: Volatile<u32>,
    enclosure_management_location: Volatile<u32>,
    enclosure_management_control: Volatile<u32>,
    host_capabilities_extended: Volatile<u32>,
    bios_handoff_control_status: Volatile<u32>,
    _reserved0: [u8; 0x74],
    _vendor0: [u8; 0x60],
    ports: [Port; 32],
}

impl Memory {
    pub fn iter_ports(&self) -> PortIterator {
        PortIterator {
            ports_implemented: self.ports_implemented.read(),
            ports: &self.ports,
            next_index: 0,
        }
    }
}

pub struct PortIterator<'a> {
    ports_implemented: u32,
    ports: &'a [Port],
    next_index: usize,
}

impl<'a> Iterator for PortIterator<'a> {
    type Item = &'a Port;

    fn next(&mut self) -> Option<Self::Item> {
        while (self.ports_implemented & (1 << self.next_index)) == 0 {
            self.next_index += 1;
        }

        if self.next_index < 32 {
            let cur_index = self.next_index;
            self.next_index += 1;
            self.ports.get(cur_index)
        } else {
            None
        }
    }
}

#[repr(C, packed)]
pub struct PRDT {
    data_addr: LittleEndianU64,
    _rsvd0: u32,
    bits: LittleEndianU32,
}

impl PRDT {
    const EMPTY: Self = Self {
        data_addr: LittleEndianU64::new(0),
        _rsvd0: 0,
        bits: LittleEndianU32::new(0),
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

#[repr(C, align(1024))]
pub struct Command {
    bits: LittleEndianU16,
    prd_table_len: LittleEndianU16,
    prd_byte_count: LittleEndianU32,
    cmd_tbl_address: LittleEndianU64,
}

#[repr(C, align(128))]
struct CommandTable {
    fis: HostToDevice,
    rsvd0: [u8; 128 - core::mem::size_of::<HostToDevice>()],
    prdt: [PRDT; 10],
}

static COMMAND: SyncUnsafeCell<Command> = SyncUnsafeCell::new(Command {
    bits: LittleEndianU16::new(0),
    prd_table_len: LittleEndianU16::new(0),
    prd_byte_count: LittleEndianU32::new(0),
    cmd_tbl_address: LittleEndianU64::new(0),
});

const COMMAND_TABLE_PRDT_COUNT: u16 = 10;
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
    prdt: [PRDT::EMPTY; COMMAND_TABLE_PRDT_COUNT as usize],
});

/// For the HBA to write received FISes. DO NOT read or write to this.
#[repr(C, align(256))]
struct FisReceived([u8; 256]);
static FIS_RECEIVED: SyncUnsafeCell<FisReceived> = SyncUnsafeCell::new(FisReceived([0u8; 256]));

#[repr(C)]
#[allow(dead_code)]
pub struct Port {
    cmd_list_ptr: VolatileSplitPtr<Command>,
    fis_list_ptr: VolatileSplitPtr<FisReceived>,
    int_status: Volatile<LittleEndianU32>,
    int_enable: Volatile<LittleEndianU32>,
    command_status: Volatile<LittleEndianU32>,
    _rsvd0: [u8; 4],
    task_file_data: Volatile<LittleEndianU32>,
    pub signature: Volatile<LittleEndianU32>,
    pub sata_status: Volatile<LittleEndianU32>,
    sata_control: Volatile<LittleEndianU32>,
    sata_error: Volatile<LittleEndianU32>,
    sata_active: Volatile<LittleEndianU32>,
    command_issue: Volatile<LittleEndianU32>,
    sata_notify: Volatile<LittleEndianU32>,
    fis_switch_control: Volatile<LittleEndianU32>,
    _rsvd1: [u8; 11],
    _vendor0: [u8; 4],
}

impl Port {
    const SECTOR_SIZE: usize = 512;

    pub const SATA_STATUS_READY: u32 = (1 << 8) | (3 << 0);
    pub const ATA_PORT_CLASS: u32 = 0x00000101;
    pub const ATA_DEV_BUSY: u8 = 0x80;
    pub const ATA_DEV_DRQ: u8 = 0x08;

    pub fn configure(&self) {
        const FRE: u32 = 4;
        const ST: u32 = 0;
        const FR: u32 = 14;
        const CR: u32 = 15;

        // Stop command processing.
        self.command_status.write(LittleEndianU32::new(
            self.command_status.read().get() & !(FRE | ST),
        ));
        while (self.command_status.read().get() & (FR | CR)) > 0 {
            core::hint::spin_loop();
        }

        self.cmd_list_ptr.set(COMMAND.get());
        self.fis_list_ptr.set(FIS_RECEIVED.get());

        // Restart command processing.
        while (self.command_status.read().get() & CR) > 0 {
            core::hint::spin_loop();
        }
        self.command_status.write(LittleEndianU32::new(
            self.command_status.read().get() | ST | FRE,
        ));
    }

    // SAFETY: This function assumes the port it belongs to is the only one being actively utilized.
    pub fn read(&self, sector_base: usize, buffer: &mut [u8]) {
        assert_eq!(
            (self.sata_status.read().get() & Self::SATA_STATUS_READY),
            Self::SATA_STATUS_READY,
            "AHCI device must be in a proper ready state"
        );
        assert!(
            self.signature.read().get() == Self::ATA_PORT_CLASS,
            "AHCI device is not a supported class"
        );

        // Wait for pending port tasks to complete.
        while (self.task_file_data.read().get() & ((Self::ATA_DEV_BUSY | Self::ATA_DEV_DRQ) as u32))
            > 0
        {
            core::hint::spin_loop();
        }

        // align to SECTOR_SIZE
        let sector_count = ((buffer.len() / Self::SECTOR_SIZE) * Self::SECTOR_SIZE) as u16;

        // Clear interrupts
        self.int_status.write(LittleEndianU32::new(0));

        // Get first command
        let command = unsafe { &mut *self.cmd_list_ptr.get() };
        command.bits = LittleEndianU16::new(
            (core::mem::size_of::<HostToDevice>() / core::mem::size_of::<u32>()) as u16,
        );
        command.cmd_tbl_address = LittleEndianU64::new(COMMAND_TBL.get() as usize as u64);
        command.prd_table_len = LittleEndianU16::new(COMMAND_TABLE_PRDT_COUNT);
        command.prd_byte_count = LittleEndianU32::new(0);

        let cmd_tbl = unsafe { &mut *COMMAND_TBL.get() };
        let fis = &mut cmd_tbl.fis;
        fis.lba0 = (sector_base >> 0) as u8;
        fis.lba1 = (sector_base >> 8) as u8;
        fis.lba2 = (sector_base >> 16) as u8;
        fis.lba3 = (sector_base >> 24) as u8;
        fis.lba4 = (sector_base >> 32) as u8;
        fis.lba5 = (sector_base >> 40) as u8;
        fis.sector_count_low = sector_count as u8;
        fis.sector_count_high = (sector_count >> 8) as u8;

        // TODO don't just assume 512b sector size, read from Identify packet
        let buffer_base = buffer.as_ptr() as usize;
        let prdts = &mut cmd_tbl.prdt;
        let sectors_per_prdt = ((2_usize.pow(21) * 2) / Self::SECTOR_SIZE) as u16;

        let mut buffer_offset = buffer_base;
        let mut remaining_sectors = sector_count;
        for prdt in prdts {
            let prdt_sector_count = core::cmp::min(sectors_per_prdt, remaining_sectors);
            let prdt_byte_count = (prdt_sector_count as usize) * Self::SECTOR_SIZE;

            prdt.data_addr = LittleEndianU64::new(buffer_offset as u64);
            prdt.bits = LittleEndianU32::new(prdt_byte_count as u32);

            remaining_sectors -= prdt_sector_count;
            buffer_offset += prdt_byte_count;

            if remaining_sectors == 0 {
                break;
            }
        }

        self.command_issue.write(LittleEndianU32::new(1));

        while (self.command_issue.read().get() & 1) > 0 {
            core::hint::spin_loop();

            // TODO check for errors
        }
    }
}

impl crate::dev::block::BlockDevice for Port {
    fn read(&self, address: usize, buffer: &mut [u8]) {
        // TODO don't just silently truncate the low address bits
        self.read(address & !(Self::SECTOR_SIZE - 1), buffer);
    }
}
