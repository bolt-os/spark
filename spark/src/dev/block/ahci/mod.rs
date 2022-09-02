use crate::dev::{
    pcie::{Device, DriverCompat},
    DeviceDriver,
};

pub mod hba;

#[used]
#[link_section = ".device_drivers"]
static AHCI_PCI_DRIVER: DeviceDriver = DeviceDriver {
    name: "ahci",
    fdt_compat: None,
    fdt_init: None,
    pci_compat: Some(&[&DriverCompat {
        class: 1,
        subclass: Some(6),
    }]),
    pci_init: Some(Ahci::from_pci_device),
};

#[allow(dead_code)]
pub struct Ahci<'a> {
    device: &'a Device,
    sata_ports: Vec<&'a hba::Port>,
}

impl Ahci<'_> {
    fn from_pci_device(device: &Device) {
        let pci_bar5 = device
            .bars()
            .nth(5)
            .expect("AHCI device does not have 5th BAR");

        let mut block_devices = super::BLOCK_DEVICES.lock();
        // # Safety: AHCI spec promises this is valid.
        let hba_mem = unsafe { (pci_bar5.read_addr() as *mut hba::Memory).as_mut() }.unwrap();

        hba_mem.iter_ports().for_each(|port| {
            if (port.sata_status.read().get() & hba::Port::SATA_STATUS_READY) > 0
                && port.signature.read().get() == hba::Port::ATA_PORT_CLASS
            {
                port.configure();
                block_devices.push(Box::new(AhciPort { port }));
            }
        });
    }
}

struct AhciPort<'a> {
    port: &'a hba::Port,
}

// # Safety: We're single threaded.
unsafe impl Send for AhciPort<'_> {}

impl super::BlockDevice for AhciPort<'_> {
    fn read(&self, address: usize, buffer: &mut [u8]) {
        assert_eq!(address & 0xFF, 0, "address must be sector-aligned");
        self.port.read(address, buffer);
    }
}
