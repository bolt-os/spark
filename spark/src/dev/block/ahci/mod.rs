pub mod hba;

pub struct Ahci<'a> {
    device: &'a crate::dev::pcie::,
    sata_ports: Vec<&'a hba::Port>
}