pub mod ahci;

pub trait BlockDevice: Send {
    fn read(&self, address: usize, buffer: &mut [u8]);
}

pub static BLOCK_DEVICES: spin::Mutex<Vec<Box<dyn BlockDevice>>> = spin::Mutex::new(Vec::new());
