pub mod ahci;

pub trait BlockDevice: Send {
    fn read(&self, address: usize, buffer: &mut [u8]);
}

pub static BLOCK_DEVICES: spin::Mutex<Vec<Box<dyn BlockDevice>>> = spin::Mutex::new(Vec::new());

pub fn register_block_device(dev: Box<dyn BlockDevice>) {
    let mut devices = BLOCK_DEVICES.lock();
    devices.push(dev);
}
