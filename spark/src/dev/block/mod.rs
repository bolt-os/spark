/*
 * Copyright (c) 2022-2023 xvanc and contributors
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

use crate::{io, size_of};
use alloc::sync::{Arc, Weak};
use core::{cmp, fmt::Debug};
use spin::{mutex::SpinMutex, RwLock};
use uuid::Uuid;

pub mod ahci;

/// Devices which provide a block-oriented interface
pub trait BlockIo: Send + Sync + Debug {
    /// Returns the logical block size of the device, in bytes.
    fn block_size(&self) -> u64;

    fn capacity(&self) -> u64;

    /// Read blocks from the device.
    ///
    /// # Errors
    ///
    /// The length of `buf` must be a multiple of the device's [`block_size`](BlockIo::block_size).
    fn read_blocks(&self, lba: u64, buf: &mut [u8]) -> io::Result<()>;

    /// Returns the device's UUID, if any.
    fn uuid(&self) -> Option<Uuid> {
        None
    }

    fn read(&self, mut offset: u64, buf: &mut [u8]) -> io::Result<usize> {
        static LOCAL_BUF: SpinMutex<Vec<u8>> = SpinMutex::new(vec![]);

        let block_size = self.block_size();

        let mut local_buf = LOCAL_BUF.lock();
        let cur_len = local_buf.len();
        local_buf.resize(cur_len.max(block_size as usize), 0);

        let mut buf_offset = 0;
        let mut count = buf.len() as u64;

        // Read leading
        let block_offset = offset & (block_size - 1);
        if block_offset > 0 {
            let read = cmp::min(count, block_size - block_offset);
            self.read_blocks(offset / block_size, &mut local_buf)?;
            buf[..read as usize]
                .copy_from_slice(&local_buf[block_offset as usize..][..read as usize]);
            offset += read;
            buf_offset += read;
            count -= read;
            if count == 0 {
                return Ok(buf.len() - count as usize);
            }
        }

        // Read full
        let blocks = count / block_size;
        if blocks > 0 {
            let bytes = blocks * block_size;
            self.read_blocks(
                offset / block_size,
                &mut buf[buf_offset as usize..][..bytes as usize],
            )?;
            offset += bytes;
            buf_offset += bytes;
            count -= bytes;
        }

        // Read trailing
        if count > 0 {
            assert!(count < block_size);
            self.read_blocks(offset / block_size, &mut local_buf)?;
            buf[buf_offset as usize..].copy_from_slice(&local_buf[..count as usize]);
        }

        Ok(buf.len())
    }
}

#[derive(Debug)]
pub struct Disk {
    device: Box<dyn BlockIo>,
    volumes: Vec<Arc<Volume>>,
    disk_guid: Option<Uuid>,
}

impl Disk {
    pub fn volumes(&self) -> &[Arc<Volume>] {
        &self.volumes
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PartitionType {
    Mbr(u8),
    Gpt(Uuid),
}

#[derive(Debug)]
pub struct Volume {
    disk: Weak<Disk>,
    block_size: u64,
    offset: u64,
    capacity: u64,
    uuid: Option<Uuid>,
    partition_type: PartitionType,
    partition_guid: Option<Uuid>,
}

unsafe impl Sync for Volume {}

impl BlockIo for Volume {
    fn read_blocks(&self, lba: u64, buf: &mut [u8]) -> crate::io::Result<()> {
        if lba + ((buf.len() as u64 + self.block_size - 1) / self.block_size) >= self.capacity {
            return Err(io::Error::OutOfBounds);
        }
        self.disk
            .upgrade()
            .unwrap()
            .device
            .read_blocks(self.offset + lba, buf)
    }

    fn block_size(&self) -> u64 {
        self.block_size
    }

    fn capacity(&self) -> u64 {
        self.capacity
    }

    fn uuid(&self) -> Option<uuid::Uuid> {
        self.uuid
    }
}

pub static DISKS: RwLock<Vec<Arc<Disk>>> = RwLock::new(vec![]);

pub fn register(device: Box<dyn BlockIo>) -> io::Result<()> {
    let block_size = device.block_size();
    let mut sector = vec![0; block_size as usize];

    let mut volumes = vec![];
    let mut disk = Arc::new(Disk {
        device,
        volumes: vec![],
        disk_guid: None,
    });

    let disk_guid = 'probe: {
        'mbr: {
            disk.device.read_blocks(0, &mut sector)?;

            if sector[510..512] != [0x55, 0xaa] {
                // Invalid MBR, assume raw media.
                break 'probe None;
            }

            let partition_table = unsafe {
                sector[0x1be..][..size_of!([MbrPartitionEntry; 4])]
                    .as_ptr()
                    .cast::<[MbrPartitionEntry; 4]>()
                    .read_unaligned()
            };
            for entry in partition_table {
                match entry.partition_type {
                    0x00 => continue, // empty partition
                    0xee => {
                        // GPT
                        assert!(volumes.is_empty());
                        break 'mbr;
                    }
                    _ => {}
                }

                volumes.push(Arc::new(Volume {
                    disk: Arc::downgrade(&disk),
                    block_size,
                    offset: entry.lba_start as u64,
                    capacity: entry.lba_size as u64,
                    uuid: None,
                    partition_type: PartitionType::Mbr(entry.partition_type),
                    partition_guid: None,
                }));
            }

            break 'probe None;
        }

        // Scan GPT
        {
            disk.device.read_blocks(1, &mut sector)?;
            let gpt_header = unsafe {
                sector[..size_of!(GptHeader)]
                    .as_ptr()
                    .cast::<GptHeader>()
                    .read()
            };

            let mut lba = gpt_header.partition_table_lba;
            let mut offset = 0;
            let mut index = 0;

            disk.device.read_blocks(lba, &mut sector)?;
            loop {
                if index >= gpt_header.partition_entries as usize {
                    break;
                }
                if offset + size_of!(GptPartitionEntry) > block_size as usize {
                    offset = 0;
                    lba += 1;
                    disk.device.read_blocks(lba, &mut sector)?;
                }

                let entry = unsafe {
                    &*sector[offset..][..size_of!(GptPartitionEntry)]
                        .as_ptr()
                        .cast::<GptPartitionEntry>()
                };
                offset += gpt_header.partition_entry_size as usize;
                index += 1;

                if entry.partition_type == PARTITION_TYPE_UNUSED {
                    continue;
                }

                volumes.push(Arc::new(Volume {
                    disk: Arc::downgrade(&disk),
                    block_size,
                    offset: entry.start_lba,
                    capacity: (entry.end_lba - entry.start_lba) + 1,
                    uuid: Some(entry.partition_uuid),
                    partition_type: PartitionType::Gpt(entry.partition_type),
                    partition_guid: None,
                }));
            }

            Some(gpt_header.disk_guid)
        }
    };

    {
        // SAFETY: We hold the only strong reference and none of the `Weak`s in the `Volume`s
        // can possibly be dereferenced until we add the disk to the list. (in addition to our
        // complete lack of threads.)
        let disk = unsafe { Arc::get_mut_unchecked(&mut disk) };

        disk.volumes = volumes;
        disk.disk_guid = disk_guid;
    }

    DISKS.write().push(disk);
    Ok(())
}

#[repr(C)]
#[derive(Debug)]
struct MbrPartitionEntry {
    status: u8,
    chs_start: [u8; 3],
    partition_type: u8,
    chs_end: [u8; 3],
    lba_start: u32,
    lba_size: u32,
}

#[repr(C)]
#[derive(Debug)]
struct GptHeader {
    signature: [u8; 8],
    revision: u32,
    header_size: u32,
    header_crc32: u32,
    reserved0: u32,
    this_lba: u64,
    alternate_lba: u64,
    first_usable_lba: u64,
    last_usable_lba: u64,
    disk_guid: Uuid,
    partition_table_lba: u64,
    partition_entries: u32,
    partition_entry_size: u32,
    partition_table_crc32: u32,
    reserved1: u32,
}

const PARTITION_TYPE_UNUSED: Uuid = Uuid::from_bytes([
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
]);

#[repr(C)]
#[derive(Debug)]
struct GptPartitionEntry {
    partition_type: Uuid,
    partition_uuid: Uuid,
    start_lba: u64,
    end_lba: u64,
    attributes: GptPartitionAttrs,
    partition_name: [u8; 72],
}

bitflags::bitflags! {
    #[repr(transparent)]
    struct GptPartitionAttrs : u64 {
        const REQUIRED              = 1 << 0;
        const NO_BLOCK_IO           = 1 << 1;
        const LEGACY_BIOS_BOOTABLE  = 1 << 2;
    }
}
