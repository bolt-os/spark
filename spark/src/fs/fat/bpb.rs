/*
 * Copyright (c) 2022 xvanc and contributors
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

//! BIOS Parameter Block

use core::ops;

mod private {
    pub trait Sealed {}

    impl Sealed for super::Fat16 {}
    impl Sealed for super::Fat32 {}
}

pub trait FatType: private::Sealed {
    type BpbData;
}

#[derive(Debug)]
pub struct Fat16;

impl FatType for Fat16 {
    type BpbData = BpbFat12And16;
}

#[derive(Debug)]
pub struct Fat32;

impl FatType for Fat32 {
    type BpbData = BpbFat32;
}

#[repr(C)]
#[derive(Debug)]
pub struct Bpb<F: FatType> {
    pub common: Common,
    pub type_data: F::BpbData,
}

impl<F: FatType> ops::Deref for Bpb<F> {
    type Target = F::BpbData;

    fn deref(&self) -> &Self::Target {
        &self.type_data
    }
}

impl Bpb<Fat32> {
    pub const fn root_cluster(&self) -> u32 {
        u32::from_le_bytes(self.type_data.root_cluster)
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct Common {
    pub boot_jmp: [u8; 3],
    pub oem_name: [u8; 8],
    pub bytes_per_sector: [u8; 2],
    pub sectors_per_cluster: u8,
    pub reserved_sectors: [u8; 2],
    pub num_fats: u8,
    pub root_entry_count: [u8; 2],
    pub total_sectors_16: [u8; 2],
    pub media: u8,
    pub fat_size_16: [u8; 2],
    pub sectors_per_track: [u8; 2],
    pub num_heads: [u8; 2],
    pub hidden_sectors: [u8; 4],
    pub total_sectors_32: [u8; 4],
}

#[repr(C)]
#[derive(Debug)]
pub struct BpbFat12And16 {
    pub drive_number: u8,
    pub reserved1: u8,
    pub boot_signature: u8,
    pub volume_id: [u8; 4],
    pub volume_label: [u8; 11],
    pub fs_type: [u8; 8],
    pub _rsvd0: [u8; 448],
    pub signature: [u8; 2],
}

#[repr(C)]
#[derive(Debug)]
pub struct BpbFat32 {
    pub fat_size_32: [u8; 4],
    pub ext_flags: [u8; 2],
    pub fs_version: [u8; 2],
    pub root_cluster: [u8; 4],
    pub fs_info: [u8; 2],
    pub bk_boot_sector: [u8; 2],
    pub reserved: [u8; 12],
    pub drive_number: u8,
    pub reserved1: u8,
    pub boot_signature: u8,
    pub volume_id: [u8; 4],
    pub volume_label: [u8; 11],
    pub fs_type: [u8; 8],
    pub _rsvd0: [u8; 420],
    pub signature: [u8; 2],
}

#[derive(Debug)]
pub enum Superblock {
    Fat16(Bpb<Fat16>),
    Fat32(Bpb<Fat32>),
}

impl ops::Deref for Superblock {
    type Target = Common;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Fat16(bpb) => &bpb.common,
            Self::Fat32(bpb) => &bpb.common,
        }
    }
}

impl Common {
    pub const fn bytes_per_sector(&self) -> u64 {
        u16::from_le_bytes(self.bytes_per_sector) as u64
    }

    pub const fn root_entry_count(&self) -> u64 {
        u16::from_le_bytes(self.root_entry_count) as u64
    }

    pub const fn sectors_per_cluster(&self) -> u64 {
        self.sectors_per_cluster as u64
    }

    pub const fn reserved_sectors(&self) -> u64 {
        u16::from_le_bytes(self.reserved_sectors) as u64
    }

    pub const fn root_directory_sectors(&self) -> u64 {
        ((self.root_entry_count() * 32) + (self.bytes_per_sector() - 1)) / self.bytes_per_sector()
    }

    pub const fn total_sectors(&self) -> u64 {
        let total16 = u16::from_le_bytes(self.total_sectors_16) as u64;
        if total16 == 0 {
            u32::from_le_bytes(self.total_sectors_32) as u64
        } else {
            total16
        }
    }

    pub fn fat_size(&self, maybe32: &[u8]) -> u64 {
        let size_16 = u16::from_le_bytes(self.fat_size_16) as u64;
        if size_16 == 0 {
            u32::from_le_bytes(maybe32.try_into().unwrap()) as u64
        } else {
            size_16
        }
    }

    pub fn data_sectors(&self, maybe32: &[u8]) -> u64 {
        self.total_sectors()
            - (self.reserved_sectors()
                + (self.num_fats as u64 * self.fat_size(maybe32))
                + self.root_directory_sectors())
    }

    pub fn cluster_count(&self, maybe32: &[u8]) -> u64 {
        self.data_sectors(maybe32) / self.sectors_per_cluster()
    }
}

impl Superblock {
    pub fn fat_entry_scale(&self) -> u64 {
        match self {
            Self::Fat16(_) => 2,
            Self::Fat32(_) => 4,
        }
    }

    pub fn fat_offset_for_cluster(&self, cluster: u32) -> (u64, usize) {
        let fat_offset = cluster as u64 * self.fat_entry_scale();
        let lba = self.reserved_sectors() + fat_offset / self.bytes_per_sector();
        let offset = fat_offset % self.bytes_per_sector();
        (lba, offset as usize)
    }

    pub fn fat_size(&self) -> u64 {
        let size_16 = u16::from_le_bytes(self.fat_size_16) as u64;
        match self {
            Self::Fat16(_) => size_16,
            Self::Fat32(bpb) => {
                if size_16 == 0 {
                    u32::from_le_bytes(bpb.type_data.fat_size_32) as u64
                } else {
                    size_16
                }
            }
        }
    }

    pub fn first_data_sector(&self) -> u64 {
        self.reserved_sectors()
            + (self.num_fats as u64 * self.fat_size())
            + self.root_directory_sectors()
    }

    pub fn cluster_to_lba(&self, cluster: u32) -> u64 {
        self.first_data_sector() + (cluster as u64 - 2) * self.sectors_per_cluster()
    }
}
