// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

#![cfg(feature = "fs-fat")]

mod bpb;
mod dir;

use self::{
    bpb::{Bpb, Fat16, Fat32, Superblock},
    dir::{DirAttrs, DirEntry, LongDirEntry, LongName, ShortDirEntry},
};
use crate::{
    dev::block::{BlockIo, PartitionType, Volume},
    fs::{File, FileType, FilesystemDriver},
    io, mem, size_of,
};
use alloc::sync::Arc;
use core::{cmp, mem::MaybeUninit};

#[used]
#[link_section = "fs_drivers"]
static DRIVER: FilesystemDriver = FilesystemDriver {
    name: "fat",
    mount: Fat::mount,
};

/// An instance of a FAT filesystem
#[derive(Debug)]
pub struct Fat {
    volume: Arc<Volume>,
    superblock: Box<Superblock>,
    cluster_size: u64,
    max_cluster: u32,
}

impl Fat {
    pub fn mount(volume: &Arc<Volume>) -> io::Result<Box<dyn File>> {
        match volume.partition_type {
            PartitionType::Mbr(0x1 | 0x4 | 0x5 | 0x6 | 0x0b | 0xc | 0xe | 0xee) => {}
            _ => return Err(io::Error::Unsupported),
        }

        let mut buf = vec![0u8; volume.block_size() as usize];
        volume.read_blocks(0, &mut buf)?;

        let common_bpb = unsafe { mem::cast_slice::<bpb::Common>(&buf[..size_of!(bpb::Common)]) };
        let cluster_count = common_bpb.cluster_count(&buf[size_of!(bpb::Common)..][..4]);

        let sb = if common_bpb.bytes_per_sector() == 0 {
            // ExFAT
            log::error!("unsupported exFAT filesystem");
            return Err(io::Error::Unsupported);
        } else if cluster_count < 4085 {
            // FAT12
            log::error!("unsupported FAT12 filesystem");
            return Err(io::Error::Unsupported);
        } else if cluster_count < 65525 {
            // FAT16
            let bpb = unsafe {
                let mut uninit = MaybeUninit::<Bpb<Fat16>>::uninit();
                uninit
                    .as_mut_ptr()
                    .copy_from_nonoverlapping(buf.as_ptr().cast(), 1);
                uninit.assume_init()
            };
            Box::new(Superblock::Fat16(bpb))
        } else {
            // FAT32
            let bpb = unsafe {
                let mut uninit = MaybeUninit::<Bpb<Fat32>>::uninit();
                uninit
                    .as_mut_ptr()
                    .copy_from_nonoverlapping(buf.as_ptr().cast(), 1);
                uninit.assume_init()
            };
            Box::new(Superblock::Fat32(bpb))
        };

        let cluster_size = sb.sectors_per_cluster() * sb.bytes_per_sector();
        let fs = Arc::new(Self {
            volume: Arc::clone(volume),
            superblock: sb,
            cluster_size,
            max_cluster: cluster_count as u32 - 1,
        });

        let sb = &*fs.superblock;
        let extents = match sb {
            Superblock::Fat16(_) => vec![Extent {
                lba: sb.reserved_sectors() + sb.num_fats as u64 * sb.fat_size(),
                blocks: sb.root_entry_count(),
            }],
            Superblock::Fat32(bpb) => fs.get_extents(bpb.root_cluster())?,
        };

        Ok(Box::new(OpenFile {
            fs,
            extents,
            offset: 0,
            cursor: Cursor {
                extent: 0,
                extent_offset: 0,
            },
            size: !0,
            entry: None,
        }))
    }

    /// Returns the [`FatEntry`] for a given cluster
    fn fat_entry(&self, cluster: u32) -> io::Result<FatEntry> {
        if cluster > self.max_cluster {
            return Err(io::Error::OutOfBounds);
        }
        let (lba, offset) = self.superblock.fat_offset_for_cluster(cluster);
        match *self.superblock {
            Superblock::Fat16(_) => {
                let mut buf = [0; 2];
                self.volume
                    .read(lba * self.volume.block_size() + offset as u64, &mut buf)?;
                let entry = match u16::from_le_bytes(buf) {
                    0x0000 => FatEntry::Free,
                    0xfff7 => FatEntry::Bad,
                    0xfff8..=0xfffe => FatEntry::Reserved,
                    0xffff => FatEntry::Allocated(None),
                    cluster => {
                        if cluster as u32 > self.max_cluster {
                            FatEntry::Reserved
                        } else {
                            FatEntry::Allocated(Some(cluster as u32))
                        }
                    }
                };
                Ok(entry)
            }
            Superblock::Fat32(_) => {
                let mut buf = [0; 4];
                self.volume
                    .read(lba * self.volume.block_size() + offset as u64, &mut buf)?;
                let entry = match u32::from_le_bytes(buf) {
                    0x00000000 => FatEntry::Free,
                    0x0ffffff7 => FatEntry::Bad,
                    0x0ffffff8..=0x0ffffffe => FatEntry::Reserved,
                    0x0fffffff => FatEntry::Allocated(None),
                    cluster => {
                        if cluster > self.max_cluster {
                            FatEntry::Reserved
                        } else {
                            FatEntry::Allocated(Some(cluster))
                        }
                    }
                };
                Ok(entry)
            }
        }
    }

    /// Search the directory at `cluster` for the given `name`
    fn lookup(&self, mut cluster: u32, name: &str) -> io::Result<DirEntry> {
        let mut lookup = Lookup {
            long_name: LongName::new(),
            name,
        };
        let mut buf = vec![0u8; self.cluster_size as usize];
        loop {
            self.volume
                .read_blocks(self.superblock.cluster_to_lba(cluster), &mut buf)?;
            if let Some(entry) = lookup.search(&buf)? {
                break Ok(entry);
            }
            match self.fat_entry(cluster)? {
                FatEntry::Allocated(Some(next)) => cluster = next,
                FatEntry::Allocated(None) => break Err(io::Error::NotFound),
                _ => break Err(io::Error::InvalidArgument),
            }
        }
    }

    fn get_extents(&self, mut cluster: u32) -> io::Result<Vec<Extent>> {
        let mut extents = vec![Extent {
            lba: self.superblock.cluster_to_lba(cluster),
            blocks: self.superblock.sectors_per_cluster(),
        }];
        while let FatEntry::Allocated(Some(next)) = self.fat_entry(cluster)? {
            if next == cluster + 1 {
                extents.last_mut().unwrap().blocks += self.superblock.sectors_per_cluster();
            } else {
                extents.push(Extent {
                    lba: self.superblock.cluster_to_lba(next),
                    blocks: self.superblock.sectors_per_cluster(),
                });
            }
            cluster = next;
        }
        Ok(extents)
    }
}

/// An entry in the File Allocation Table
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum FatEntry {
    Free,
    Allocated(Option<u32>),
    Bad,
    Reserved,
}

#[derive(Clone, Copy, Debug)]
struct Extent {
    lba: u64,
    blocks: u64,
}

#[derive(Clone, Copy, Debug, Default)]
struct Cursor {
    extent: usize,
    extent_offset: u64,
}

#[derive(Debug)]
struct OpenFile {
    fs: Arc<Fat>,
    extents: Vec<Extent>,
    offset: u64,
    cursor: Cursor,
    size: u64,
    entry: Option<DirEntry>,
}

impl File for OpenFile {
    fn open(&mut self, path: &str) -> io::Result<Box<dyn File>> {
        let block_size = self.fs.volume.block_size();
        let mut path_iter = path.trim_start_matches('/').split('/').peekable();

        let mut search_root = || -> io::Result<Result<DirEntry, u32>> {
            let sb = &*self.fs.superblock;
            match sb {
                Superblock::Fat16(_) => {
                    let root_lba = sb.reserved_sectors() + sb.num_fats as u64 * sb.fat_size();
                    let root_size = sb.root_directory_sectors();
                    let mut buf = vec![0u8; (root_size * block_size) as usize];
                    self.fs.volume.read_blocks(root_lba, &mut buf)?;

                    let mut lookup = Lookup {
                        long_name: LongName::new(),
                        name: path_iter.next().unwrap(),
                    };
                    let entry = lookup.search(&buf)?.ok_or(io::Error::NotFound)?;

                    if path_iter.peek().is_none() {
                        // this is the end
                        Ok(Ok(entry))
                    } else {
                        Ok(Err(entry.cluster))
                    }
                }
                Superblock::Fat32(bpb) => Ok(Err(bpb.root_cluster())),
            }
        };

        let entry_or_cluster = if path.starts_with('/') {
            // absolute
            search_root()?
        } else if let Some(entry) = &self.entry {
            // relative
            Err(entry.cluster)
        } else {
            // relative but `self` is the root so
            search_root()?
        };
        let entry = match entry_or_cluster {
            Ok(entry) => entry,
            Err(mut cluster) => loop {
                // Get the next path component.
                let Some(name) = path_iter.next() else {
                    return Err(io::Error::NotFound);
                };
                // Find the entry for the name.
                let entry = self.fs.lookup(cluster, name)?;
                if path_iter.peek().is_none() {
                    if entry.file_type == FileType::Directory {
                        return Err(io::Error::IsADirectory);
                    }
                    break entry;
                }
                if entry.file_type == FileType::Directory {
                    cluster = entry.cluster;
                } else {
                    // Expected a directory.
                    return Err(io::Error::NotADirectory);
                }
            },
        };

        let extents = self.fs.get_extents(entry.cluster)?;

        Ok(Box::new(OpenFile {
            fs: Arc::clone(&self.fs),
            extents,
            offset: 0,
            size: entry.size,
            cursor: Cursor::default(),
            entry: Some(entry),
        }))
    }

    fn size(&mut self) -> u64 {
        self.size
    }

    fn position(&mut self) -> u64 {
        self.offset
    }

    fn volume(&self) -> &Arc<Volume> {
        &self.fs.volume
    }

    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let block_size = self.fs.volume.block_size();
        let mut count = buf.len() as u64;
        let mut buf_offset = 0;

        while count > 0 {
            if self.offset >= self.size {
                break;
            }
            let Some(ext) = self.extents.get(self.cursor.extent) else {
                // End of file.
                break;
            };

            // Byte offset in volume.
            let v_offset = ext.lba * block_size + self.cursor.extent_offset;
            // Remaining bytes in current extent.
            let ext_rem = ext.blocks * block_size - self.cursor.extent_offset;
            let tx_size = cmp::min(count, ext_rem);

            self.fs
                .volume
                .read(v_offset, &mut buf[buf_offset..][..tx_size as usize])?;

            // If we read the remainder of this extent, move on to the next one.
            // Otherwise, just advance the offset.
            if tx_size == ext_rem {
                self.cursor.extent += 1;
                self.cursor.extent_offset = 0;
            } else {
                self.cursor.extent_offset += tx_size;
            }
            self.offset += tx_size;
            buf_offset += tx_size as usize;
            count -= tx_size;
        }

        Ok(buf.len() - count as usize)
    }
}

struct Lookup<'a> {
    long_name: LongName,
    name: &'a str,
}

impl Lookup<'_> {
    fn search(&mut self, buf: &[u8]) -> io::Result<Option<DirEntry>> {
        let mut offset = 0;

        while let Some(raw_entry) = buf.get(offset..offset + dir::ENTRY_SIZE) {
            offset += dir::ENTRY_SIZE;

            match raw_entry[0] {
                0xe5 => continue, // unused entry
                0x00 => break,    // unused entry, end of list
                _ => {}
            }

            let attrs = DirAttrs::new(raw_entry[11]);
            if attrs.contains(DirAttrs::LONG_NAME) {
                let entry = unsafe { mem::cast_slice::<LongDirEntry>(raw_entry) };
                let piece = entry.name_piece_raw();
                let mut len = 0;
                while len < piece.len() && piece[len] != 0 {
                    len += 1;
                }
                self.long_name.push(&piece[..len])?;
            } else {
                if attrs.contains(DirAttrs::VOLUME_ID) {
                    // This directory entry contains the FAT Volume ID.
                    continue;
                }
                let entry = unsafe { mem::cast_slice::<ShortDirEntry>(raw_entry) };
                let entry_name = if self.long_name.is_empty() {
                    entry.name()
                } else {
                    self.long_name.finish().unwrap()
                };
                if self.name.to_lowercase() != entry_name.to_lowercase() {
                    continue;
                }
                return Ok(Some(DirEntry {
                    file_type: attrs.into(),
                    cluster: entry.cluster(),
                    size: entry.size as u64,
                }));
            }
        }

        Ok(None)
    }
}
