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

#![no_std]

use core::ptr::NonNull;

pub const PAGE_SIZE: usize = 4096;

#[repr(C)]
#[derive(Debug)]
pub struct Bootinfo {
    pub hart_id: usize,
    pub free_list: FreeList,
    pub kern_file_ptr: *mut u8,
    pub kern_file_len: usize,
}

#[repr(C)]
#[derive(Debug)]
pub struct Region {
    pub base: usize,
    pub num_frames: usize,
    pub next: Option<NonNull<Region>>,
    pub prev: Option<NonNull<Region>>,
}

impl Region {
    pub const fn base(&self) -> usize {
        self.base
    }

    pub const fn end(&self) -> usize {
        self.base + PAGE_SIZE * self.num_frames
    }
}

/// A doubly-linked list of usable memory ranges stored intrusively in the memory it describes
#[repr(C)]
#[derive(Debug, Default)]
pub struct FreeList {
    pub head: Option<NonNull<Region>>,
    pub tail: Option<NonNull<Region>>,
}

unsafe impl Send for FreeList {}

impl FreeList {
    pub const fn new_empty() -> FreeList {
        Self {
            head: None,
            tail: None,
        }
    }

    pub fn regions(&self) -> impl Iterator<Item = &Region> {
        let mut regionp = self.head;

        core::iter::from_fn(move || {
            let region = unsafe { regionp?.as_ref() };
            regionp = region.next;
            Some(region)
        })
    }

    pub fn remove_region(&mut self, region: &Region) {
        if let Some(mut prev) = region.prev {
            unsafe { prev.as_mut().next = region.next };
        } else {
            self.head = region.next;
        }

        if let Some(mut next) = region.next {
            unsafe { next.as_mut().prev = region.prev };
        } else {
            self.tail = region.prev;
        }
    }

    pub fn insert_region(&mut self, mut base: usize, mut num_frames: usize) {
        let mut prev = self.tail;
        let mut next = None;

        while let Some(region) = prev {
            let region = unsafe { region.as_ref() };

            if region.base < base {
                break;
            }

            next = prev;
            prev = region.prev;
        }

        if let Some(prevp) = prev {
            let prevp = unsafe { prevp.as_ref() };
            assert!(prevp.end() <= base);

            if prevp.end() == base {
                base = prevp.base();
                num_frames += prevp.num_frames;
                prev = prevp.prev;
            }
        }
        if let Some(nextp) = next {
            let nextp = unsafe { nextp.as_ref() };
            let new_end = base + PAGE_SIZE * num_frames;
            assert!(new_end <= nextp.base());

            if new_end == nextp.base() {
                num_frames += nextp.num_frames;
                next = nextp.next;
            }
        }

        let new_region = unsafe {
            let ptr = base as *mut Region;

            ptr.write(Region {
                base,
                num_frames,
                next,
                prev,
            });

            Some(NonNull::new_unchecked(ptr))
        };

        if let Some(mut prev) = prev {
            unsafe { prev.as_mut().next = new_region };
        } else {
            self.head = new_region;
        }
        if let Some(mut next) = next {
            unsafe { next.as_mut().prev = new_region };
        } else {
            self.tail = new_region;
        }
    }
}
