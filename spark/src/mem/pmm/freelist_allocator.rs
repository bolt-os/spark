// SPDX-FileCopyrightText:  2022-2023 xvanc and contributors
// SPDX-License-Identifier: BSD-3-Clause

use core::{mem, ptr};

pub struct Tag {
    prev: *mut Tag,
    next: *mut Tag,
    pub base: usize,
    pub size: usize,
}

fn pow2_align_up(x: usize, align: usize) -> usize {
    (x + (align - 1)) & !(align - 1)
}

impl Tag {
    unsafe fn prev(self: *const Self) -> *mut Tag {
        (*self).prev
    }

    unsafe fn next(self: *const Self) -> *mut Tag {
        (*self).next
    }

    unsafe fn base(self: *const Self) -> usize {
        (*self).base
    }

    unsafe fn size(self: *const Self) -> usize {
        (*self).size
    }

    unsafe fn end(self: *const Self) -> usize {
        self.base() + self.size()
    }

    unsafe fn is_empty(self: *const Self) -> bool {
        self.size() == 0
    }

    unsafe fn can_satisfy(self: *const Self, size: usize, align: usize) -> Option<usize> {
        assert!(!self.is_empty());
        let start = pow2_align_up(self.base(), align);
        (start < self.end() && self.end() - start >= size).then_some(start - self.base())
    }
}

pub struct FreelistAllocator {
    head: *mut Tag,
    tail: *mut Tag,
    len: usize,
}

unsafe impl Send for FreelistAllocator {}
unsafe impl Sync for FreelistAllocator {}

impl FreelistAllocator {
    // Returns an iterator over all tags.
    fn tags(&self) -> impl Iterator<Item = *mut Tag> {
        let mut tag = self.head;
        core::iter::from_fn(move || unsafe {
            if tag.is_null() {
                None
            } else {
                let next = tag.next();
                Some(mem::replace(&mut tag, next))
            }
        })
    }

    // Unlink a tag and decrement `self.len`.
    unsafe fn remove_tag(&mut self, tag: *mut Tag) {
        let prev = tag.prev();
        let next = tag.next();

        if prev.is_null() {
            self.head = next;
        } else {
            (*prev).next = next;
        }
        if next.is_null() {
            self.tail = prev;
        } else {
            (*next).prev = prev;
        }
        self.len -= 1;
    }

    // Link a tag.
    unsafe fn link_tag(&mut self, tag: *mut Tag, prev: *mut Tag, next: *mut Tag) {
        if next.is_null() {
            self.tail = tag;
        } else {
            (*next).prev = tag;
        }
        if prev.is_null() {
            self.head = tag;
        } else {
            (*prev).next = tag;
        }
    }

    // Link a tag and decrement `self.len`.
    unsafe fn insert_tag(&mut self, tag: *mut Tag, prev: *mut Tag, next: *mut Tag) {
        self.link_tag(tag, prev, next);
        self.len += 1;
    }

    // Insert (add/deallocate) a region.
    unsafe fn insert(&mut self, mut base: usize, mut size: usize) {
        let mut prev = ptr::null_mut();
        let mut next = ptr::null_mut();
        for tag in self.tags() {
            prev = next;
            next = tag;
            if next.base() > base {
                break;
            }
        }

        if !prev.is_null() && prev.end() == base {
            base = prev.base();
            size += prev.size();
            prev = prev.prev();
            self.len -= 1;
        }

        if !next.is_null() && (base + size) == next.base() {
            size += next.size();
            next = next.next();
            self.len -= 1;
        }

        let new = ptr::from_exposed_addr_mut::<Tag>(base);
        *new = Tag {
            prev,
            next,
            base,
            size,
        };

        self.insert_tag(new, prev, next);
    }

    // Find and remove (allocate) a region.
    unsafe fn remove(&mut self, size: usize, align: usize) -> Option<usize> {
        for tag in self.tags() {
            if let Some(offset) = tag.can_satisfy(size, align) {
                let addr = tag.base() + offset;

                if offset == 0 {
                    // Take from the front.

                    // If this would leave the region empty, just remove the tag.
                    if tag.size() == size {
                        self.remove_tag(tag);
                    } else {
                        // Move the tag to after the allocation.
                        let base = tag.base() + size;
                        let new = ptr::from_exposed_addr_mut::<Tag>(base);
                        *new = Tag {
                            prev: tag.prev(),
                            next: tag.next(),
                            base,
                            size: tag.size() - size,
                        };
                        self.link_tag(new, tag.prev(), tag.next());
                    }
                } else if offset + size == tag.size() {
                    // Take from the back.
                    (*tag).size -= size;
                } else {
                    // Take from the middle.

                    // Create a new tag for the region after the allocation.
                    let new = ptr::from_exposed_addr_mut::<Tag>(addr + size);
                    *new = Tag {
                        prev: tag,
                        next: tag.next(),
                        base: addr,
                        size: tag.size() - offset,
                    };

                    (*tag).size = offset;
                    self.insert_tag(new, tag.next(), tag);
                }

                return Some(addr);
            }
        }
        None
    }
}

impl FreelistAllocator {
    /// Create a new, empty `FreelistAllocator`.
    pub const fn new() -> FreelistAllocator {
        Self {
            head: ptr::null_mut(),
            tail: ptr::null_mut(),
            len: 0,
        }
    }

    /// Returns the number of entries in the list
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Returns an iterator over all entries in the list
    pub fn entries(&self) -> impl Iterator<Item = &Tag> {
        self.tags().map(|tag| unsafe { &*tag })
    }

    /// Add a region to the list
    pub unsafe fn add_region(&mut self, base: usize, size: usize) {
        self.insert(base, size);
    }

    pub fn allocate(&mut self, size: usize, align: usize) -> Option<usize> {
        unsafe { self.remove(size, align) }
    }

    pub unsafe fn deallocate(&mut self, base: usize, size: usize) {
        self.insert(base, size);
    }
}
