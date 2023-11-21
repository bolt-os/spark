/*
 * Copyright (c) 2022-2023 xvanc and contributors
 * SPDX-License-Identifier: BSD-3-Clause
 */

use core::fmt;

#[derive(Clone, Copy)]
pub struct Range {
    pub base: usize,
    pub size: usize,
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Range {{ base: {:#x}, size: {:#x}, end: {:#x} }}",
            self.base,
            self.size,
            self.end(),
        )
    }
}

impl Range {
    fn end(self) -> usize {
        self.base + self.size
    }

    fn is_empty(self) -> bool {
        self.size == 0
    }

    fn overlaps_with(self, other: Self) -> bool {
        other.base < self.end() && other.end() > self.base
    }

    fn contains(self, other: Self) -> bool {
        self.base <= other.base && other.end() <= self.end()
    }
}

pub struct InitRanges<const MAX_RANGES: usize> {
    ranges: [Range; MAX_RANGES],
    len: usize,
    removed: bool,
}

impl<const MAX_RANGES: usize> InitRanges<MAX_RANGES> {
    pub const fn new() -> InitRanges<MAX_RANGES> {
        Self {
            ranges: [Range { base: 0, size: 0 }; MAX_RANGES],
            len: 0,
            removed: false,
        }
    }

    pub fn ranges(&self) -> &[Range] {
        &self.ranges[..self.len]
    }

    fn ranges_mut(&mut self) -> &mut [Range] {
        &mut self.ranges[..self.len]
    }

    fn insert_range(&mut self, index: usize, range: Range) {
        assert!(self.len < MAX_RANGES);
        self.ranges.copy_within(index..self.len, index + 1);
        self.ranges[index] = range;
        self.len += 1;
    }

    fn remove_range(&mut self, index: usize) {
        self.ranges.copy_within(index + 1.., index);
        self.len -= 1;
    }

    fn range_overlaps(&self, range: Range) -> bool {
        self.ranges().iter().any(|r| r.overlaps_with(range))
    }

    pub fn insert(&mut self, base: usize, size: usize) {
        let range = Range { base, size };
        let index = self.ranges().partition_point(|r| r.base < range.base);

        assert!(
            !self.removed,
            "cannot insert new ranges after ranges have been removed"
        );
        assert!(!self.range_overlaps(range));

        // Check if we can merge with the previous range.
        if index > 0 {
            let prev = &mut self.ranges[index - 1];
            if prev.end() == range.base {
                prev.size += size;
                // Check if we've closed a gap.
                if index < self.len {
                    let next = self.ranges[index];
                    let prev = &mut self.ranges[index - 1];
                    if prev.end() == next.base {
                        prev.size += next.size;
                        self.remove_range(index);
                    }
                }
                return;
            }
        }

        // Check if we can merge with the next range.
        if index < self.len {
            let next = &mut self.ranges[index];
            if range.end() == next.base {
                next.base = base;
                next.size += size;
                // Check if we've closed a gap.
                if index > 0 {
                    let next = *next;
                    let prev = &mut self.ranges[index - 1];
                    if prev.end() == next.base {
                        prev.size += next.size;
                        self.remove_range(index);
                    }
                }
                return;
            }
        }

        assert!(index < MAX_RANGES, "too many memory ranges");
        self.insert_range(index, range);
    }

    pub fn remove(&mut self, base: usize, size: usize) {
        let range = Range { base, size };

        let (index, from) = self
            .ranges_mut()
            .iter_mut()
            .enumerate()
            .find(|(_, r)| r.contains(range))
            .expect("`remove()` called on invalid range");

        if base == from.base {
            from.size -= size;
            from.base = base + size;
            if from.is_empty() {
                self.remove_range(index);
            }
            return;
        }

        if range.end() == from.end() {
            from.size -= size;
            if from.is_empty() {
                self.remove_range(index);
            }
            return;
        }

        let new = Range {
            base: range.end(),
            size: from.end() - range.end(),
        };

        from.size = range.base - from.base;
        self.insert_range(index + 1, new);
    }
}
