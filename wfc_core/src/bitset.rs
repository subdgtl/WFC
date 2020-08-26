use std::mem;

use crate::convert::{cast_u8, cast_usize};

const U8_MAX: u16 = u8::MAX as u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TinyBitSet {
    data: [u64; 4],
}

impl TinyBitSet {
    pub fn zeroes() -> Self {
        Self { data: [0; 4] }
    }

    pub fn ones() -> Self {
        Self {
            data: [u64::MAX; 4],
        }
    }

    pub fn contains(&self, index: u8) -> bool {
        let index = usize::from(index);
        debug_assert!(index <= mem::size_of::<[u64; 4]>() * 8);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        value != 0
    }

    pub fn add(&mut self, index: u8) -> bool {
        let index = usize::from(index);
        debug_assert!(index <= mem::size_of::<[u64; 4]>() * 8);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        self.data[blk_index] |= 1 << bit_index;

        value == 0
    }

    pub fn remove(&mut self, index: u8) -> bool {
        let index = usize::from(index);
        debug_assert!(index <= mem::size_of::<[u64; 4]>() * 8);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        self.data[blk_index] &= !(1 << bit_index);

        value != 0
    }

    pub fn len(&self) -> usize {
        let c0 = cast_usize(self.data[0].count_ones());
        let c1 = cast_usize(self.data[1].count_ones());
        let c2 = cast_usize(self.data[2].count_ones());
        let c3 = cast_usize(self.data[3].count_ones());

        c0.saturating_add(c1).saturating_add(c2).saturating_add(c3)
    }

    pub fn clear(&mut self) {
        self.data = [0; 4];
    }

    pub fn iter(&self) -> TinyBitSetIterator {
        TinyBitSetIterator {
            bitset: self,
            next: 0,
        }
    }

    pub fn and(&mut self, other: &TinyBitSet) {
        for (self_value_mut, other_value) in self.data.iter_mut().zip(other.data.iter()) {
            *self_value_mut &= other_value;
        }
    }

    pub fn or(&mut self, other: &TinyBitSet) {
        for (self_value_mut, other_value) in self.data.iter_mut().zip(other.data.iter()) {
            *self_value_mut |= other_value;
        }
    }

    pub fn xor(&mut self, other: &TinyBitSet) {
        for (self_value_mut, other_value) in self.data.iter_mut().zip(other.data.iter()) {
            *self_value_mut ^= other_value;
        }
    }

    pub fn not(&mut self) {
        for self_value_mut in self.data.iter_mut() {
            *self_value_mut = !*self_value_mut;
        }
    }
}

pub struct TinyBitSetIterator<'a> {
    bitset: &'a TinyBitSet,
    next: u16,
}

impl<'a> Iterator for TinyBitSetIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        while self.next <= U8_MAX {
            let current = cast_u8(self.next);

            let contains = self.bitset.contains(current);
            self.next += 1;

            if contains {
                return Some(current);
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Mask out the already spent bits before computing size hint
        let m0 = 0u64.wrapping_sub(1) << self.next.max(64);
        let m1 = 0u64.wrapping_sub(1) << self.next.saturating_sub(64).max(64);
        let m2 = 0u64.wrapping_sub(1) << self.next.saturating_sub(64 * 2).max(64);
        let m3 = 0u64.wrapping_sub(1) << self.next.saturating_sub(64 * 3).max(64);

        let c0 = cast_usize((m0 & self.bitset.data[0]).count_ones());
        let c1 = cast_usize((m1 & self.bitset.data[1]).count_ones());
        let c2 = cast_usize((m2 & self.bitset.data[2]).count_ones());
        let c3 = cast_usize((m3 & self.bitset.data[3]).count_ones());

        let size = c0.saturating_add(c1).saturating_add(c2).saturating_add(c3);

        (size, Some(size))
    }
}

impl<'a> IntoIterator for &'a TinyBitSet {
    type Item = u8;
    type IntoIter = TinyBitSetIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TinyBitSetIterator {
            bitset: self,
            next: 0,
        }
    }
}
