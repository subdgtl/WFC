use std::mem;

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
        // usize is defined to be at least 16 bits wide, the following `as`
        // casts should be ok for up to 2^16 ones in the whole array.
        let c0 = self.data[0].count_ones() as usize;
        let c1 = self.data[1].count_ones() as usize;
        let c2 = self.data[2].count_ones() as usize;
        let c3 = self.data[3].count_ones() as usize;

        c0 + c1 + c2 + c3
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
        self.data[0] &= other.data[0];
        self.data[1] &= other.data[1];
        self.data[2] &= other.data[2];
        self.data[3] &= other.data[3];
    }

    pub fn or(&mut self, other: &TinyBitSet) {
        self.data[0] |= other.data[0];
        self.data[1] |= other.data[1];
        self.data[2] |= other.data[2];
        self.data[3] |= other.data[3];
    }

    pub fn xor(&mut self, other: &TinyBitSet) {
        self.data[0] ^= other.data[0];
        self.data[1] ^= other.data[1];
        self.data[2] ^= other.data[2];
        self.data[3] ^= other.data[3];
    }

    pub fn not(&mut self) {
        self.data[0] &= !self.data[0];
        self.data[1] &= !self.data[1];
        self.data[2] &= !self.data[2];
        self.data[3] &= !self.data[3];
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
            let current = self.next as u8;

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

        // usize is defined to be at least 16 bits wide, the following `as`
        // casts should be ok for up to 2^16 ones in the whole array.
        let c0 = (m0 & self.bitset.data[0]).count_ones() as usize;
        let c1 = (m1 & self.bitset.data[1]).count_ones() as usize;
        let c2 = (m2 & self.bitset.data[2]).count_ones() as usize;
        let c3 = (m3 & self.bitset.data[3]).count_ones() as usize;

        let size = c0 + c1 + c2 + c3;

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
