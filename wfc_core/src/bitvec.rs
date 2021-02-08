use std::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BitVec {
    data: [u64; 4],
}

impl BitVec {
    /// Creates a new bit vector with all bits set to zero.
    pub fn zeros() -> Self {
        Self { data: [0; 4] }
    }

    /// Creates a new bit vector with all bits set to one.
    pub fn ones() -> Self {
        Self {
            data: [u64::MAX; 4],
        }
    }

    /// Returns whether the bit vector has a bit set.
    pub fn contains(&self, index: u8) -> bool {
        let index = usize::from(index);
        debug_assert!(index < mem::size_of::<[u64; 4]>() * 8);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        value != 0
    }

    /// Sets a bit in the vector to one. Returns `true` if the bit was not
    /// previously set.
    pub fn add(&mut self, index: u8) -> bool {
        let index = usize::from(index);
        debug_assert!(index < mem::size_of::<[u64; 4]>() * 8);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        self.data[blk_index] |= 1 << bit_index;

        value == 0
    }

    /// Sets a bit in the vector to zero. Returns `true` if the bit was
    /// previously set.
    pub fn remove(&mut self, index: u8) -> bool {
        let index = usize::from(index);
        debug_assert!(index < mem::size_of::<[u64; 4]>() * 8);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        self.data[blk_index] &= !(1 << bit_index);

        value != 0
    }

    /// Returns the number of ones in the binary representation of the bit
    /// vector.
    pub fn len(&self) -> usize {
        // usize is defined to be at least 16 bits wide, the following `as`
        // casts should be ok for up to 2^16 ones in the whole array.
        let c0 = self.data[0].count_ones() as usize;
        let c1 = self.data[1].count_ones() as usize;
        let c2 = self.data[2].count_ones() as usize;
        let c3 = self.data[3].count_ones() as usize;

        c0 + c1 + c2 + c3
    }

    /// Sets all bits in the bit vector to zero.
    pub fn clear(&mut self) {
        self.data = [0; 4];
    }

    /// Creates a shared borrowed iterator over the indices of bits set to one.
    pub fn iter(&self) -> BitVecIterator {
        BitVecIterator {
            bitvec: self,
            next: 0,
        }
    }

    /// Performs a bitwise AND operation between this bit vector and other.
    pub fn and(&mut self, other: &BitVec) {
        self.data[0] &= other.data[0];
        self.data[1] &= other.data[1];
        self.data[2] &= other.data[2];
        self.data[3] &= other.data[3];
    }

    /// Performs a bitwise OR operation between this bit vector and other.
    pub fn or(&mut self, other: &BitVec) {
        self.data[0] |= other.data[0];
        self.data[1] |= other.data[1];
        self.data[2] |= other.data[2];
        self.data[3] |= other.data[3];
    }

    /// Performs a bitwise XOR operation between this bit vector and other.
    pub fn xor(&mut self, other: &BitVec) {
        self.data[0] ^= other.data[0];
        self.data[1] ^= other.data[1];
        self.data[2] ^= other.data[2];
        self.data[3] ^= other.data[3];
    }

    /// Performs a bitwise negation on this bit vector.
    pub fn not(&mut self) {
        self.data[0] &= !self.data[0];
        self.data[1] &= !self.data[1];
        self.data[2] &= !self.data[2];
        self.data[3] &= !self.data[3];
    }
}

pub struct BitVecIterator<'a> {
    bitvec: &'a BitVec,
    next: u16,
}

impl<'a> Iterator for BitVecIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        while self.next <= u16::from(u8::MAX) {
            let index = self.next as u8;

            let contains = self.bitvec.contains(index);
            self.next += 1;

            if contains {
                return Some(index);
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Size hints are computed using the count_ones intrinsic on the u64
        // components of the bit vector. To not count bits we already iterated
        // over, we mask them. Masks are computed from the the index to the bit
        // we would visit next.

        let n = u32::from(self.next);

        let m0 = u64::MAX.checked_shl(n).unwrap_or(0);
        let m1 = u64::MAX.checked_shl(n.saturating_sub(64)).unwrap_or(0);
        let m2 = u64::MAX.checked_shl(n.saturating_sub(64 * 2)).unwrap_or(0);
        let m3 = u64::MAX.checked_shl(n.saturating_sub(64 * 3)).unwrap_or(0);

        // Mask out spent bits and count how many set bits remain.

        // usize is defined to be at least 16 bits wide, the following `as`
        // casts should be ok for up to 2^16 ones in the whole array.
        let c0 = (m0 & self.bitvec.data[0]).count_ones() as usize;
        let c1 = (m1 & self.bitvec.data[1]).count_ones() as usize;
        let c2 = (m2 & self.bitvec.data[2]).count_ones() as usize;
        let c3 = (m3 & self.bitvec.data[3]).count_ones() as usize;

        let size = c0 + c1 + c2 + c3;

        (size, Some(size))
    }
}

impl<'a> IntoIterator for &'a BitVec {
    type Item = u8;
    type IntoIter = BitVecIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        BitVecIterator {
            bitvec: self,
            next: 0,
        }
    }
}
