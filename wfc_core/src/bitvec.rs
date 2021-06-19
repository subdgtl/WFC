use std::fmt;

pub const MAX_LEN: u8 = u8::MAX - 8;

const DATA_MASK: u64 = u64::MAX >> 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BitVec {
    /// The data and length of the bit vector.
    ///
    /// The last eight bits represent the length and the first 248 bits contain
    /// the data itself. While the length could be computed every time with
    /// [`u64::count_ones`], the intrinsic showed up in profiling quite a lot
    /// and it is just faster to use the cached value.
    data: [u64; 4],
}

impl BitVec {
    /// Creates a new bit vector with all bits set to zero.
    pub const fn zeros() -> Self {
        Self { data: [0; 4] }
    }

    /// Returns whether the bit vector has a bit set.
    pub fn contains(&self, index: u8) -> bool {
        assert!(index < MAX_LEN);
        let index = usize::from(index);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        value != 0
    }

    /// Returns the first set bit or [`None`].
    pub fn first(&self) -> Option<u8> {
        let len = self.data[3] >> 56;
        if len == 0 {
            None
        } else {
            let lz0 = self.data[0].leading_zeros();
            if lz0 < 64 {
                let first = lz0 as u8;
                return Some(first);
            }

            let lz1 = self.data[1].leading_zeros();
            if lz1 < 64 {
                let first = 64 - 1 + lz1 as u8;
                return Some(first);
            }

            let lz2 = self.data[2].leading_zeros();
            if lz2 < 64 {
                let first = 64 * 2 - 1 + lz2 as u8;
                return Some(first);
            }

            let lz3 = (self.data[3] & DATA_MASK).leading_zeros();
            let first = 64 * 3 - 1 + lz3 as u8;

            Some(first)
        }
    }

    /// Sets a bit in the vector to one. Returns [`true`] if the bit was not
    /// previously set.
    pub fn add(&mut self, index: u8) -> bool {
        assert!(index < MAX_LEN);
        let index = usize::from(index);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        self.data[blk_index] |= 1 << bit_index;

        if value == 0 {
            self.inc_len();
            true
        } else {
            false
        }
    }

    /// Sets a bit in the vector to zero. Returns [`true`] if the bit was
    /// previously set.
    pub fn remove(&mut self, index: u8) -> bool {
        assert!(index < MAX_LEN);
        let index = usize::from(index);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        self.data[blk_index] &= !(1 << bit_index);

        if value != 0 {
            self.dec_len();
            true
        } else {
            false
        }
    }

    /// Returns the number of ones in the binary representation of the bit
    /// vector.
    pub fn len(&self) -> usize {
        (self.data[3] >> 56) as usize
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

    fn inc_len(&mut self) {
        let mut len = self.data[3] >> 56;
        assert!(len < MAX_LEN as u64);

        len += 1;

        self.data[3] = (self.data[3] & DATA_MASK) | (len << 56);
    }

    fn dec_len(&mut self) {
        let mut len = self.data[3] >> 56;
        assert!(len > 0);

        len -= 1;

        self.data[3] = (self.data[3] & DATA_MASK) | (len << 56)
    }
}

impl fmt::Binary for BitVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{:064b}][{:064b}][{:064b}][{:064b}]",
            self.data[0], self.data[1], self.data[2], self.data[3],
        )
    }
}

pub struct BitVecIterator<'a> {
    bitvec: &'a BitVec,
    next: u8,
}

impl<'a> Iterator for BitVecIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        // TODO(yan): @Speed Try using leading zeros intrinsic to directly seek
        // to the next element. Ideally do this in a BitVec::first_after
        // function or similar.
        while self.next < MAX_LEN {
            let index = self.next;

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

        let shl0 = n;
        let shl1 = n.saturating_sub(64);
        let shl2 = n.saturating_sub(64 * 2);
        let shl3 = n.saturating_sub(64 * 3);

        let m0 = u64::MAX.checked_shl(shl0).unwrap_or(0);
        let m1 = u64::MAX.checked_shl(shl1).unwrap_or(0);
        let m2 = u64::MAX.checked_shl(shl2).unwrap_or(0);
        let m3 = u64::MAX.checked_shl(shl3).unwrap_or(0) & DATA_MASK;

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
