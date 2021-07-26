use std::fmt;

use crate::convert::{cast_u32, cast_usize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BitVec<const N: usize> {
    /// The data and length of the bit vector.
    ///
    /// XXX: Doc where is data and where is len.
    data: [u64; N],
}

impl<const N: usize> BitVec<N> {
    // TODO(yan): @Correctness This implementation completely breaks down once
    // DATA_CAP reaches 2^64 (or N reaches 2^60), because len currently can't span
    // multiple blocks. In practice, this is a lot of memory and won't happen.

    pub const DATA_CAP: usize = N * u64::BITS as usize - Self::LEN_SEGMENT_SIZE;

    const LEN_SEGMENT_SIZE: usize = find_len_segment_size(N);
    const DATA_MASK: u64 = u64::MAX >> (64 - Self::LEN_SEGMENT_SIZE);

    /// Creates a new bit vector with all bits set to zero.
    pub const fn zeros() -> Self {
        Self { data: [0; N] }
    }

    /// Returns whether the bit vector has a bit set.
    pub fn contains(&self, index: usize) -> bool {
        assert!(index < Self::DATA_CAP);
        let index = usize::from(index);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        value != 0
    }

    /// Sets a bit in the vector to one. Returns [`true`] if the bit was not
    /// previously set.
    pub fn add(&mut self, index: usize) -> bool {
        assert!(index < Self::DATA_CAP);
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
    pub fn remove(&mut self, index: usize) -> bool {
        assert!(index < Self::DATA_CAP);
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
        cast_usize(self.data[N - 1] >> Self::LEN_SEGMENT_SIZE)
    }

    /// Sets all bits in the bit vector to zero.
    pub fn clear(&mut self) {
        self.data = [0; N];
    }

    pub fn iter(&self) -> BitVecIterator<N> {
        BitVecIterator {
            bitvec: self,
            next: 0,
        }
    }

    fn inc_len(&mut self) {
        let mut len = self.data[N - 1] >> Self::LEN_SEGMENT_SIZE;
        assert!(len < Self::DATA_CAP as u64);

        len += 1;

        self.data[N - 1] = (self.data[N - 1] & Self::DATA_MASK) | (len << Self::LEN_SEGMENT_SIZE);
    }

    fn dec_len(&mut self) {
        let mut len = self.data[N - 1] >> Self::LEN_SEGMENT_SIZE;
        assert!(len > 0);

        len -= 1;

        self.data[N - 1] = (self.data[N - 1] & Self::DATA_MASK) | (len << Self::LEN_SEGMENT_SIZE)
    }
}

impl<const N: usize> fmt::Binary for BitVec<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "B:{}", N)?;

        for i in 0..N {
            write!(f, "[{:064b}]", self.data[i])?;
        }

        Ok(())
    }
}

impl<'a, const N: usize> IntoIterator for &'a BitVec<N> {
    type Item = usize;
    type IntoIter = BitVecIterator<'a, N>;

    fn into_iter(self) -> Self::IntoIter {
        BitVecIterator {
            bitvec: self,
            next: 0,
        }
    }
}

pub struct BitVecIterator<'a, const N: usize> {
    bitvec: &'a BitVec<N>,
    next: usize,
}

impl<'a, const N: usize> Iterator for BitVecIterator<'a, N> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        // TODO(yan): @Speed Try using leading zeros intrinsic to directly seek
        // to the next element. Ideally do this in a BitVec::first_after
        // function or similar.
        while self.next < BitVec::<N>::DATA_CAP {
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
        // XXX: Can we cast less? If we could be u16, we could widen to u32 and
        // usize as needed without fear.

        // Size hints are computed using the count_ones intrinsic on the u64
        // components of the bit vector. To not count bits we already iterated
        // over, we mask them. Masks are computed from the the index to the bit
        // we would visit next.

        // TODO(yan): @Correctness self.next likely won't be larger than than
        // u32, but we'd love to prove this statically. Unfortunately, we need
        // to bound N with usize, because it's a parameter for array size.
        let next = cast_u32(self.next);

        let mut size = 0;
        for i in 0..N - 1 {
            let s = next.saturating_sub(64 * cast_u32(i));
            let m = u64::MAX.checked_shl(s).unwrap_or(0);
            let c = (m & self.bitvec.data[i]).count_ones();

            size += c;
        }

        {
            let s = next.saturating_sub(64 * (cast_u32(N) - 1));
            let m = u64::MAX.checked_shl(s).unwrap_or(0) & BitVec::<N>::DATA_MASK;
            let c = (m & self.bitvec.data[N - 1]).count_ones();

            size += c;
        }

        let size_usize = cast_usize(size);

        (size_usize, Some(size_usize))
    }
}

const fn find_len_segment_size(blk_count: usize) -> usize {
    // TODO(yan): This is kind of lazy, work out the formula?

    // Start at 6, as 2^6 is 64 - our block size.
    let mut offset = 6;

    while blk_count * 64 > 2_usize.pow(offset as u32) {
        offset += 1;
    }

    offset
}
