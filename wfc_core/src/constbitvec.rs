use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstBitVec<const N: usize> {
    /// The data and length of the bit vector.
    ///
    /// XXX: Doc where is data and where is len.
    data: [u64; N],
}

impl<const N: usize> ConstBitVec<N> {
    // TODO(yan): @Correctness This implementation completely breaks down once
    // CAP reaches 2^64 (or N reaches 2^60), because len currently can't span
    // multiple blocks. In practice, this is a lot of memory and won't happen.

    pub const CAP: usize = N * u64::BITS as usize;

    const LEN_SEGMENT_SIZE: u8 = find_len_segment_size(N);
    const DATA_MASK: u64 = u64::MAX >> (64 - Self::LEN_SEGMENT_SIZE);

    /// Creates a new bit vector with all bits set to zero.
    pub const fn zeros() -> Self {
        Self { data: [0; N] }
    }

    /// Returns whether the bit vector has a bit set.
    pub fn contains(&self, index: usize) -> bool {
        assert!(index < Self::CAP);
        let index = usize::from(index);

        let blk_index = index / 64;
        let bit_index = index % 64;
        let value = self.data[blk_index] & (1 << bit_index);

        value != 0
    }

    /// Sets a bit in the vector to one. Returns [`true`] if the bit was not
    /// previously set.
    pub fn add(&mut self, index: usize) -> bool {
        assert!(index < Self::CAP);
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
        assert!(index < Self::CAP);
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
        (self.data[N - 1] >> Self::LEN_SEGMENT_SIZE) as usize
    }

    /// Sets all bits in the bit vector to zero.
    pub fn clear(&mut self) {
        self.data = [0; N];
    }

    fn inc_len(&mut self) {
        let mut len = self.data[N - 1] >> Self::LEN_SEGMENT_SIZE;
        assert!(len < Self::CAP as u64);

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

impl<const N: usize> fmt::Binary for ConstBitVec<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "B:{}", N)?;

        for i in 0..N {
            write!(f, "[{:064b}]", self.data[i])?
        }

        Ok(())
    }
}

pub const fn find_len_segment_size(blk_count: usize) -> u8 {
    // TODO(yan): This is kind of lazy, work out the formula?

    // Start at 6, as 2^6 is 64 - our block size.
    let mut offset = 6_u8;

    while blk_count * 64 > 2_usize.pow(offset as u32) {
        offset += 1;
    }

    offset
}
