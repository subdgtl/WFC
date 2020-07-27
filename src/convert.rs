use std::convert::TryInto;
use std::fmt::Debug;

/// Convert `n` to `u16` using `TryFrom` or panic.
///
/// # Panics
///
/// Panics if the conversion returns an error.
pub fn cast_u16<T>(n: T) -> u16
where
    T: TryInto<u16>,
    <T as TryInto<u16>>::Error: Debug,
{
    n.try_into().expect("Expected N to fit in u16")
}

/// Convert `n` to `u32` using `TryFrom` or panic.
///
/// # Panics
///
/// Panics if the conversion returns an error.
pub fn cast_u32<T>(n: T) -> u32
where
    T: TryInto<u32>,
    <T as TryInto<u32>>::Error: Debug,
{
    n.try_into().expect("Expected N to fit in u32")
}

/// Convert `n` to `usize` using `TryFrom` or panic.
///
/// # Panics
///
/// Panics if the conversion returns an error.
pub fn cast_usize<T>(n: T) -> usize
where
    T: TryInto<usize>,
    <T as TryInto<usize>>::Error: Debug,
{
    n.try_into().expect("Expected N to fit in usize")
}
