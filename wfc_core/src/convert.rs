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
