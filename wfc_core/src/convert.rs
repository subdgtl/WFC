use std::convert::TryInto;
use std::fmt::Debug;

/// Convert `n` to `u8` using `TryFrom` or panic.
///
/// # Panics
///
/// Panics if the conversion returns an error.
pub fn cast_u8<T>(n: T) -> u8
where
    T: TryInto<u8>,
    <T as TryInto<u8>>::Error: Debug,
{
    n.try_into().expect("Expected N to fit in u8")
}
