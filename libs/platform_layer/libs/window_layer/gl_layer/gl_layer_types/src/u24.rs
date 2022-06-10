/// An unsigned 24-bit integer. Since an `f32` has a 23 bit mantissa, and IEEE
/// floating point has an implicit hidden bit of precision for the mantissa, all
/// values of this type can be losslessy converted to the `f32` values that 
/// represent the same magnitude.
// I briefly considered making this a 3 byte struct, but that seems to increase the
// number of assembly instructions emitted when passing the struct into functions.
// So, it's probably slower. Slower, and a more cumbersome internal implementation 
// just for the sake of a byte in something we don't expect to have many instances of
// sure sounds like a lose to me.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct U24(u32);

impl U24 {
    #[must_use]
    pub fn from_u32_masked(n: u32) -> U24 {
        U24(n & 0x00FF_FFFF)
    }
}

impl From<U24> for f32 {
    // See documentation of `U24` for why this is okay.
    #[allow(clippy::cast_precision_loss)]
    fn from(U24(n): U24) -> f32 {
        n as f32
    }
}

impl From<U24> for u32 {
    fn from(U24(n): U24) -> u32 {
        n
    }
}