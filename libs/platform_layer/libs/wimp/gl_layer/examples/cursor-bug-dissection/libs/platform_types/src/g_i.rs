/// A module containg a Generational Index implementation
use macros::{d, fmt_debug, fmt_display, ord};
pub type Generation = u32;
type LengthSize = u32;

/// The amount of elements in the collection using generational indexes. Not a valid index.
#[derive(Clone, Copy, Default, Hash)]
pub struct Length(LengthSize);
fmt_debug!(for Length: Length(l) in "{}", l);
fmt_display!(for Length: Length(l) in "{}", l);
ord!(and friends for Length: length, other in {
    length.0.cmp(&other.0)
});

impl Length {
    /// This returns a `usize` to make comparing to usize lengths conveinient.
    pub const fn max_value() -> usize {
        LengthSize::max_value() as usize
    }

    /// This takes a `usize` to make creation from usize lengths, where we don't care about
    /// the maximum case, conveinient.
    pub fn or_max(len: usize) -> Self {
        Self({
            let max = Self::max_value();
            if len > max {
                max as LengthSize
            } else {
                len as LengthSize
            }
        })
    }
}

impl From<Length> for usize {
    fn from(length: Length) -> Self {
        length.0 as usize
    }
}

/// The part of `Index` which does not have to do with generations. That is, the part which
/// denotes which element in the collection is desired, in the usual 0-based way.
#[derive(Clone, Copy, Default, Hash)]
pub struct IndexPart(LengthSize);
fmt_debug!(for IndexPart: IndexPart(l) in "{}", l);
fmt_display!(for IndexPart: IndexPart(l) in "{}", l);
ord!(and friends for IndexPart: index_part, other in {
    index_part.0.cmp(&other.0)
});

impl IndexPart {
    /// This returns a `usize` to make comparing to usize lengths conveinient.
    pub const fn max_value() -> usize {
        (LengthSize::max_value() - 1) as usize
    }

    /// This takes a `usize` to make creation from usize lengths, where we don't care about
    /// the maximum case, conveinient.
    pub fn or_max(i: usize) -> Self {
        Self({
            let max = Self::max_value();
            if i > max {
                max as LengthSize
            } else {
                i as LengthSize
            }
        })
    }
}

impl macros::SaturatingAdd<usize> for IndexPart {
    type Output = Self;

    fn saturating_add(self, rhs: usize) -> Self::Output {
        let sum = (self.0 as usize).saturating_add(rhs);

        Self::or_max(sum)
    }
}

impl macros::SaturatingSub<usize> for IndexPart {
    type Output = Self;

    fn saturating_sub(self, rhs: usize) -> Self::Output {
        // assumes `LengthSize` is an unsigned type.
        Self((self.0 as usize).saturating_sub(rhs) as LengthSize)
    }
}

impl From<IndexPart> for usize {
    fn from(part: IndexPart) -> Self {
        part.0 as usize
    }
}

impl From<IndexPart> for Length {
    fn from(part: IndexPart) -> Self {
        Length(part.0)
    }
}

impl std::ops::Rem<Length> for IndexPart {
    type Output = Self;

    // I guess this operation should be doing generation checking, which would imply that
    // `Length` should store the generation, and more importantly, this operation could fail.
    // Let's see if that actually becomes a problem though I guess? If it does we could avoid
    // that by making this a function that takes the container so it is known that the `Length`
    // is the correct genetration.
    fn rem(self, modulus: Length) -> Self::Output {
        Self(self.0 % modulus.0)
    }
}

impl std::ops::RemAssign<Length> for IndexPart {
    fn rem_assign(&mut self, modulus: Length) {
        *self = *self % modulus;
    }
}

// It could be argued that this should do generation checking, but it could also be agued that
// you should be allowed to compare to old lengths assuming you know what yoa are doing. We'll
// see if it ecomes an issue I guess.
impl std::cmp::PartialOrd<Length> for IndexPart {
    fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl std::cmp::PartialEq<Length> for IndexPart {
    fn eq(&self, other: &Length) -> bool {
        self.partial_cmp(other)
            .map(|o| o == std::cmp::Ordering::Equal)
            .unwrap_or(false)
    }
}

/// The type of invalidation that caused the index to need another generation. We keep track
/// of this so that we can auto-fix the indexes from one generation ago, when possible.
/// `RemovedAt(d!())` is a reasonable default because it results is a fixup of no change at all
/// which is correct for the first instance.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Invalidation {
    RemovedAt(IndexPart),
}

d!(for Invalidation: Invalidation::RemovedAt(d!()));

#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct State {
    current: Generation,
    invalidation: Invalidation,
}

impl State {
    fn advance(&mut self, invalidation: Invalidation) {
        *self = State {
            current: self.current.wrapping_add(1),
            invalidation,
        };
    }
    pub fn removed_at(&mut self, index: Index) {
        self.removed_at_index_part(index.index);
    }
    pub fn removed_at_index_part(&mut self, index: IndexPart) {
        self.advance(Invalidation::RemovedAt(index));
    }

    /// Attempt to convert an index from a given gerneation to the current generation.
    pub fn migrate(self, index: Index) -> Option<Index> {
        index.get_index_part(self).map(|i| self.new_index(i))
    }
    pub fn new_index(&self, index: IndexPart) -> Index {
        Index {
            generation: self.current,
            index,
        }
    }
}

#[derive(Clone, Copy, Default, Debug, Hash)]
/// A generational index
pub struct Index {
    generation: Generation,
    /// 4 billion what-zits ought to be enough for anybody!
    index: IndexPart,
}
ord!(and friends for Index: index, other in {
    index.generation.cmp(&other.generation).then_with(|| index.index.cmp(&other.index))
});

impl std::cmp::PartialOrd<IndexPart> for Index {
    fn partial_cmp(&self, other: &IndexPart) -> Option<std::cmp::Ordering> {
        Some(self.index.cmp(&other))
    }
}

impl std::cmp::PartialEq<IndexPart> for Index {
    fn eq(&self, other: &IndexPart) -> bool {
        self.partial_cmp(other)
            .map(|o| o == std::cmp::Ordering::Equal)
            .unwrap_or(false)
    }
}

impl Index {
    pub fn get(self, state: State) -> Option<usize> {
        self.get_index_part(state).map(|IndexPart(i)| i as usize)
    }
    fn get_index_part(self, state: State) -> Option<IndexPart> {
        if self.generation == state.current {
            Some(self.index)
        } else if self.generation == state.current.wrapping_sub(1) {
            match state.invalidation {
                Invalidation::RemovedAt(i) => {
                    use std::cmp::Ordering::*;
                    // Imagine the vec looks like this:
                    // `vec![10, 11, 12, 13, 14]`.
                    // and that we called `v.remove(2)` so now it looks like:
                    // `vec![10, 11, 13, 14]`.
                    // If you wanted `12` you can't have it, but if you wanted `10` or `11`
                    // your index is valid as is. Finally, if you wanted `13` or `14` then your
                    // index needs to be shifted down by one.
                    match self.index.cmp(&i) {
                        Equal => None,
                        Less => Some(self.index),
                        Greater => (self.index.0).checked_sub(1).map(IndexPart),
                    }
                }
            }
        } else {
            // insert your own joke about people > 40 years older than yourself here.
            None
        }
    }
}

impl macros::SaturatingAdd<usize> for Index {
    type Output = Self;

    fn saturating_add(mut self, rhs: usize) -> Self::Output {
        self.index = self.index.saturating_add(rhs);

        self
    }
}

impl macros::SaturatingSub<usize> for Index {
    type Output = Self;

    fn saturating_sub(mut self, rhs: usize) -> Self::Output {
        self.index = self.index.saturating_sub(rhs);

        self
    }
}

impl From<Index> for usize {
    fn from(index: Index) -> Self {
        usize::from(index.index)
    }
}

impl From<Index> for Length {
    fn from(index: Index) -> Self {
        Length::from(index.index)
    }
}

impl std::ops::Rem<Length> for Index {
    type Output = Self;

    fn rem(mut self, modulus: Length) -> Self::Output {
        self.index %= modulus;
        self
    }
}

impl std::ops::RemAssign<Length> for Index {
    fn rem_assign(&mut self, modulus: Length) {
        *self = *self % modulus;
    }
}

impl std::cmp::PartialOrd<Length> for Index {
    fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
        Some(self.index.0.cmp(&other.0))
    }
}

impl std::cmp::PartialEq<Length> for Index {
    fn eq(&self, other: &Length) -> bool {
        self.partial_cmp(other)
            .map(|o| o == std::cmp::Ordering::Equal)
            .unwrap_or(false)
    }
}

// intended for tests only.
pub mod arb {
    use super::*;
    use proptest::prelude::{any, Strategy};

    impl State {
        pub fn new_removed_at(current: Generation, index_part: IndexPart) -> Self {
            State {
                current,
                invalidation: Invalidation::RemovedAt(index_part),
            }
        }
    }

    impl Index {
        pub fn new_from_parts(generation: Generation, index: IndexPart) -> Self {
            Index { generation, index }
        }
    }

    pub fn index_part() -> impl Strategy<Value = IndexPart> {
        any::<LengthSize>().prop_map(|i| IndexPart::or_max(i as _))
    }

    fn invalidation() -> impl Strategy<Value = Invalidation> {
        index_part().prop_map(Invalidation::RemovedAt)
    }

    pub fn state() -> impl Strategy<Value = State> {
        (any::<Generation>(), invalidation()).prop_map(|(current, invalidation)| State {
            current,
            invalidation,
        })
    }
}
