/// A module containg a Generational Index implementation
use macros::{d, u, fmt_debug, fmt_display, ord, SaturatingAdd, SaturatingSub};
use crate::{vec1, Vec1, SelectionMove};
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
    SwappedAt(IndexPart, IndexPart),
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

    pub fn swapped_at_or_ignore(&mut self, index1: Index, index2: Index) {
        self.swapped_at_or_ignore_index_part(index1.index, index2.index);
    }
    pub fn swapped_at_or_ignore_index_part(&mut self, index1: IndexPart, index2: IndexPart) {
        self.advance(Invalidation::SwappedAt(index1, index2));
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
    /// Equivalent to `new_index(IndexPart::or_max(i))`.
    pub fn new_index_or_max(&self, i: usize) -> Index {
        self.new_index(IndexPart::or_max(i))
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
            use Invalidation::*;
            match state.invalidation {
                RemovedAt(i) => {
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
                SwappedAt(i1, i2) => {
                    match self.index {
                        i if i == i1 => Some(i2),
                        i if i == i2 => Some(i1),
                        _ => Some(self.index)
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

/// A Vec1 that uses `Index`es and has a notion that one of the elements is
/// "selected" or is "the current element". This "selected" element can be borrowed
/// and the operations that change the currently selected index and/or move the selected
/// element around are also made more convenient.
#[derive(Debug)]
pub struct SelectableVec1<A> {
    elements: Vec1<A>,
    index_state: State,
    current_index: Index,
}

impl<A> Default for SelectableVec1<A>
where
    A: Default,
{
    fn default() -> Self {
        Self::new(A::default())
    }
}

impl<A> SelectableVec1<A> {
    pub fn new(inital_element: A) -> Self {
        Self {
            elements: Vec1::new(inital_element),
            index_state: d!(),
            current_index: d!(),
        }
    }

    /// Since there is always at least one buffer, this always returns at least 1.
    pub fn len(&self) -> Length {
        debug_assert!(self.elements.len() <= Length::max_value());
        Length::or_max(self.elements.len())
    }

    /// The index of the first buffer.
    pub fn first_index(&self) -> Index {
        self.index_state.new_index(IndexPart::or_max(0))
    }

    /// The index of the last buffer.
    pub fn last_index(&self) -> Index {
        let len: usize = self.len().into();
        self.index_state.new_index(IndexPart::or_max(len - 1))
    }

    /// The index of the currectly selected buffer.
    pub fn current_index(&self) -> Index {
        self.current_index
    }

    pub fn get_mut(&mut self, index: Index) -> Option<&mut A> {
        index
            .get(self.index_state)
            .and_then(move |i| self.elements.get_mut(i))
    }

    pub fn get(&self, index: Index) -> Option<&A> {
        index
            .get(self.index_state)
            .and_then(|i| self.elements.get(i))
    }

    /// Only actually updates the current index if an element can be retreived with 
    /// the passed index. Returns `true` if the index actually changed, and `false`
    /// otherwise.
    pub fn set_current_index(&mut self, index: Index) -> bool {
        let valid = self.get(index).is_some();
        if valid {
            self.current_index = index;
        }
        valid
    }

    pub fn get_current_buffer(&self) -> Option<&A> {
        self.get(self.current_index())
    }

    pub fn get_current_buffer_mut(&mut self) -> Option<&mut A> {
        self.get_mut(self.current_index())
    }

    pub fn push(&mut self, element: A) {
        let will_fit = self.elements.len() < Length::max_value();
        debug_assert!(will_fit);
        if will_fit {
            self.elements.push(element);
        }
    }

    pub fn move_selected(&mut self, selection_move: SelectionMove) {
        u!{SelectionMove}
        let target_index = match selection_move {
            Left => self.previous_index(),
            Right => self.next_index(),
            ToStart => self.first_index(),
            ToEnd => self.last_index(),
        };

        self.swap_or_ignore(
            target_index,
            self.current_index,
        );
    }

    pub fn close_buffer(&mut self, index: Index) {
        self.remove_if_present(index);

        self.set_current_index(
            self.index_state
            .migrate(self.current_index)
            .or_else(|| self.index_state.migrate(self.previous_index()))
            .unwrap_or_else(||
                // if the current index is zero and we remove it we end up pointing at the new
                // first element. In this case, this is desired.
                self.index_state.new_index(d!())
            )
        );
    }

    pub fn select_next(&mut self) {
        self.set_current_index(self.next_index());
    }

    pub fn select_previous(&mut self) {
        self.set_current_index(self.previous_index());
    }

    pub fn next_index(&self) -> Index {
        (self.current_index.saturating_add(1)) % self.len()
    }

    pub fn previous_index(&self) -> Index {
        let current_buffer_index = self.current_index;
        let i: usize = current_buffer_index.into();
        if i == 0 {
            self.last_index()
        } else {
            current_buffer_index.saturating_sub(1)
        }
    }

    pub fn swap_or_ignore(&mut self, index1: Index, index2: Index) {
        if index1 < self.len() && index2 < self.len() {
            if let Some((i1, i2)) = index1.get(self.index_state)
                .and_then(|i1| {
                    index2.get(self.index_state)
                        .map(|i2| (i1, i2))
                }) {
                self.elements.swap(i1, i2);
                self.index_state.swapped_at_or_ignore(index1, index2);
            }
        }
    }

    pub fn remove_if_present(&mut self, index: Index) -> Option<A> {
        if index < self.len() {
            index.get(self.index_state).and_then(|i| {
                let output = self.elements.try_remove(i).ok();

                if output.is_some() {
                    // No reason to update the index state if we didn't remove anything.
                    self.index_state.removed_at(index);
                }

                output
            })
        } else {
            None
        }
    }

    pub fn index_state(&self) -> State {
        self.index_state
    }
}

pub struct IterWithIndexes<'iter, A> {
    index: Index,
    iter: std::slice::Iter<'iter, A>,
}

impl<'iter, A> Iterator for IterWithIndexes<'iter, A> {
    type Item = (Index, &'iter A);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|b| {
            let i = self.index.clone();

            self.index = self.index.saturating_add(1);

            (i, b)
        })
    }
}

impl<A> SelectableVec1<A> {
    pub fn iter(&self) -> std::slice::Iter<A> {
        self.elements.iter()
    }

    pub fn iter_with_indexes(&self) -> IterWithIndexes<A> {
        IterWithIndexes {
            index: d!(),
            iter: self.iter(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::{proptest, Just};
    use arb_macros::{arb_enum};
    
    pub mod arb {
        use super::*;
        use proptest::prelude::{any, Strategy, prop_compose, prop_oneof};

        use std::convert::TryInto;
    
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
    
        pub fn index_part(max_len: LengthSize) -> impl Strategy<Value = IndexPart> {
            (0..=max_len).prop_map(|i| IndexPart::or_max(i as _))
        }
    
        arb_enum!{
            fn invalidation(max_index: LengthSize) -> Invalidation {
                RemovedAt(_) => { index_part(max_index).prop_map(RemovedAt) },
                SwappedAt(_, _) => { 
                    (0..=max_index, 0..=max_index).prop_map(|(a, b)| {
                        Invalidation::SwappedAt(
                            IndexPart::or_max(a as usize),
                            IndexPart::or_max(b as usize)
                        )
                    })
                }
           }
        }
    
        pub fn state(max_index: LengthSize) -> impl Strategy<Value = State> {
            (any::<Generation>(), invalidation(max_index)).prop_map(|(current, invalidation)| State {
                current,
                invalidation,
            })
        }

        prop_compose!{
            pub fn state_with_index(max_index: LengthSize)
               (s in {state(max_index)}, i_p in index_part(max_index))
               -> (Index, State) {
               (Index::new_from_parts(s.current, i_p), s) 
            }
        }

        prop_compose!{
            pub fn vector_with_valid_index(max_len: LengthSize)
                (vector in proptest::collection::vec(any::<usize>(), 1usize..(max_len as _)))
                (
                    (i, s) in state_with_index({ 
                        let len: LengthSize = vector.len().try_into().unwrap();
                        len - 1u32
                    }),
                    v in Just(vector)
                )
                -> (Index, State, Vec<usize>) {
                    (i, s, v)
            }
        }

        pub type SwapTestKit = (Index, State, Vec<usize>, (Index, Index));

        prop_compose!{
            pub fn swap_test_kit(max_len: LengthSize)
                                ((index, state, vector) in vector_with_valid_index(max_len))
                                ((s_i1, s_i2) in (0..vector.len(), 0..vector.len()), (i, s, v) in Just((index, state, vector)))
             -> SwapTestKit {
                (i, s, v, (s.new_index_or_max(s_i1), s.new_index_or_max(s_i2)))
            }
        }

        proptest!{    
            #[test]
            fn swap_test_kit_always_returns_valid_indexes(
                (previous_index, state, vector, (swap_i1, swap_i2)) in swap_test_kit(16)
            ) {
                vector.get(previous_index.get(state).expect("previous_index.get")).expect("vector.get with previous_index");
                vector.get(swap_i1.get(state).expect("swap_i1.get")).expect("vector.get with swap_i1");
                vector.get(swap_i2.get(state).expect("swap_i2.get")).expect("vector.get with swap_i2");
            }
        }
    }

    #[test]
    fn the_10_to_14_example_mentioned_in_a_comment_is_accuracte() {
        let initial_vec = vec![10, 11, 12, 13, 14];
        let mut current_vec = initial_vec.clone();

        let mut state: State = d!();

        let index_for_11 = state.new_index_or_max(1);
        // precondition
        assert_eq!(current_vec[index_for_11.get(state).unwrap()], 11);
        
        let index_for_12 = state.new_index_or_max(2);
        // precondition
        assert_eq!(current_vec[index_for_12.get(state).unwrap()], 12);

        let index_for_13 = state.new_index_or_max(3);
        // precondition
        assert_eq!(current_vec[index_for_13.get(state).unwrap()], 13);

        current_vec.remove(2);
        // precondition
        assert_eq!(current_vec, vec![10, 11, 13, 14]);
        state.removed_at(index_for_12);

        assert_eq!(current_vec[index_for_11.get(state).unwrap()], 11);
        assert_eq!(index_for_12.get(state), None);
        assert_eq!(current_vec[index_for_13.get(state).unwrap()], 13);
    }

    fn every_previous_generation_index_works_after_a_swap_on(
        (previous_index, mut state, mut vector, (swap_i1, swap_i2)): arb::SwapTestKit,
    ) {
        let previous_value = vector[previous_index.get(state).unwrap()];

        vector.swap(swap_i1.get(state).unwrap(), swap_i2.get(state).unwrap());
        state.swapped_at_or_ignore(swap_i1, swap_i2);

        assert_eq!(
            vector[previous_index.get(state).unwrap()],
            previous_value,
            "{:?} mapped to {:?} which corresponds to {} not {} in {:?}",
            previous_index,
            previous_index.get(state).unwrap(),
            vector[previous_index.get(state).unwrap()],
            previous_value,
            vector,
        );
    }

    proptest!{
        #[test]
        fn every_previous_generation_index_works_after_a_swap(
            kit in arb::swap_test_kit(16),
        ) {
            every_previous_generation_index_works_after_a_swap_on(kit);
        }
    }

    #[test]
    fn every_previous_generation_index_works_after_a_swap_in_this_generated_case() {
        every_previous_generation_index_works_after_a_swap_on((
            d!(),
            d!(),
            vec![5069618756514270747, 16983951054639971746],
            (Index::new_from_parts(0, IndexPart::or_max(1)), d!())
        ));
    }
}

