/// A module containg a Generational Index implementation
use macros::{d, fmt_debug, fmt_display, ord, SaturatingAdd, SaturatingSub};
pub use vec1::{Vec1, vec1};
pub use move_mod::Move;

use core::hash::{Hash};

pub type Generation = u32;
pub type LengthSize = u32;

/// The amount of elements in the collection using generational indexes. Not a valid index.
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Length(LengthSize);
fmt_debug!(for Length: Length(l) in "{}", l);
fmt_display!(for Length: Length(l) in "{}", l);

impl Length {
    /// This returns a `usize` to make comparing to usize lengths conveinient.
    #[must_use]
    pub const fn max_value() -> usize {
        LengthSize::max_value() as usize
    }

    /// This takes a `usize` to make creation from usize lengths, where we don't care about
    /// the maximum case, conveinient.
    #[must_use]
    pub fn or_max(len: usize) -> Self {
        #[allow(clippy::cast_possible_truncation)]
        // We explicitly saturate instead of truncate
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
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexPart(LengthSize);
fmt_debug!(for IndexPart: IndexPart(l) in "{}", l);
fmt_display!(for IndexPart: IndexPart(l) in "{}", l);

impl IndexPart {
    /// This returns a `usize` to make comparing to usize lengths conveinient.
    #[must_use]
    pub const fn max_value() -> usize {
        (LengthSize::max_value() - 1) as usize
    }

    /// This takes a `usize` to make creation from usize lengths, where we don't care about
    /// the maximum case, conveinient.
    #[must_use]
    pub fn or_max(i: usize) -> Self {
        #[allow(clippy::cast_possible_truncation)]
        // We explicitly saturate instead of truncate
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
        #[allow(clippy::cast_possible_truncation)]
        // This is fine since self.0 was a LengthSize already. Even if `rhs` 
        // exceeds `LengthSize::max_value()` we just get 0.
        // Assumes `LengthSize` is an unsigned type.
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

// It could be argued that this should do generation checking, but it could also be argued that
// you should be allowed to compare to old lengths assuming you know what you are doing. We'll
// see if it becomes an issue I guess.
impl std::cmp::PartialOrd<Length> for IndexPart {
    fn partial_cmp(&self, other: &Length) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

impl std::cmp::PartialEq<Length> for IndexPart {
    fn eq(&self, other: &Length) -> bool {
        self.partial_cmp(other)
            .map_or(false, |o| o == std::cmp::Ordering::Equal)
    }
}

/// The type of invalidation that caused the index to need another generation. We keep track
/// of this so that we can auto-fix the indexes from one generation ago, when possible.
/// `RemovedAt(d!())` is a reasonable default because it results is a fixup of no change at all
/// which is correct for the first instance.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum Invalidation {
    RemovedAt(IndexPart),
    SwappedAt(IndexPart, IndexPart),
    MovedTo(IndexPart, IndexPart),
}

d!(for Invalidation: Invalidation::RemovedAt(d!()));

#[derive(Clone, Copy, Default, Debug, Hash, Eq, PartialEq)]
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

    pub fn moved_to_or_ignore(&mut self, source: Index, target: Index) {
        self.moved_to_or_ignore_index_part(source.index, target.index);
    }
    pub fn moved_to_or_ignore_index_part(&mut self, source: IndexPart, target: IndexPart) {
        self.advance(Invalidation::MovedTo(source, target));
    }

    /// Attempt to convert an index from a given generation to the current generation.
    #[must_use]
    pub fn migrate(self, index: Index) -> Option<Index> {
        index.get_index_part(self).map(|i| self.new_index(i))
    }

    #[must_use]
    pub fn new_index(&self, index: IndexPart) -> Index {
        Index {
            generation: self.current,
            index,
        }
    }

    /// Equivalent to `new_index(IndexPart::or_max(i))`.
    #[must_use]
    pub fn new_index_or_max(&self, i: usize) -> Index {
        self.new_index(IndexPart::or_max(i))
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Debug, Hash)]
/// A generational index
pub struct Index {
    generation: Generation,
    /// 4 billion what-zits ought to be enough for anybody!
    index: IndexPart,
}
ord!(for Index: index, other in {
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
            .map_or(false, |o| o == std::cmp::Ordering::Equal)
    }
}

impl Index {
    #[must_use]
    pub fn get(self, state: State) -> Option<usize> {
        self.get_index_part(state).map(|IndexPart(i)| i as usize)
    }

    #[must_use]
    pub fn get_index_part(self, state: State) -> Option<IndexPart> {
        if self.generation == state.current {
            Some(self.index)
        } else if self.generation == state.current.wrapping_sub(1) {
            use Invalidation::*;
            use std::cmp::Ordering::*;
            match state.invalidation {
                RemovedAt(i) => {
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
                MovedTo(source, target) => match source.cmp(&target) {
                    Equal => Some(self.index),
                    Less => match self.index {
                        i if i == source => Some(target),
                        i if i > source && i <= target => Some(self.index.saturating_sub(1)),
                        _ => Some(self.index)
                    },
                    Greater => match self.index {
                        i if i == source => Some(target),
                        i if i >= target && i < source => Some(self.index.saturating_add(1)),
                        _ => Some(self.index)
                    },
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
            .map_or(false, |o| o == std::cmp::Ordering::Equal)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum SelectionMove {
    Left,
    Right,
    ToStart,
    ToEnd,
}

macro_rules! selection_move_to_num {
    ($m: expr) => {{
        use SelectionMove::*;
        match $m {
            Left => 0,
            Right => 1,
            ToStart => 2,
            ToEnd => 3,
        }
    }};
}

ord!(and friends for SelectionMove: (selection_move) {
    selection_move_to_num!(selection_move)
});

#[derive(Clone, Copy, Debug)]
pub enum SelectionAdjustment {
    Next,
    Previous,
    Move(SelectionMove),
}

ord!(and friends for SelectionAdjustment: (adjustment){
        use SelectionAdjustment::*;
        match adjustment {
            Next => 0,
            Previous => 1,
            Move(m) => 2 + selection_move_to_num!(m),
        }
});

#[macro_export]
macro_rules! svec1 {
    ($($element: expr),+) => {
        $crate::SelectableVec1::new_from_vec1(
            $crate::vec1![$($element),+]
        )
    }
}

/// This module exists so we can be sure that only the methods that mutate
/// SelectableVec1 can change its fields.
#[mut_methods::mut_methods]
mod selectable_vec1 {
    use macros::{d, u, some_or, SaturatingAdd, SaturatingSub};
    use crate::{Vec1, Length, Index, IndexPart, State, SelectionAdjustment, SelectionMove};
    use core::hash::{Hash, Hasher};

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

    impl<A> Clone for SelectableVec1<A>
    where
        A: Clone,
    {
        fn clone(&self) -> Self {
            Self {
                elements: self.elements.clone(),
                index_state: self.index_state,
                current_index: self.current_index,
            }
        }
    }
    
    impl<A> SelectableVec1<A> {
        /// This is useful to minimize the amount of element hashing needed
        /// if elements happen to be expensive to hash.
        pub fn non_element_hash<H: Hasher>(&self, state: &mut H) {
            self.index_state.hash(state);
            self.current_index.hash(state);
        }
    }

    impl <A: PartialEq> PartialEq for SelectableVec1<A> {
        fn eq(&self, other: &Self) -> bool {
            self.index_state == other.index_state
            && self.current_index == other.current_index
            && self.elements == other.elements
        }
    }

    impl <A: Eq> Eq for SelectableVec1<A> {}

    impl<A: Hash> Hash for SelectableVec1<A> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.non_element_hash(state);
            self.elements.hash(state);
        }
    }

    #[allow(clippy::len_without_is_empty)]
    // an is_empty method would always return false.
    impl<A> SelectableVec1<A> {
        #[must_use]
        pub fn new(inital_element: A) -> Self {
            Self {
                elements: Vec1::new(inital_element),
                index_state: d!(),
                current_index: d!(),
            }
        }

        #[must_use]
        pub fn new_from_vec1(elements: Vec1<A>) -> Self {
            Self {
                elements,
                index_state: d!(),
                current_index: d!(),
            }
        }
        
        /// Since there is always at least one element, this always returns at least 1.
        #[must_use]
        pub fn len(&self) -> Length {
            debug_assert!(self.elements.len() <= Length::max_value());
            Length::or_max(self.elements.len())
        }
    
        /// The index of the first element.
        #[must_use]
        pub fn first_index(&self) -> Index {
            self.index_state.new_index(IndexPart::or_max(0))
        }
    
        /// The index of the last element.
        #[must_use]
        pub fn last_index(&self) -> Index {
            let len: usize = self.len().into();
            self.index_state.new_index(IndexPart::or_max(len - 1))
        }

        /// The index of an element would have if one was immeadiately appended.
        #[must_use]
        pub fn append_index(&self) -> Index {
            let len: usize = self.len().into();
            self.index_state.new_index(IndexPart::or_max(len))
        }
    
        /// The index of the currectly selected element.
        #[must_use]
        pub fn current_index(&self) -> Index {
            self.current_index
        }

        /// The index of the currectly selected element. This is intended mainly for the purpose of displaying 
        /// the current index.
        #[must_use]
        pub fn current_index_part(&self) -> IndexPart {
            self.current_index.index
        }
    
        #[must_use]
        pub fn get_current_element(&self) -> &A {
            some_or!(
                self.get_current_element_or_none(),
                self.elements.first()
            )
        }
    
        #[must_use]
        pub fn get_current_element_mut(&mut self) -> &mut A {
            // We can't use the same thing as we do in `get_current_element` because of the 
            // borrow checker. This may eventually be resolved by non-lexical lifetimes.
            let i = self.current_index().get(self.index_state).unwrap_or(usize::max_value());
            if i < self.elements.len() {
                // This unwrap is valid because of the length check. Additionally, `set_current_index` 
                // doesn't set the value if it is invalid. There's also a proptest checking that this 
                // method doesn't panic when all of the &mut ation
                self.elements.get_mut(i).unwrap()
            } else {
                self.elements.first_mut()    
            }
        }
    
        #[must_use]
        pub fn get_current_element_or_none(&self) -> Option<&A> {
            self.get(self.current_index())
        }
    
        #[must_use]
        pub fn get_current_element_or_none_mut(&mut self) -> Option<&mut A> {
            self.get_mut(self.current_index())
        }
    
        #[must_use]
        pub fn get_mut(&mut self, index: Index) -> Option<&mut A> {
            index
                .get(self.index_state)
                .and_then(move |i| self.elements.get_mut(i))
        }
    
        #[must_use]
        pub fn get(&self, index: Index) -> Option<&A> {
            index
                .get(self.index_state)
                .and_then(|i| self.elements.get(i))
        }
    
        /// Only actually updates the current index if an element can be retreived with 
        /// the passed index. Returns `true` if the index actually changed, and `false`
        /// otherwise.
        pub fn set_current_index(&mut self, index: Index) -> bool {
            let mut valid = false;
            if let Some(index) = self.index_state.migrate(index) {
                self.current_index = index;
                valid = true;
            }
            valid
        }
    
        pub fn push_and_select_new(&mut self, element: A) {
            self.push(element);
    
            self.set_current_index(self.last_index());
        }
    
        fn push(&mut self, element: A) {
            let will_fit = self.elements.len() < Length::max_value();
            debug_assert!(will_fit);
            if will_fit {
                self.elements.push(element);
            }
        }
    
        pub fn adjust_selection(&mut self, adjustment: SelectionAdjustment) {
            u!{SelectionAdjustment}
            match adjustment {
                Next => { self.set_current_index(self.next_index()); },
                Previous => { self.set_current_index(self.previous_index()); },
                // move the selected element to the target location
                Move(selection_move) => {
                    u!{SelectionMove}
                    let target_index = match selection_move {
                        Left => self.previous_index(),
                        Right => self.next_index(),
                        ToStart => self.first_index(),
                        ToEnd => self.last_index(),
                    };

                    self.move_or_ignore(self.current_index(), target_index);
                },
            }
        }
    
        pub fn close_element(&mut self, index: Index) {
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
    
        #[must_use]
        pub fn next_index(&self) -> Index {
            self.next_index_from(self.current_index)
        }
    
        #[must_use]
        pub fn next_index_from(&self, index: Index) -> Index {
            (index.saturating_add(1)) % self.len()
        }
    
        #[must_use]
        pub fn previous_index(&self) -> Index {
            self.previous_index_from(self.current_index)
        }

        #[must_use]
        pub fn previous_index_no_wrap(&self) -> Option<Index> {
            Self::previous_index_from_no_wrap(self.current_index)
        }
    
        #[must_use]
        pub fn previous_index_from(&self, index: Index) -> Index {
            Self::previous_index_from_no_wrap(index)
                .unwrap_or_else(|| self.last_index())
        }

        #[must_use]
        fn previous_index_from_no_wrap(index: Index) -> Option<Index> {
            let i: usize = index.into();
            if i == 0 {
                None
            } else {
                Some(index.saturating_sub(1))
            }
        }
    
        /// ```
        /// # #[macro_use] extern crate g_i; fn main() {
        /// let mut v = svec1![0, 1, 2];
        /// let index_state = v.index_state();
        /// let i0 = index_state.new_index_or_max(0);
        /// let i2 = index_state.new_index_or_max(2);
        /// v.swap_or_ignore(i0, i2);
        /// assert_eq!(v.elements(), svec1![2, 1, 0].elements());
        /// # }
        /// ```
        pub fn swap_or_ignore(&mut self, index1: Index, index2: Index) {
            if index1 < self.len() && index2 < self.len() {
                if let Some((i1, i2)) = index1.get(self.index_state)
                    .and_then(|i1| {
                        index2.get(self.index_state)
                            .map(|i2| (i1, i2))
                    }) {
                    self.elements.swap(i1, i2);
                    self.index_state.swapped_at_or_ignore(index1, index2);
    
                    // We specifically do not swap (or we double swap if you prefer,)
                    // so that the selection will follow the swapping.
                    if self.current_index == index1 {
                        self.set_current_index(index1);
                    } else if self.current_index == index2 {
                        self.set_current_index(index2);
                    }
                }
            }
        }
    
        /// ```
        /// # #[macro_use] extern crate g_i; fn main() {
        /// let mut v = svec1![0, 1, 2];
        /// let index_state = v.index_state();
        /// let i0 = index_state.new_index_or_max(0);
        /// let i2 = index_state.new_index_or_max(2);
        /// v.move_or_ignore(i0, i2);
        /// assert_eq!(v.elements(), svec1![1, 2, 0].elements());
        /// # }
        /// ```
        pub fn move_or_ignore(&mut self, index1: Index, index2: Index) {
            if index1 < self.len() && index2 < self.len() {
                if let Some((i1, i2)) = index1.get(self.index_state)
                    .and_then(|i1| {
                        index2.get(self.index_state)
                            .map(|i2| (i1, i2))
                    }) {
    
                    if let Ok(moved_val) = self.elements.try_remove(i1) {
                        self.elements.insert(i2, moved_val);
    
                        self.index_state.moved_to_or_ignore(index1, index2);
    
                        if self.current_index == index1 {
                            self.set_current_index(index1);
                        } else if self.current_index == index2 {
                            self.set_current_index(self.next_index_from(index2));
                        }
                    }
                }
            }
        }
    
        /// ```
        /// # #[macro_use] extern crate g_i; fn main() {
        /// let mut v = svec1![0, 1, 2];
        /// let index_state = v.index_state();
        /// let i0 = index_state.new_index_or_max(0);
        /// assert_eq!(v.remove_if_present(i0), Some(0));
        /// assert_eq!(v.elements(), svec1![1, 2].elements());
        /// # }
        /// ```
        pub fn remove_if_present(&mut self, index: Index) -> Option<A> {
            let output = if index < self.len() {
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
            };
    
            if output.is_some() {
                self.set_current_index(
                    self.previous_index_no_wrap()
                    .and_then(|i| self.index_state.migrate(i))
                    .unwrap_or_else(||
                        // if the current index is zero and we remove it we end up pointing at the new
                        // first element. In this case, this is desired.
                        self.index_state.new_index(d!())
                    )
                );
            }
    
            output
        }

        #[must_use]
        pub fn index_state(&self) -> State {
            self.index_state
        }

        // Mutates the vec1 in place, replacing the elements with those produced
        // by applying the mapper to the elements of the `other`. The selected 
        // element will become the one that corresponded to the one from `other`.
        // This implies that indexes saved from interactions with `other` should
        // work with `self` afterwards, but ones previously obtained from 
        // interactions with `self` are not guarenteed to.
        #[perf_viz::record]
        pub fn replace_with_mapped<B, F>(&mut self, other: &SelectableVec1<B>, mut mapper: F)
        where
            F: FnMut(&B) -> A {
            
            let _ = self.elements.try_truncate(1);

            *self.elements.first_mut() = mapper(other.elements.first());
            
            let mut iter = other.elements.iter();
            iter.next(); // skip first

            for e in iter {
                self.elements.push(mapper(e));
            }

            self.index_state = other.index_state;
            self.current_index = other.current_index;
        }

        #[must_use]
        pub fn elements(&self) -> &Vec1<A> {
            &self.elements
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
                let i = self.index;
    
                self.index = self.index.saturating_add(1);
    
                (i, b)
            })
        }
    }
    
    impl<A> SelectableVec1<A> {
        #[must_use]
        pub fn iter(&self) -> std::slice::Iter<A> {
            self.elements.iter()
        }
    
        #[must_use]
        pub fn iter_with_indexes(&self) -> IterWithIndexes<A> {
            IterWithIndexes {
                index: self.first_index(),
                iter: self.iter(),
            }
        }
    }

    impl <A> IntoIterator for SelectableVec1<A> {
        type Item = (Index, A);
        type IntoIter = std::vec::IntoIter<Self::Item>;
    
        fn into_iter(self) -> Self::IntoIter {
            let mut index = self.index_state.new_index(d!());

            let v: Vec<_> = self.elements.into_iter().map(|a| {
                let i = index;

                index = index.saturating_add(1);

                (i, a)
            }).collect();
    
            v.into_iter()
        }
    }

    #[cfg(any(test, feature = "pub_arb"))]
    impl<A> SelectableVec1<A> {
        #[must_use]
        pub fn from_parts(
            elements: Vec1<A>,
            index_state: State,
            mut current_index: Index,
        ) -> Self {
            let len = Length::or_max(elements.len());
            if current_index.index >= len {
                current_index.index = IndexPart::or_max((len.0 - 1) as usize)
            }

            Self {
                elements,
                index_state,
                current_index,
            }
        }
    }
} 
pub use selectable_vec1::{SelectableVec1, IterWithIndexes};

#[cfg(not(feature = "fast_hash"))]
use std::collections::HashMap;

#[cfg(feature = "fast_hash")]
use fast_hash::Map as HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Map<A> {
    map: HashMap<IndexPart, A>,
    last_state: Option<State>,
}

impl <A> Map<A> {
    #[must_use]
    pub fn with_capacity(capacity: Length) -> Self {
        Map {
            map: HashMap::with_capacity_and_hasher(capacity.into(), d!()),
            last_state: d!(),
        }
    }

    #[must_use]
    pub fn len(&self) -> Length {
        debug_assert!(self.map.len() <= Length::max_value());
        Length::or_max(self.map.len())
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    #[must_use]
    pub fn get(&mut self, state: State, index: Index) -> Option<&A> {
        if let Some(i) = index.get_index_part(state) {
            // This is why we need a &mut self, and this is needed
            // to make at least one test pass.
            self.migrate_all(state); 
            self.map.get(&i)
        } else {
            None
        }
    }

    #[must_use]
    pub fn get_mut(&mut self, state: State, index: Index) -> Option<&mut A> {
        if let Some(i) = index.get_index_part(state) {
            self.migrate_all(state);
            self.map.get_mut(&i)
        } else {
            None
        }
    }

    pub fn insert(&mut self, state: State, current_index: Index, value: A) {
        if let Some(current_index) = current_index.get_index_part(state) {
            self.migrate_all(state);

            self.map.insert(current_index, value);
        }
    }

    pub fn migrate_all(&mut self, state: State) {
        let last_state = self.last_state;
        if Some(state) != last_state {
            // This intermediary `Vec` prevents keys being overwritten 
            // nd the values therefore lost.
            let mut entries: Vec<_> = Vec::with_capacity(self.map.len());
            let keys: Vec<_> = self.map.keys().cloned().collect();
            for old_key in keys {
                if let Some(new_key) = last_state.and_then(|s| {
                    state
                        .migrate(s.new_index(old_key))
                        .and_then(|i| i.get_index_part(state))
                }) {
                    if let Some(value) = self.map.remove(&old_key) {
                        entries.push((new_key, value));
                    }
                } else {
                    self.map.remove(&old_key);
                }
            }

            for (k, v) in entries {
                self.map.insert(k, v);
            }

            self.last_state = Some(state);
        }
    }
}

impl <A> IntoIterator for Map<A> {
    type Item = (Index, A);
    type IntoIter = std::collections::hash_map::IntoIter<Index, A>;

    fn into_iter(self) -> Self::IntoIter {
        let map: HashMap<_, _> = if let Some(state) = self.last_state {
            self.map.into_iter().map(|(part, a)| {
                (state.new_index(part), a)
            }).collect()
        } else {
            // if the last_state is none, that implies that no inserts were done.
            d!()
        };

        map.into_iter()
    }
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests;

