/// A module containg a Generational Index implementation
use macros::{d, u, fmt_debug, fmt_display, ord, some_or, SaturatingAdd, SaturatingSub};
use crate::{Vec1};
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
    MovedTo(IndexPart, IndexPart),
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

    pub fn moved_to_or_ignore(&mut self, source: Index, target: Index) {
        self.moved_to_or_ignore_index_part(source.index, target.index);
    }
    pub fn moved_to_or_ignore_index_part(&mut self, source: IndexPart, target: IndexPart) {
        self.advance(Invalidation::MovedTo(source, target));
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
                MovedTo(source, target) => {
                    if source == target {
                        Some(self.index)
                    } else if source > target {
                        match self.index {
                            i if i == source => Some(target),
                            i if i >= target && i < source => Some(self.index.saturating_add(1)),
                            _ => Some(self.index)
                        }
                    } else { // source < target
                        match self.index {
                            i if i == source => Some(target),
                            i if i > source && i <= target => Some(self.index.saturating_sub(1)),
                            _ => Some(self.index)
                        }
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

/// This module exists so we can be sure that only the methods that mutate
/// SelectableVec1 can change its fields.
#[mut_methods::mut_methods]
mod selectable_vec1 {
    use super::*;

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
    
        /// Since there is always at least one element, this always returns at least 1.
        pub fn len(&self) -> Length {
            debug_assert!(self.elements.len() <= Length::max_value());
            Length::or_max(self.elements.len())
        }
    
        /// The index of the first element.
        pub fn first_index(&self) -> Index {
            self.index_state.new_index(IndexPart::or_max(0))
        }
    
        /// The index of the last element.
        pub fn last_index(&self) -> Index {
            let len: usize = self.len().into();
            self.index_state.new_index(IndexPart::or_max(len - 1))
        }
    
        /// The index of the currectly selected element.
        pub fn current_index(&self) -> Index {
            self.current_index
        }

        /// The index of the currectly selected element. This is intended mainly for the purpose of displaying 
        /// the current index.
        pub fn current_index_part(&self) -> IndexPart {
            self.current_index.index
        }
    
        pub fn get_current_element(&self) -> &A {
            some_or!(
                self.get_current_element_or_none(),
                self.elements.first()
            )
        }
    
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
    
        pub fn get_current_element_or_none(&self) -> Option<&A> {
            self.get(self.current_index())
        }
    
        pub fn get_current_element_or_none_mut(&mut self) -> Option<&mut A> {
            self.get_mut(self.current_index())
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
                Move(selection_move) => {
                    u!{SelectionMove}
                    let target_index = match selection_move {
                        Left => self.previous_index(),
                        Right => self.next_index(),
                        ToStart => self.first_index(),
                        ToEnd => self.last_index(),
                    };
            
                    self.move_or_ignore(
                        self.current_index,
                        target_index,
                    );
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
    
        pub fn next_index(&self) -> Index {
            self.next_index_from(self.current_index)
        }
    
        pub fn next_index_from(&self, index: Index) -> Index {
            (index.saturating_add(1)) % self.len()
        }
    
        pub fn previous_index(&self) -> Index {
            self.previous_index_from(self.current_index)
        }
    
        pub fn previous_index_from(&self, index: Index) -> Index {
            let i: usize = index.into();
            if i == 0 {
                self.last_index()
            } else {
                index.saturating_sub(1)
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
    
            output
        }
    
        pub fn index_state(&self) -> State {
            self.index_state
        }

        // Mutates the vec1 in place, replacing the elements with those produced
        // by applying the mapper to the output of the iterator, if the iterator
        // has exactly the same amount of elements as `self`. Otherwise this 
        // method does nothing. If the elements were replaced, then `true` is 
        // returned, otherwise `false` is.
        pub fn replace_with_mapped_or_ignore<B, Iter, F>(&mut self, iter: Iter, mut mapper: F) -> bool
        where
            Iter: std::iter::ExactSizeIterator<Item = B>,
            F: FnMut(B) -> A {
            if iter.len() != self.len().0 as usize {
                return false
            }
            
            let slice = self.elements.as_mut_slice();
            for (i, e) in iter.enumerate() {
                slice[i] = mapper(e);
            }

            true
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
    impl<A> SelectableVec1<A> {
        pub fn from_parts(
            elements: Vec1<A>,
            index_state: State,
            current_index: Index,
        ) -> Self {
            Self {
                elements,
                index_state,
                current_index,
            }
        }
    }
} 
pub use selectable_vec1::{SelectableVec1, IterWithIndexes};

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::{proptest, Just};
    use arb_macros::{arb_enum};
    use vec1::{vec1};

    macro_rules! g_i_s {
        (g $generation: literal mi $source: literal $target: literal) => {
            State { 
                current: $generation,
                invalidation: Invalidation::MovedTo(source, $target)
            }
        };
        (g $generation: literal ri $index: literal) => {
            State::new_removed_at($generation, IndexPart::or_max($index))
        };
        () => {
            g_i_s!(g 0 ri 0)
        };
    }
    
    impl State {
        pub fn new_removed_at(current: Generation, index_part: IndexPart) -> Self {
            State {
                current,
                invalidation: Invalidation::RemovedAt(index_part),
            }
        }
    }

    macro_rules! g_i_i {
        (g $generation: literal i $index: literal) => {
            Index::new_from_parts($generation, IndexPart::or_max($index))
        };
        () => {
            g_i_i!(g 0 i 0)
        };
    }

    impl Index {
        pub fn new_from_parts(generation: Generation, index: IndexPart) -> Self {
            Index { generation, index }
        }
    }

    macro_rules! g_i_as {
        ($($tokens: tt)*) => {{
            u!{SelectionAdjustment}
            u!{SelectionMove}

            vec![$($tokens)*]
        }}
    }

    macro_rules! g_i_mtk {
        (g $generation: literal i $index_part: literal ri $invalidation_index: literal [$($g_i_as_tokens: tt)*] ($(i)? $i1: literal, $(i)? $i2: literal)) => {
            (
                g_i_i!(g $generation i $index_part),
                g_i_s!(g $generation ri $invalidation_index),
                g_i_as![$($g_i_as_tokens)*],
                (g_i_i!(g $generation i $i1), g_i_i!(g $generation i $i2))
            )
        }
    }

    #[derive(Clone, Debug)]
    pub enum MutMethodSpec<A> {
        GetCurrentElementMut,
        GetCurrentElementOrNoneMut,
        GetMut(Index),
        SetCurrentIndex(Index),
        RemoveIfPresent(Index),
        MoveOrIgnore(Index, Index),
        SwapOrIgnore(Index, Index),
        CloseElement(Index),
        AdjustSelection(SelectionAdjustment),
        PushAndSelectNew(A),
    }
    d!(<A> for MutMethodSpec<A>: MutMethodSpec::GetCurrentElementMut);

    impl <A> MutMethodSpec<A> {
        fn apply(self, svec1: &mut SelectableVec1<A>) {
            use MutMethodSpec::*;
            match self {
                GetCurrentElementMut => {
                    svec1.get_current_element_mut();
                },
                GetCurrentElementOrNoneMut => {
                    svec1.get_current_element_or_none_mut();
                },
                GetMut(index) => {
                    svec1.get_mut(index);
                },
                SetCurrentIndex(index) => {
                    svec1.set_current_index(index);
                },
                PushAndSelectNew(a) => {
                    svec1.push_and_select_new(a);
                },
                AdjustSelection(adjustment) => {
                    svec1.adjust_selection(adjustment);
                },
                CloseElement(index) => {
                    svec1.close_element(index);
                },
                SwapOrIgnore(index1, index2) => {
                    svec1.swap_or_ignore(index1, index2);
                },
                MoveOrIgnore(index1, index2) => {
                    svec1.move_or_ignore(index1, index2);
                },
                RemoveIfPresent(index) => {
                    svec1.remove_if_present(index);
                }
            }
        }

        fn method_names() -> Vec<&'static str> {
            vec![
                "get_current_element_mut", 
                "get_current_element_or_none_mut",
                "get_mut",
                "set_current_index",
                "push_and_select_new",
                "adjust_selection",
                "close_element",
                "swap_or_ignore",
                "move_or_ignore",
                "remove_if_present",
            ]
        }
    }

    pub mod arb {
        use super::{MutMethodSpec::*, *};
        use proptest::prelude::{any, Strategy, prop_compose};

        use std::convert::TryInto;
    
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
                },
                MovedTo(_, _) => { 
                    (0..=max_index, 0..=max_index).prop_map(|(a, b)| {
                        Invalidation::MovedTo(
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
            pub fn index(max_index: LengthSize)
               ((i, _) in state_with_index(max_index))
               -> Index {
               i
            }
        }

        prop_compose!{
            pub fn vector_with_valid_index(max_len: LengthSize)
                (vector in proptest::collection::vec(any::<i32>(), 1usize..(max_len as _)))
                (
                    (i, s) in state_with_index({ 
                        let len: LengthSize = vector.len().try_into().unwrap();
                        len - 1u32
                    }),
                    v in Just(vector)
                )
                -> (Index, State, Vec<i32>) {
                    (i, s, v)
            }
        }

        pub type MoveTestKit = (Index, State, Vec<i32>, (Index, Index));

        prop_compose!{
            pub fn move_test_kit(max_len: LengthSize)
                                ((index, state, vector) in vector_with_valid_index(max_len))
                                ((s_i1, s_i2) in (0..vector.len(), 0..vector.len()), (i, s, v) in Just((index, state, vector)))
             -> MoveTestKit {
                (i, s, v, (s.new_index_or_max(s_i1), s.new_index_or_max(s_i2)))
            }
        }

        proptest!{    
            #[test]
            fn move_test_kit_always_returns_valid_indexes(
                (previous_index, state, vector, (swap_i1, swap_i2)) in move_test_kit(16)
            ) {
                vector.get(previous_index.get(state).expect("previous_index.get")).expect("vector.get with previous_index");
                vector.get(swap_i1.get(state).expect("swap_i1.get")).expect("vector.get with swap_i1");
                vector.get(swap_i2.get(state).expect("swap_i2.get")).expect("vector.get with swap_i2");
            }
        }

        prop_compose!{
            pub fn selectable_vec1_of_i32(max_len: LengthSize)
                                ((index, state, vector) in vector_with_valid_index(max_len))
                                ((current_index, index_state) in Just((index, state)), elements in Just(vec1::Vec1::try_from_vec(vector).unwrap()))
             -> SelectableVec1<i32> {
                SelectableVec1::from_parts(
                    elements,
                    index_state,
                    current_index,
                )
            }
        }

        arb_enum!{
            pub fn selection_move() -> SelectionMove {
                Left => Just(Left),
                Right => Just(Right),
                ToStart => Just(ToStart),
                ToEnd => Just(ToEnd),
            }
        }

        prop_compose!{
            pub fn selection_moves(max_len: LengthSize)
                    (vector in proptest::collection::vec(selection_move(), 0usize..(max_len as _)))
             -> Vec<SelectionMove> {
                vector
            }
        }

        arb_enum!{
            pub fn selection_adjustment() -> SelectionAdjustment {
                Next => Just(Next),
                Previous => Just(Previous),
                Move(_) => selection_move().prop_map(Move),
            }
        }

        prop_compose!{
            pub fn selection_adjustments(max_len: LengthSize)
                    (vector in proptest::collection::vec(selection_adjustment(), 0usize..(max_len as _)))
             -> Vec<SelectionAdjustment> {
                vector
            }
        }

        arb_enum!{
            pub fn mut_method_spec(max_index: LengthSize) -> MutMethodSpec<i32> as MutMethodSpec {
                GetCurrentElementMut => Just(GetCurrentElementMut),
                GetCurrentElementOrNoneMut => Just(GetCurrentElementOrNoneMut),
                GetMut(_) => index(max_index).prop_map(GetMut),
                SetCurrentIndex(_) => index(max_index).prop_map(SetCurrentIndex),
                RemoveIfPresent(_) => index(max_index).prop_map(RemoveIfPresent),
                MoveOrIgnore(_, _) => (index(max_index), index(max_index)).prop_map(|(i1, i2)| MoveOrIgnore(i1, i2)),
                SwapOrIgnore(_, _) => (index(max_index), index(max_index)).prop_map(|(i1, i2)| SwapOrIgnore(i1, i2)),
                CloseElement(_) => index(max_index).prop_map(CloseElement),
                AdjustSelection(SelectionAdjustment) => selection_adjustment().prop_map(AdjustSelection),
                PushAndSelectNew(_) => any::<i32>().prop_map(PushAndSelectNew),
            }
        }

        prop_compose!{
            pub fn mut_method_specs(max_len: LengthSize)
                    (vector in proptest::collection::vec(mut_method_spec(max_len), 0usize..(max_len as _)))
             -> Vec<MutMethodSpec<i32>> {
                vector
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
        (previous_index, mut state, mut vector, (swap_i1, swap_i2)): arb::MoveTestKit,
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
            kit in arb::move_test_kit(16),
        ) {
            every_previous_generation_index_works_after_a_swap_on(kit);
        }
    }

    #[test]
    fn every_previous_generation_index_works_after_a_swap_in_this_generated_case() {
        every_previous_generation_index_works_after_a_swap_on((
            d!(),
            d!(),
            vec![506961, 1698395105],
            (Index::new_from_parts(0, IndexPart::or_max(1)), d!())
        ));
    }

    fn every_previous_generation_index_works_after_a_move_to_on(
        (previous_index, mut state, mut vector, (source, target)): arb::MoveTestKit,
    ) {
        let previous_value = vector[previous_index.get(state).unwrap()];
        
        {
            let moved_val = vector.remove(source.get(state).unwrap());
            vector.insert(target.get(state).unwrap(), moved_val);
        }
        state.moved_to_or_ignore(source, target);

        let result_index = previous_index.get(state).unwrap();

        assert_eq!(
            vector[result_index],
            previous_value,
            "{:?} mapped to {:?} which corresponds to {} not {} in {:?}",
            previous_index,
            result_index,
            vector[result_index],
            previous_value,
            vector,
        );
    }

    proptest!{
        #[test]
        fn every_previous_generation_index_works_after_a_move_to(
            kit in arb::move_test_kit(16),
        ) {
            every_previous_generation_index_works_after_a_move_to_on(kit);
        }
    }

    #[test]
    fn every_previous_generation_index_works_after_a_move_to_in_this_reduced_case() {
        every_previous_generation_index_works_after_a_move_to_on(g_i_mtk!(
            g 0 i 1 ri 1 [10, 11] (i 0, i 1)
        ));
    }

    #[test]
    fn every_previous_generation_index_works_after_a_move_to_in_this_generated_case() {
        every_previous_generation_index_works_after_a_move_to_on(g_i_mtk!(
            g 59910 i 8 ri 1 [116927047, -350084188, 1481126173, -1185533403, 1753439307, 1329181082, -2060683224, -1418353247, -890176339, 927573044, -740644675, -1681590353]
            (i 9, i 10)
        ));
    }

    #[test]
    fn every_previous_generation_index_works_after_a_move_to_in_this_differently_reduced_case() {
        every_previous_generation_index_works_after_a_move_to_on(g_i_mtk!(
            g 0 i 2 ri 1 [30, 31, 32, 33, 34]
            (i 3, i 4)
        ));
    }

    fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_on<A: std::fmt::Debug>(
        mut s_vec1: SelectableVec1<A>,
        adjustments: Vec<SelectionAdjustment>,
    ) {
        // precondition
        assert!(s_vec1.get_current_element_or_none().is_some(), "precondition failed on {:?}", s_vec1);
    
        for (i, adjustment) in adjustments.into_iter().enumerate() {
            s_vec1.adjust_selection(adjustment);
    
            assert!(
                s_vec1.get_current_element_or_none().is_some(),
                "{:?} (index {}) caused get_current_element to return None",
                adjustment,
                i
            );
        }
    }
    
    proptest!{
        #[test]
        fn no_selection_adjustment_causes_getting_the_current_element_to_return_none(
            s_vec1 in arb::selectable_vec1_of_i32(16),
            adjustments in arb::selection_adjustments(16),
        ) {
            no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
                s_vec1,
                adjustments,
            )
        }
    }

    fn no_selection_adjustment_causes_getting_the_current_element_mut_to_panic_on<A: std::fmt::Debug>(
        mut s_vec1: SelectableVec1<A>,
        adjustments: Vec<SelectionAdjustment>,
    ) {
        // precondition: this doesn't panic
        s_vec1.get_current_element_mut();
    
        for (i, adjustment) in adjustments.into_iter().enumerate() {
            s_vec1.adjust_selection(adjustment);

            // if this doesn't panic, the test passes
            s_vec1.get_current_element_mut();
        }
    }
    
    proptest!{
        #[test]
        fn no_selection_adjustment_causes_getting_the_current_element_mut_to_panic(
            s_vec1 in arb::selectable_vec1_of_i32(16),
            adjustments in arb::selection_adjustments(16),
        ) {
            no_selection_adjustment_causes_getting_the_current_element_mut_to_panic_on(
                s_vec1,
                adjustments,
            )
        }
    }

    #[test]
    fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_in_this_generated_case() {
        let s_vec1 = SelectableVec1::from_parts(
            vec1![12654029],
            g_i_s!(g 39631998 ri 0),
            g_i_i!(g 39631998 i 0),
        );

        let adjustments = g_i_as![Next, Move(ToStart), Move(Right), Next, Move(Right), Next, Next, Move(ToEnd), Next, Previous, Next, Next, Next, Previous];
        
        no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
            s_vec1,
            adjustments,
        )
    }

    #[test]
    fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_in_this_reduced_case() {
        no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
            SelectableVec1::new(0),
            g_i_as![Next, Move(ToStart), Move(Right)],
        )
    }

    #[test]
    fn no_selection_adjustment_causes_getting_the_current_element_to_return_none_in_this_two_element_generated_case() {
        let s_vec1 = SelectableVec1::from_parts(
            vec1![919639994, -804550252],
            g_i_s!(g 0 ri 0),
            g_i_i!(g 0 i 1),
        );

        let adjustments = g_i_as![Next, Move(Left), Move(Right), Next, Previous, Move(ToEnd), Move(Right), Previous];
        
        no_selection_adjustment_causes_getting_the_current_element_to_return_none_on(
            s_vec1,
            adjustments,
        )
    }

    fn no_selection_move_causes_getting_the_current_element_mut_to_return_a_different_element_on(
        mut s_vec1: SelectableVec1<i32>,
        moves: Vec<SelectionMove>,
    ) {
        let previous_element = *s_vec1.get_current_element_mut();
    
        for (i, r#move) in moves.into_iter().enumerate() {
            s_vec1.adjust_selection(SelectionAdjustment::Move(r#move));

            assert_eq!(
                *s_vec1.get_current_element_mut(),
                previous_element,
                "{:?} (index {}) caused the current element to change",
                r#move,
                i
            );
        }
    }

    proptest!{
        #[test]
        fn no_selection_move_causes_getting_the_current_element_mut_to_return_a_different_element(
            s_vec1 in arb::selectable_vec1_of_i32(16),
            moves in arb::selection_moves(16),
        ) {
            no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
                s_vec1,
                moves,
            )
        }
    }

    fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
        mut s_vec1: SelectableVec1<i32>,
        moves: Vec<SelectionMove>,
    ) {
        let previous_element = *s_vec1.get_current_element();
    
        for (i, r#move) in moves.into_iter().enumerate() {
            s_vec1.adjust_selection(SelectionAdjustment::Move(r#move));

            assert_eq!(
                *s_vec1.get_current_element(),
                previous_element,
                "{:?} (index {}) caused the current element to change",
                r#move,
                i
            );
        }
    }

    proptest!{
        #[test]
        fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element(
            s_vec1 in arb::selectable_vec1_of_i32(16),
            moves in arb::selection_moves(16),
        ) {
            no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
                s_vec1,
                moves,
            )
        }
    }

    #[test]
    fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element_in_this_single_right_case() {
        let s_vec1 = SelectableVec1::from_parts(
            vec1![919639994, -804550252],
            g_i_s!(),
            g_i_i!(),
        );

        let moves = g_i_as![Right];

        no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
            s_vec1,
            moves,
        )
    }

    #[test]
    fn no_selection_move_causes_getting_the_current_element_to_return_a_different_element_in_this_larger_single_right_case() {
        let s_vec1 = SelectableVec1::from_parts(
            vec1![20, 21, 22, 23, 24],
            g_i_s!(),
            g_i_i!(g 0 i 4),
        );

        let moves = g_i_as![Right];

        no_selection_move_causes_getting_the_current_element_to_return_a_different_element_on(
            s_vec1,
            moves,
        )
    }

    fn no_mut_method_spec_causes_getting_the_current_element_mut_to_panic_on<A: std::fmt::Debug>(
        mut s_vec1: SelectableVec1<A>,
        specs: Vec<MutMethodSpec<A>>,
    ) {
        // precondition: this doesn't panic
        s_vec1.get_current_element_mut();
    
        for spec in specs.into_iter() {
            spec.apply(&mut s_vec1);

            // if this doesn't panic, the test passes
            s_vec1.get_current_element_mut();
        }
    }
    
    proptest!{
        #[test]
        fn no_mut_method_spec_causes_getting_the_current_element_mut_to_panic(
            s_vec1 in arb::selectable_vec1_of_i32(16),
            specs in arb::mut_method_specs(16),
        ) {
            no_mut_method_spec_causes_getting_the_current_element_mut_to_panic_on(
                s_vec1,
                specs,
            )
        }
    }

    #[test]
    fn all_the_mut_methods_are_at_least_claimed_to_be_tested() {
        assert_eq!(
            MutMethodSpec::<i32>::method_names(),
            super::selectable_vec1::MUT_METHODS.to_vec()
        );
    }
}

