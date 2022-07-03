#![deny(unused)]
use core::cmp::{max, min};
use editor_types::{Cursor, SetPositionAction};
use panic_safe_rope::{Rope};
use vec1::{Vec1, vec1};
use macros::{d};
use rope_pos::{clamp_position, offset_pair};

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Cursors {
    // Should be sorted in reverse order (positions later in a test file will have lower indexes)
    // and no two cursors should be overlapping or have overlapping highlight regions. The order
    // ensures that the edit don't screw the indexes up, and the lack of overlaps ensures that
    // edits don't overwrite each other in a single action.
    cursors: Vec1<Cursor>,
}

#[macro_export]
macro_rules! curs {
    ($rope: expr, $($cursor_elements: expr),+ $(,)?) => (
        Cursors::new(&$rope, vec1![$($cursor_elements)+])
    );
}

impl Cursors {
    /// We require a rope parameter only so we can make sure the cursors are within the given
    /// rope's bounds.
    #[must_use]
    pub fn new(rope: &Rope, mut cursors: Vec1<Cursor>) -> Self {
        use std::cmp::Reverse;
        use editor_types::Position;
        use panic_safe_rope::LineIndex;

        // This pre-clamping is needed to make the sorting work as expected in
        // particular cases where the cursor is (somehow) past the end of the line.
        for c in cursors.iter_mut() {
            let pos = c.get_position();

            if let Some(line) = rope.line(LineIndex(pos.line)) {
                use panic_safe_rope::RopeSliceTrait;
                let final_offset = line.len_chars();
                if pos.offset > final_offset {
                    c.set_position(Position{ offset: final_offset, ..pos });
                }
            }
        }

        cursors.sort_by_key(|w| Reverse(*w));

        Self::clamp_vec_to_rope(&mut cursors, rope);

        Self { cursors }
    }

    #[must_use]
    pub fn mapped_ref<F, Out>(&self, mapper: F) -> Vec1<Out>
    where
        F: FnMut(&Cursor) -> Out,
    {
        self.cursors.mapped_ref(mapper)
    }

    #[must_use]
    pub fn borrow_cursors(&self) -> &Vec1<Cursor> {
        &self.cursors
    }

    #[must_use]
    #[perf_viz::record]
    pub fn get_cloned_cursors(&self) -> Vec1<Cursor> {
        self.cursors.clone()
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.cursors.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.cursors.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Cursor> {
        self.cursors.iter()
    }

    #[must_use]
    pub fn first(&self) -> &Cursor {
        self.cursors.first()
    }

    #[must_use]
    pub fn last(&self) -> &Cursor {
        self.cursors.last()
    }

    pub fn clamp_to_rope(&mut self, rope: &Rope) {
        Self::clamp_vec_to_rope(&mut self.cursors, rope);
    }

    fn clamp_vec_to_rope(cursors: &mut Vec1<Cursor>, rope: &Rope) {
        for cursor in cursors.iter_mut() {
            let (p_op, h_op) = offset_pair(rope, cursor);

            if h_op.is_none() {
                if let Some(h) = cursor.get_highlight_position() {
                    cursor.set_highlight_position(clamp_position(rope, h));
                }
            }

            if p_op.is_none() {
                let clamped = clamp_position(rope, cursor.get_position());
                cursor.set_position_custom(
                    clamped,
                    SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
                );

                if h_op.is_none() {
                    cursor.set_highlight_position(clamped);
                }
            }
        }

        Self::merge_overlaps(cursors);
    }

    /// Assumes that the cursors are sorted
    fn merge_overlaps(cursors: &mut Vec1<Cursor>) {
        let mut len;

        while {
            len = cursors.len();
            Self::merge_overlaps_once(cursors);

            len > cursors.len()
        } {}
    }

    fn merge_overlaps_once(cursors: &mut Vec1<Cursor>) {
        let mut keepers: Vec<Cursor> = Vec::with_capacity(cursors.len());
        for &cursor in cursors.iter() {
            if let Some(last) = keepers.last_mut() {
                use std::cmp::Ordering::*;

                #[derive(Copy, Clone, Debug)]
                enum MaxWas {
                    P,
                    H,
                }

                macro_rules! get_tuple {
                    ($cursor: ident) => {{
                        let p = $cursor.get_position();
                        let h = $cursor.get_highlight_position().unwrap_or(p);
                        match p.cmp(&h) {
                            o @ (Less | Equal) => (p, h, o),
                            Greater => (h, p, Greater),
                        }
                    }};
                }

                // We intuitively expect something called `c1` to be <= `c2`. Or at least the
                // opposite is counter-intuitive. Because the Vec1 should be in reverse order,
                // we swap the order here.
                let c1: Cursor = cursor;
                let c2: Cursor = *last;

                match (get_tuple!(c1), get_tuple!(c2)) {
                    ((c1_min, c1_max, c1_ordering), (c2_min, c2_max, c2_ordering))
                        // if they overlap
                        if (c1_min <= c2_min && c1_max >= c2_min)
                        || (c1_min <= c2_max && c1_max >= c2_max) =>
                    {
                        // The merged cursor should highlight the union of the areas highlighed by
                        // the two cursors.

                        let max_was = match (c1_max.cmp(&c2_max), c1_ordering, c2_ordering) {
                            (Greater | Equal, Greater, _)
                            | (Less, _, Greater) => MaxWas::P,
                            (Greater, Less | Equal, _)
                            | (Less, _, Less | Equal)
                            // If the two cursors are the same it doesn't matter
                            // which one we keep.
                            | (Equal, _, _) => MaxWas::H,
                        };

                        match max_was {
                            MaxWas::P => {
                                let mut merged = Cursor::new(max(c1_max, c2_max));
                                merged.set_highlight_position(min(c1_min, c2_min));
                                *last = merged;
                            }
                            MaxWas::H => {
                                let mut merged = Cursor::new(min(c1_min, c2_min));
                                merged.set_highlight_position(max(c1_max, c2_max));
                                *last = merged;
                            }
                        }
                    }
                    _ => keepers.push(cursor),
                }
            } else {
                keepers.push(cursor);
            }
        }

        // It's probably possible to write an in-place version of this function, or at least to
        // reuse the memory allocation but currently that seems like premature optimization.
        if let Ok(cs) = Vec1::try_from_vec(keepers) {
            *cursors = cs;
        }
    }

    pub fn reset_states(&mut self) {
        for c in self.cursors.iter_mut() {
            c.state = d!();
        }
    }

    pub fn collapse_to(&mut self, index: usize) {
        if let Some(&c) = self.cursors.get(index) {
            self.cursors = vec1![c];
        }
    }

    // TODO write a test that fails when we add a new field that isn't counted here.
    // A compile-time assert would be preferable, of course.
    #[must_use]
    pub fn size_in_bytes(&self) -> usize {
        use core::mem;
        mem::size_of_val(&self.cursors) +
        (self.cursors.len() * mem::size_of::<Cursor>())
    }
}

pub fn set_cursors(rope: &Rope, pointer: &mut Cursors, mut new: Cursors) {
    new.clamp_to_rope(rope);
    *pointer = new;
}

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests {
    use super::*;
    pub mod arb;
}

