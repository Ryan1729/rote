#![deny(unused_imports)]
use crate::{
    cursors::{Cursors, set_cursors},
};
use editor_types::{Cursor, SetPositionAction, cur};
use macros::{d};
use panic_safe_rope::{LineIndex, Rope};
use platform_types::{*};
use rope_pos::{AbsoluteCharOffsetRange, final_non_newline_offset_for_rope_line, offset_pair};

use std::cmp::{min, max};

pub fn apply<'rope, 'cursors>(mut applier: Applier, edit: &Edit) {
    // we assume that the edits are in the proper order so we won't mess up our indexes with our
    // own inserts and removals. I'm not positive that there being a single order that works
    // is possible for all possible edits, but in practice I think the edits we will actually
    // produce will work out. The tests should tell us if we're wrong!
    for range_edit in edit.range_edits.iter() {
        range_edit.apply(applier.rope);
    }

    applier.set_cursors(edit.cursors.new.clone());
}

#[derive(Debug)]
enum SpecialHandling {
    None,
}
d!(for SpecialHandling: SpecialHandling::None);

#[derive(Debug)]
enum PostDeltaShift {
    None,
}
d!(for PostDeltaShift: PostDeltaShift::None);

#[derive(Debug, Default)]
struct CursorPlacementSpec {
    offset: AbsoluteCharOffset,
    delta: isize,
    special_handling: SpecialHandling,
    post_delta_shift: PostDeltaShift,
}

/// Calls the `FnMut` once with a copy of each cursor and a reference to the same clone of the
/// `Rope`. Then the (potentially) modified cursors and another copy of the `original_cursors`
/// are wrapped up along with the returned `RangeEdit`s into the Edit.
#[perf_viz::record]
fn get_edit<F>(original_rope: &Rope, original_cursors: &Cursors, mut mapper: F) -> Edit
where
    F: FnMut(&Cursor, &mut Rope, usize) -> (RangeEdits, CursorPlacementSpec),
{
    perf_viz::start_record!("init cloning + sort");
    // We need to sort cursors, so our `range_edits` are in the right order, so we can go
    // backwards, when we apply them so our indexes don't get messed up but our own inserts
    // and deletes.
    // Should we just always maintin that the cursors in sorted order?
    let mut cloned_cursors = sort_cursors(original_cursors.get_cloned_cursors());
    let mut cloned_rope = original_rope.clone();
    perf_viz::end_record!("init cloning + sort");

    // the index needs to account for the order being from the high positions to the low positions
    perf_viz::start_record!("range_edits");
    let mut index = cloned_cursors.len();
    let mut specs = Vec::with_capacity(index);
    let range_edits = cloned_cursors.mapped_ref(|c| {
        index -= 1;
        let (edits, spec) = mapper(c, &mut cloned_rope, index);

        specs.push(spec);
        edits
    });

    let mut total_delta: isize = 0;
    for (
        i,
        CursorPlacementSpec {
            offset,
            delta,
            special_handling,
            post_delta_shift,
        },
    ) in specs.into_iter().enumerate().rev()
    {
        total_delta = total_delta.saturating_add(delta);

        // This is only correct if the `offset` doesn't have the sign bit set.
        // Plus overflow and so forth, but these are probably 64 bits so who cares?
        let mut signed_offset = offset.0 as isize + total_delta;
        match post_delta_shift {
            PostDeltaShift::None => {}
        }
        let o = AbsoluteCharOffset(if signed_offset > 0 {
            signed_offset as usize
        } else {
            0
        });

        let cursor = &mut cloned_cursors[i];

        let action = SetPositionAction::ClearHighlight;
        
    }

    perf_viz::end_record!("range_edits");

    perf_viz::record_guard!("construct result");
    Edit {
        range_edits,
        cursors: Change {
            new: Cursors::new(&cloned_rope, cloned_cursors),
            old: original_cursors.clone(),
        },
    }
}

/// Returns an edit that, if applied, deletes the highlighted region at each cursor if there is one.
/// Otherwise the applying the edit will delete a single character at each cursor.
pub fn get_delete_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);
        match offsets {
            (Some(o), None) if o > 0 => {
                let delete_offset_range = AbsoluteCharOffsetRange::new(o - 1, o);
                let chars = copy_string(&rope, delete_offset_range);
                rope.remove(delete_offset_range.range());

                let min = delete_offset_range.min();
                let max = delete_offset_range.max();
                (
                    RangeEdits {
                        delete_range: Some(RangeEdit {
                            chars,
                            range: delete_offset_range,
                        }),
                        ..d!()
                    },
                    CursorPlacementSpec {
                        offset: max,
                        delta: min.0 as isize - max.0 as isize,
                        ..d!()
                    },
                )
            }
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                let (range_edit, delete_offset, delete_delta) = delete_highlighted(rope, o1, o2);
                (
                    RangeEdits {
                        delete_range: Some(range_edit),
                        ..d!()
                    },
                    CursorPlacementSpec {
                        offset: delete_offset,
                        delta: delete_delta,
                        ..d!()
                    },
                )
            }
            _ => d!(),
        }
    })
}

pub fn extend_cursor_to_cover_line(c: &mut Cursor, rope: &Rope) {
    let position = c.get_position();
    let highlight_position = c.get_highlight_position_or_position();

    let min_line = min(position.line, highlight_position.line);
    let max_line = max(position.line, highlight_position.line) + 1;

    let has_no_next_line = rope.len_lines().0 <= max_line;
    
    if has_no_next_line {
        if let Some((previous_line, last_non_newline_offset)) = min_line
            .checked_sub(1)
            .and_then(|i| 
                rope.line(LineIndex(i))
                    .map(final_non_newline_offset_for_rope_line)
                    .map(|o| (i, o.0))
            ) {
            *c = dbg!(cur!{pos!{l previous_line, o last_non_newline_offset}, pos!{l max_line, o 0}});
            return
        }
    }

    *c = dbg!(cur!{pos!{l min_line, o 0}, pos!{l max_line, o 0}});
}

/// Returns an edit that, if applied, deletes the line(s) each cursor intersects with.
pub fn get_delete_lines_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    let mut extended_cursors = original_cursors.get_cloned_cursors();
    for c in extended_cursors.iter_mut() {
        extend_cursor_to_cover_line(c, original_rope);
    }

    let mut edit = get_delete_edit(original_rope, &Cursors::new(original_rope, extended_cursors));

    edit.cursors.old = original_cursors.clone();

    edit
}

/// `range_edits` and the two `Vec1`s in `cursors` must all be the same length.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Edit {
    range_edits: Vec1<RangeEdits>,
    cursors: Change<Cursors>,
}

impl From<Change<Cursors>> for Edit {
    fn from(cursors: Change<Cursors>) -> Edit {
        Edit {
            range_edits: cursors.new.mapped_ref(|_| d!()),
            cursors,
        }
    }
}

impl std::ops::Not for Edit {
    type Output = Edit;

    fn not(self) -> Self::Output {
        let reversed = self
            .range_edits
            .mapped(|r_e| !r_e)
            .into_vec()
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
        Edit {
            // This unwrap is fine because `self.range_edits` was a `Vec1`.
            range_edits: Vec1::try_from_vec(reversed).unwrap(),
            cursors: !self.cursors,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct RangeEdit {
    chars: String,
    range: AbsoluteCharOffsetRange,
}

/// Some seemingly redundant information is stored here, for example the insert's range's maximum
/// is never read. But that information is needed if the edits are ever negated, which swaps the
/// insert and delete ranges.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
struct RangeEdits {
    /// The characters to insert and a range that only the minimum of which is used as the
    /// insertion point. Applied second.
    insert_range: Option<RangeEdit>,
    /// The characters to delete and where to delete them from. Applied first.
    delete_range: Option<RangeEdit>,
}

impl RangeEdits {
    fn apply(&self, rope: &mut Rope) {
        if let Some(RangeEdit { range, .. }) = self.delete_range {
            rope.remove(range.range());
        }

        if let Some(RangeEdit {
            ref chars, range, ..
        }) = self.insert_range
        {
            rope.insert(range.min(), chars);
        }
    }
}

impl std::ops::Not for RangeEdits {
    type Output = RangeEdits;

    fn not(self) -> Self::Output {
        RangeEdits {
            insert_range: self.delete_range,
            delete_range: self.insert_range,
        }
    }
}

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Change<T> {
    pub old: T,
    pub new: T,
}

impl<T> std::ops::Not for Change<T> {
    type Output = Change<T>;

    fn not(self) -> Self::Output {
        let Change { old, new } = self;

        Change { old: new, new: old }
    }
}

#[macro_export]
macro_rules! change {
    //
    // Pattern matching
    //
    ($old: ident, $new: ident) => {
        Change { old: $old, new: $new }
    };
    (_, $new: ident) => {
        Change { old: _, new: $new }
    };
    ($old: ident, _) => {
        Change { old: $old, new: _ }
    };
    //
    // Initialization
    //
    ($old: expr, $new: expr) => {
        Change { old: $old, new: $new }
    };
}

#[perf_viz::record]
fn sort_cursors(cursors: Vec1<Cursor>) -> Vec1<Cursor> {
    let mut cursors = cursors.to_vec();

    cursors.sort();
    cursors.reverse();

    // This unwrap is fine because we knew it was a Vec1 at the start.
    Vec1::try_from_vec(cursors).unwrap()
}

/// returns a `RangeEdit` representing the deletion. and the relevant cursor positioning info
fn delete_highlighted(
    rope: &mut Rope,
    o1: AbsoluteCharOffset,
    o2: AbsoluteCharOffset,
) -> (RangeEdit, AbsoluteCharOffset, isize) {
    let range = AbsoluteCharOffsetRange::new(o1, o2);

    delete_within_range(rope, range)
}

/// returns the same thing as `delete_highlighted`
fn delete_within_range(
    rope: &mut Rope,
    range: AbsoluteCharOffsetRange,
) -> (RangeEdit, AbsoluteCharOffset, isize) {
    let chars = copy_string(rope, range);

    rope.remove(range.range());

    let min = range.min();
    let max = range.max();
    (
        RangeEdit { chars, range },
        max,
        min.0 as isize - max.0 as isize,
    )
}

pub fn copy_string(rope: &Rope, range: AbsoluteCharOffsetRange) -> String {
    rope.slice(range.range())
        .map(|slice| {
            let s: String = slice.into();
            s
        })
        .unwrap_or_default()
}

/// We want to ensure that the cursors are always kept within bounds, meaning the whenever they
/// are changed they need to be clamped to the range of the rope. But we want to have a different
/// module handle the actual changes. So we give the pass an instance of this struct which allows
/// editing the rope and cursors in a controlled fashion.
// TODO given we've moved this into the `edit` module, does this still make sense?
pub struct Applier<'rope, 'cursors> {
    pub rope: &'rope mut Rope,
    cursors: &'cursors mut Cursors,
}

impl<'rope, 'cursors> Applier<'rope, 'cursors> {
    pub fn new(rope: &'rope mut Rope, cursors: &'cursors mut Cursors) -> Self {
        Applier {
            rope,
            cursors,
        }
    }

    fn set_cursors(&mut self, new: Cursors) {
        set_cursors(self.rope, self.cursors, new);
    }
}

