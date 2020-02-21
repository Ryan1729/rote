use crate::move_cursor::{forward, get_next_selection_point, get_previous_selection_point};
use editor_types::{Cursor, SetPositionAction, Vec1};
use macros::{d, some_or, CheckedSub};
use panic_safe_rope::{ByteIndex, LineIndex, Rope, RopeLine, RopeSlice, RopeSliceTrait};
use platform_types::*;
use std::borrow::Borrow;
use std::cmp::{max, min};
use std::collections::VecDeque;

mod edit;
mod move_cursor;

use edit::{AbsoluteCharOffsetRange, Edit};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
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
    /// We require a rope paramter only so we can make sure the cursors are within the given
    /// rope's bounds.
    pub fn new(rope: &Rope, mut cursors: Vec1<Cursor>) -> Self {
        cursors.sort();
        cursors.reverse();

        Self::clamp_vec_to_rope(&mut cursors, rope);

        Self { cursors }
    }

    pub fn mapped_ref<F, Out>(&self, mapper: F) -> Vec1<Out>
    where
        F: FnMut(&Cursor) -> Out,
    {
        self.cursors.mapped_ref(mapper)
    }

    #[perf_viz::record]
    pub fn get_cloned_cursors(&self) -> Vec1<Cursor> {
        self.cursors.clone()
    }

    pub fn len(&self) -> usize {
        self.cursors.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Cursor> {
        self.cursors.iter()
    }

    pub fn first(&self) -> &Cursor {
        self.cursors.first()
    }

    pub fn last(&self) -> &Cursor {
        self.cursors.last()
    }

    pub fn clamp_to_rope(&mut self, rope: &Rope) {
        Self::clamp_vec_to_rope(&mut self.cursors, rope)
    }

    fn clamp_vec_to_rope(cursors: &mut Vec1<Cursor>, rope: &Rope) {
        for cursor in cursors.iter_mut() {
            let (p_op, h_op) = strict_offset_pair(rope, cursor);

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
        for cursor in cursors.iter() {
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
                        let h = $cursor.get_highlight_position().unwrap_or(p.clone());
                        match p.cmp(&h) {
                            o @ Less | o @ Equal => (p, h, o),
                            Greater => (h, p, Greater),
                        }
                    }};
                }

                // We intuitively expect something called `c1` to be <= `c2`. Or at least the
                // opposite is counter-intuitive. Because the Vec1 should be in reverse order,
                // we swap the order here.
                let c1: Cursor = cursor.clone();
                let c2: Cursor = (*last).clone();

                match (get_tuple!(c1), get_tuple!(c2)) {
                    ((c1_min, c1_max, c1_ordering), (c2_min, c2_max, c2_ordering))
                        // if they overlap
                        if (c1_min <= c2_min && c1_max >= c2_min)
                        || (c1_min <= c2_max && c1_max >= c2_max) =>
                    {
                        // The merged cursor should highlight the union of the areas highlighed by
                        // the two cursors.

                        let max_was = match (c1_max.cmp(&c2_max), c1_ordering, c2_ordering) {
                            (Greater, Greater, _) => MaxWas::P,
                            (Greater, Less, _)|(Greater, Equal, _) => MaxWas::H,
                            (Less, _, Greater) => MaxWas::P,
                            (Less, _, Less)|(Less, _, Equal) => MaxWas::H,
                            (Equal, Greater, _) => MaxWas::P,
                            // If the two cursors are the same it doesn't matter which one we keep.
                            (Equal, _, _) => MaxWas::H
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
                    _ => keepers.push(cursor.clone()),
                }
            } else {
                keepers.push(cursor.clone());
            }
        }

        // It's probably possible to write an in-place version of this function, or at least to
        // reuse the memory allocation but currently that seems like premature optimization.
        if let Ok(cs) = Vec1::try_from_vec(keepers) {
            *cursors = cs;
        }
    }

    fn reset_states(&mut self) {
        for c in self.cursors.iter_mut() {
            c.state = d!();
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct TextBuffer {
    rope: Rope,
    cursors: Cursors,
    history: VecDeque<Edit>,
    history_index: usize,
}

impl TextBuffer {
    pub fn len(&self) -> usize {
        self.borrow_rope().chars().count()
    }

    pub fn borrow_rope(&self) -> &Rope {
        &self.rope
    }

    pub fn borrow_cursors(&self) -> &Cursors {
        &self.cursors
    }

    pub fn borrow_cursors_vec(&self) -> &Vec1<Cursor> {
        &self.cursors.cursors
    }

    pub fn borrow_cursors_vec1(&self) -> &Vec1<Cursor> {
        &self.cursors.cursors
    }

    pub fn reset_cursor_states(&mut self) {
        self.cursors.reset_states();
    }
}

impl From<String> for TextBuffer {
    fn from(s: String) -> Self {
        let mut output: Self = d!();

        output.rope = Rope::from(s);

        output
    }
}

impl From<&str> for TextBuffer {
    fn from(s: &str) -> Self {
        let mut output: Self = d!();

        output.rope = Rope::from(s);

        output
    }
}

impl From<&TextBuffer> for String {
    fn from(t_b: &TextBuffer) -> Self {
        t_b.rope.clone().into()
    }
}

impl From<&mut TextBuffer> for String {
    fn from(t_b: &mut TextBuffer) -> Self {
        t_b.rope.clone().into()
    }
}


/// A `haystack_range` of `None` means use the whole haystack. AKA no limit.
/// A `max_needed` of `None` means return all the results. AKA no limit.
pub fn get_search_ranges(
    needle: RopeSlice,
    haystack: &Rope,
    haystack_range: Option<AbsoluteCharOffsetRange>,
    max_needed: Option<std::num::NonZeroUsize>,
) -> Vec<(Position, Position)> {
    perf_viz::record_guard!("get_search_ranges");
    let (min, slice) = haystack_range
        .and_then(|r| haystack.slice(r.range()).map(|s| (r.min().0, s)))
        .unwrap_or_else(|| (0, haystack.full_slice()));

    let offsets = get_search_ranges_impl(needle, slice, max_needed);

    perf_viz::start_record!("offsets.into_iter() char_offset_to_pos");
    let output = offsets
        .into_iter()
        .filter_map(|(o_min, o_max)| {
            let a_o_min = AbsoluteCharOffset(o_min.0 + min);
            let a_o_max = AbsoluteCharOffset(o_max.0 + min);

            char_offset_to_pos(haystack, a_o_min)
                .and_then(|p_min| char_offset_to_pos(haystack, a_o_max).map(|p_max| (p_min, p_max)))
        })
        .collect();
    perf_viz::end_record!("offsets.into_iter() char_offset_to_pos");
    output
}

fn get_search_ranges_impl(
    needle: RopeSlice,
    haystack: RopeSlice,
    max_needed: Option<std::num::NonZeroUsize>,
) -> Vec<(CharOffset, CharOffset)> {
    perf_viz::record_guard!("get_search_ranges_impl");
    // Two-way string matching based on http://www-igm.univ-mlv.fr/~lecroq/string/node26.html

    macro_rules! max_suffix {
        ($name: ident, >) => {
            max_suffix!($name, a, b, a > b)
        };
        ($name: ident, <) => {
            max_suffix!($name, a, b, a < b)
        };
        ($name: ident, $a: ident, $b: ident, $test: expr) => {
            fn $name(rope: &RopeSlice) -> Option<(isize, isize)> {
                perf_viz::record_guard!(stringify!($name));
                let len = rope.len_chars().0 as isize;
                let mut ms: isize = -1;
                let mut j: isize = 0;
                let mut p: isize = 1;
                let mut k: isize = p;

                while j + k < len {
                    let $a = rope.char(CharOffset((j + k) as usize))?;
                    let $b = rope.char(CharOffset((ms + k) as usize))?;
                    if $test {
                        j += k;
                        k = 1;
                        p = j - ms;
                    } else {
                        if $a == $b {
                            if k != p {
                                k += 1;
                            } else {
                                j += p;
                                k = 1;
                            }
                        } else {
                            ms = j;
                            j = ms + 1;
                            p = 1;
                            k = p;
                        }
                    }
                }

                Some((ms, p))
            }
        };
    }

    /* Computing of the maximal suffix for <= */
    max_suffix!(max_suffix_le, <);
    /* Computing of the maximal suffix for >= */
    max_suffix!(max_suffix_ge, >);

    let mut output = Vec::new();
    let needle_len = needle.len_chars().0 as isize;
    let haystack_len = haystack.len_chars().0 as isize;
    macro_rules! push {
        ($start_offset: expr) => {
            let start_offset = $start_offset as usize;
            let end_offset = start_offset + needle_len as usize;

            output.push((CharOffset(start_offset), CharOffset(end_offset)));
            if let Some(max_needed) = max_needed {
                if max_needed.get() >= output.len() {
                    return output;
                }
            }
        };
    }

    /* Two Way string matching algorithm. */
    //void TW(char *x, int m, char *y, int n) {
    let ell;
    let mut period;
    {
        /* Preprocessing */
        match (max_suffix_le(&needle), max_suffix_ge(&needle)) {
            (Some((i, p)), Some((j, q))) => {
                if i > j {
                    ell = i;
                    period = p;
                } else {
                    ell = j;
                    period = q;
                }
            }
            (le, ge) => {
                if cfg!(debug_assertions) {
                    panic!("le {:?}, ge {:?}", le, ge);
                } else {
                    return output;
                }
            }
        }
    }

    macro_rules! opt_iters_match_once {
        ($method: ident : $op_iter1: expr, $op_iter2: expr) => {
            match (&mut $op_iter1, &mut $op_iter2) {
                (Some(ref mut i1), Some(ref mut i2)) => match (i1.$method(), i2.$method()) {
                    (Some(e1), Some(e2)) if e1 == e2 => true,
                    (None, None) => true,
                    _ => false,
                },
                (None, None) => true,
                _ => false,
            }
        };
    }

    /* Searching */
    let period_matches = {
        let mut start_chars = needle.chars();
        let mut period_chars = needle.chars().skip(period as usize);
        let mut matches = true;
        for _ in 0..(ell + 1) {
            match (start_chars.next(), period_chars.next()) {
                (Some(e1), Some(e2)) if e1 == e2 => {}
                (None, None) => {}
                _ => {
                    matches = false;
                    break;
                }
            }
        }
        matches
    };

    // this avoids trapping the ropey `Chars` in a `Skip` struct, so we can still call `prev`.
    macro_rules! get_chars_at {
        ($rope: expr, $n: expr) => {{
            perf_viz::record_guard!("get_chars_at");
            use std::convert::TryInto;
            $n.try_into()
                .ok()
                .map(CharOffset)
                .and_then(|offset| $rope.chars_at(offset))
        }};
    }

    let mut i: isize;
    let mut j: isize = 0;
    if period_matches {
        let mut memory: isize = -1;
        while j <= haystack_len - needle_len {
            i = std::cmp::max(ell, memory) + 1;
            {
                let mut n_chars = get_chars_at!(needle, i);
                let mut h_chars = get_chars_at!(haystack, i + j);
                while i < needle_len && opt_iters_match_once!(next: n_chars, h_chars) {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    let mut n_chars = get_chars_at!(needle, i + 1);
                    let mut h_chars = get_chars_at!(haystack, i + j + 1);
                    while i > memory && opt_iters_match_once!(prev: n_chars, h_chars) {
                        i -= 1;
                    }
                }
                if i <= memory {
                    push!(j);
                }
                j += period;
                memory = needle_len - period - 1;
            } else {
                j += i - ell;
                memory = -1;
            }
        }
    } else {
        perf_viz::record_guard!("!period_matches");
        period = std::cmp::max(ell + 1, needle_len - ell - 1) + 1;
        while j <= haystack_len - needle_len {
            i = ell + 1;
            {
                let mut n_chars = get_chars_at!(needle, i);
                let mut h_chars = get_chars_at!(haystack, i + j);
                while i < needle_len && opt_iters_match_once!(next: n_chars, h_chars) {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    let mut n_chars = get_chars_at!(needle, i + 1);
                    let mut h_chars = get_chars_at!(haystack, i + j + 1);

                    while i >= 0 && opt_iters_match_once!(prev: n_chars, h_chars) {
                        i -= 1;
                    }
                }
                if i < 0 {
                    push!(j);
                }
                j += period;
            } else {
                j += i - ell;
            }
        }
    }

    output
}

pub fn next_instance_of_selected(rope: &Rope, cursor: &Cursor) -> Option<(Position, Position)> {
    match offset_pair(rope, cursor) {
        (Some(p_offset), Some(h_offset)) => {
            let range = AbsoluteCharOffsetRange::new(p_offset, h_offset);
            let selected_text = rope.slice(range.range())?;

            get_search_ranges(
                selected_text,
                rope,
                Some(AbsoluteCharOffsetRange::new(range.max(), rope.len_chars())),
                std::num::NonZeroUsize::new(1),
            )
            .pop()
            .or_else(|| {
                get_search_ranges(
                    selected_text,
                    rope,
                    Some(AbsoluteCharOffsetRange::new(d!(), range.min())),
                    std::num::NonZeroUsize::new(1),
                )
                .pop()
            })
        }
        _ => None,
    }
}

type OffsetPair = (Option<AbsoluteCharOffset>, Option<AbsoluteCharOffset>);

/// This will return `Some` if the offset is one-past the last index.
// TODO do we actually need both of these? Specifically, will `strict_offset_pair` work everywhere?
fn offset_pair(rope: &Rope, cursor: &Cursor) -> OffsetPair {
    (
        pos_to_char_offset(rope, &cursor.get_position()),
        cursor
            .get_highlight_position()
            .and_then(|p| pos_to_char_offset(rope, &p)),
    )
}

/// This will return `None` if the offset is one-past the last index.
fn strict_offset_pair(rope: &Rope, cursor: &Cursor) -> OffsetPair {
    let filter_out_of_bounds =
        |position: Position| macros::some_if!(in_cursor_bounds(rope, position) => position);

    (
        Some(cursor.get_position())
            .and_then(filter_out_of_bounds)
            .and_then(|p| pos_to_char_offset(rope, &p)),
        cursor
            .get_highlight_position()
            .and_then(filter_out_of_bounds)
            .and_then(|p| pos_to_char_offset(rope, &p)),
    )
}

enum ApplyKind {
    Record,
    Playback,
}

enum AllOrOne {
    All,
    Index(usize),
}

enum MoveOrSelect {
    Move,
    Select,
}

struct CursorMoveSpec {
    what: MoveOrSelect,
    how_many: AllOrOne,
}

fn char_to_string(c: char) -> String {
    let mut buf = [0; 4];
    c.encode_utf8(&mut buf).to_owned()
}

impl TextBuffer {
    #[perf_viz::record]
    pub fn insert(&mut self, c: char) {
        self.insert_string(char_to_string(c));
    }

    #[perf_viz::record]
    pub fn insert_string(&mut self, s: String) {
        self.record_edit(
            edit::get_insert_edit(&self.rope, &self.cursors, |_| s.clone())
        );
    }

    pub fn insert_at_each_cursor<F>(&mut self, func: F)
    where
        F: Fn(usize) -> String,
    {
        self.record_edit(
            edit::get_insert_edit(&self.rope, &self.cursors, func)
        );
    }

    #[perf_viz::record]
    pub fn delete(&mut self) {
        self.record_edit(
            edit::get_delete_edit(&self.rope, &self.cursors)
        );
    }

    #[perf_viz::record]
    pub fn delete_lines(&mut self) {
        self.record_edit(
            edit::get_delete_lines_edit(&self.rope, &self.cursors)
        );
    }

    pub fn move_all_cursors(&mut self, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Move,
                how_many: AllOrOne::All,
            },
            r#move,
        );
    }

    pub fn extend_selection_for_all_cursors(&mut self, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Select,
                how_many: AllOrOne::All,
            },
            r#move,
        );
    }

    pub fn copy_selections(&self) -> Vec<String> {
        self.get_selections_and_cut_edit().0
    }

    pub fn cut_selections(&mut self) -> Vec<String> {
        let (output, edit) = self.get_selections_and_cut_edit();

        self.record_edit(edit);

        output
    }

    fn get_selections_and_cut_edit(&self) -> (Vec<String>, Edit) {
        let edit = edit::get_cut_edit(&self.rope, &self.cursors);

        (edit.selected(), edit)
    }

    #[perf_viz::record]
    pub fn set_cursor<C: Into<Cursor>>(&mut self, cursor: C, replace_or_add: ReplaceOrAdd) {
        if let Some(cursors) = self.get_new_cursors(cursor, replace_or_add) {
            self.apply_cursor_edit(cursors);
        }
    }

    pub fn select_all(&mut self) {
        self.set_cursor(pos! {}, ReplaceOrAdd::Replace);
        self.extend_selection_for_all_cursors(Move::ToBufferEnd);
    }

    fn get_new_cursors<C: Into<Cursor>>(
        &self,
        cursor: C,
        replace_or_add: ReplaceOrAdd,
    ) -> Option<Vec1<Cursor>> {
        let unclamped_cursor = cursor.into();

        let cursor: Option<Cursor> = match (
            nearest_valid_position_on_same_line(&self.rope, unclamped_cursor.get_position()),
            unclamped_cursor
                .get_highlight_position()
                .and_then(|h| nearest_valid_position_on_same_line(&self.rope, h)),
        ) {
            (Some(p), Some(h)) => Some((p, h).into()),
            (Some(p), None) => Some(p.into()),
            (None, _) => None,
        };

        cursor.map(|c| match replace_or_add {
            ReplaceOrAdd::Replace => Vec1::new(c),
            ReplaceOrAdd::Add => {
                let mut cursors = self.cursors.get_cloned_cursors();
                cursors.push(c);
                cursors
            }
        })
    }

    #[perf_viz::record]
    pub fn drag_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();

        if let Some(p) = nearest_valid_position_on_same_line(&self.rope, position) {
            let mut new = self.cursors.get_cloned_cursors();
            for c in new.iter_mut() {
                c.set_position_custom(p, SetPositionAction::OldPositionBecomesHighlightIfItIsNone);
            }
            self.apply_cursor_edit(new);
        }
    }

    /// Selects a grouping oof characters with a single character type, where the character types
    /// are as follows:
    /// * Word characters, as defined by the `regex` crate
    /// * Whitspace characters, again as defined by the `regex` crate
    /// * everything else, which we will call "Punctuation"
    /// (see get_offsets in move_cursor.rs for details)
    ///
    /// If it helps, you can think of it as "select_word" if the cursor is on a word, and other
    /// stuff otherwise
    pub fn select_char_type_grouping<C: Into<Cursor>>(
        &mut self,
        cursor: C,
        replace_or_add: ReplaceOrAdd,
    ) {
        if let Some(mut new) = self.get_new_cursors(cursor, replace_or_add) {
            let c = new.last_mut();

            let rope = &self.rope;
            let old_position = c.get_position();

            let highlight_position = get_previous_selection_point(
                rope,
                // Both of these `.unwrap_or(old_position)` calls are necessary.
                // If we were to use an `and_then` call instead, then we would default to
                // `old_position` too often
                forward(rope, old_position).unwrap_or(old_position),
            )
            .unwrap_or(old_position);
            c.set_highlight_position(highlight_position);

            let pos = get_next_selection_point(rope, old_position).unwrap_or(old_position);
            c.set_position_custom(
                pos,
                SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
            );

            self.apply_cursor_edit(new);
        }
    }

    #[perf_viz::record]
    pub fn move_cursor(&mut self, index: usize, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Move,
                how_many: AllOrOne::Index(index),
            },
            r#move,
        );
    }

    #[perf_viz::record]
    pub fn extend_selection(&mut self, index: usize, r#move: Move) {
        self.move_cursors(
            CursorMoveSpec {
                what: MoveOrSelect::Select,
                how_many: AllOrOne::Index(index),
            },
            r#move,
        );
    }

    fn move_cursors(&mut self, spec: CursorMoveSpec, r#move: Move) -> Option<()> {
        let mut new = self.cursors.get_cloned_cursors();

        let action: for<'r, 's> fn(&'r Rope, &'s mut Cursor, Move) = match spec.what {
            MoveOrSelect::Move => move_cursor::or_clear_highlights,
            MoveOrSelect::Select => move_cursor::and_extend_selection,
        };

        match spec.how_many {
            AllOrOne::All => {
                for cursor in new.iter_mut() {
                    action(&self.rope, cursor, r#move);
                }
            }
            AllOrOne::Index(index) => {
                action(&self.rope, new.get_mut(index)?, r#move);
            }
        };

        self.apply_cursor_edit(new);

        Some(())
    }

    /// It is important that all edits that only involve cursor changes go through here
    /// This is because they require special handling regarding undo/redo.
    fn apply_cursor_edit(&mut self, new: Vec1<Cursor>) {
        let change = edit::Change {
            old: self.cursors.clone(),
            new: Cursors::new(&self.rope, new),
        };

        self.apply_edit(
            change.clone().into(),
            // We don't record cursor movements for undo purposes
            // so you can undo, select, copy and then redo...
            ApplyKind::Playback,
        );
    }

    pub fn tab_in(&mut self) {
        self.record_edit(
            edit::get_tab_in_edit(&self.rope, &self.cursors),
        );
    }

    pub fn tab_out(&mut self) {
        self.record_edit(
            edit::get_tab_out_edit(&self.rope, &self.cursors),
        );
    }

    #[perf_viz::record]
    fn record_edit(&mut self, edit: Edit) {
        self.apply_edit(edit, ApplyKind::Record)
    }

    #[perf_viz::record]
    fn apply_edit(&mut self, edit: Edit, kind: ApplyKind) {
        let applier = EditApplier {
            rope: &mut self.rope,
            cursors: &mut self.cursors,
        };
        edit::apply(applier, &edit);

        match kind {
            ApplyKind::Record => {
                self.history.truncate(self.history_index);
                self.history.push_back(edit);
                self.history_index += 1;
            }
            ApplyKind::Playback => {}
        }
    }

    // some of these are convenience methods for tests
    #[allow(dead_code)]
    fn set_cursors(&mut self, new: Cursors) {
        set_cursors(&self.rope, &mut self.cursors, new);
    }

    #[allow(dead_code)]
    fn set_cursors_from_vec1(&mut self, cursors: Vec1<Cursor>) {
        self.cursors = Cursors::new(&self.rope, cursors);
    }
}

fn set_cursors(rope: &Rope, pointer: &mut Cursors, mut new: Cursors) {
    new.clamp_to_rope(rope);
    *pointer = new;
}

/// We want to ensure that the cursors are always kept within bounds, meaning the whenever they
/// are changed they need to be clamped to the range of the rope. But we want to have a different
/// module handle the actual changes. So we give the `edit` module an instance of this struct
/// which allows editing the rope and cursors in a controlled fashion.
pub struct EditApplier<'rope, 'cursors> {
    pub rope: &'rope mut Rope,
    cursors: &'cursors mut Cursors,
}

impl<'rope, 'cursors> EditApplier<'rope, 'cursors> {
    fn set_cursors(&mut self, new: Cursors) {
        set_cursors(self.rope, self.cursors, new);
    }
}

/// returns `None` if the input position's line does not refer to a line in the `Rope`.
fn nearest_valid_position_on_same_line<P: Borrow<Position>>(rope: &Rope, p: P) -> Option<Position> {
    let p = p.borrow();

    final_non_newline_offset_for_line(rope, LineIndex(p.line)).map(|final_offset| Position {
        offset: min(p.offset, final_offset),
        ..*p
    })
}

/// Returns `None` if that line is not in the `Rope`.
#[perf_viz::record]
fn final_non_newline_offset_for_line(rope: &Rope, line_index: LineIndex) -> Option<CharOffset> {
    rope.line(line_index)
        .map(final_non_newline_offset_for_rope_line)
}

#[perf_viz::record]
fn final_non_newline_offset_for_rope_line(line: RopeLine) -> CharOffset {
    final_non_newline_offset_for_rope_line_(line)
}
fn final_non_newline_offset_for_rope_line_(line: RopeLine) -> CharOffset {
    let mut len = line.len_chars();

    macro_rules! get_char_before_len {
        () => {
            if let Some(c) = line.char(len - 1) {
                c
            } else {
                // We know that the index we are passing in is less than `len_chars()`
                // so this case should not actually happen. But, we have a reasonable
                // value to return so why no just do that?
                return CharOffset(0);
            };
        };
    }

    macro_rules! return_if_0 {
        () => {
            if len == 0 {
                return CharOffset(0);
            }
        };
    }

    return_if_0!();

    let last = get_char_before_len!();

    if last == '\n' {
        len -= 1;

        return_if_0!();

        let second_last = get_char_before_len!();

        if second_last == '\r' {
            len -= 1;
            return_if_0!();
        }
    } else if is_linebreak_char(last)
    {
        len -= 1;
        return_if_0!();
    }

    len
}

pub fn is_linebreak_char(c: char) -> bool {
    // The rope library we are using treats these as line breaks, so we do too.
    // See also https://www.unicode.org/reports/tr14/tr14-32.html
    (c >= '\u{a}' && c <= '\r')
        || c == '\u{0085}'
        || c == '\u{2028}'
        || c == '\u{2029}'
}

#[perf_viz::record]
fn in_cursor_bounds<P: Borrow<Position>>(rope: &Rope, position: P) -> bool {
    let p = position.borrow();
    final_non_newline_offset_for_line(rope, LineIndex(p.line))
        .map(|l| p.offset <= l)
        .unwrap_or(false)
}

/// Returns `None` iff the position is not valid for the given rope.
#[perf_viz::record]
fn pos_to_char_offset(rope: &Rope, position: &Position) -> Option<AbsoluteCharOffset> {
    let line_index = LineIndex(position.line);

    let line_start = rope.line_to_char(line_index)?;
    let line = rope.line(line_index)?;
    let offset = position.offset;
    if offset == 0 || offset <= line.len_chars() {
        Some(line_start + offset)
    } else {
        None
    }
}

#[perf_viz::record]
fn char_offset_to_pos(rope: &Rope, offset: AbsoluteCharOffset) -> Option<Position> {
    if rope.len_chars() == offset {
        Some(LineIndex(rope.len_lines().0 - 1))
    } else {
        rope.char_to_line(offset)
    }
    .and_then(|line_index| {
        let start_of_line = rope.line_to_char(line_index)?;

        offset
            .checked_sub(start_of_line)
            .map(|o: CharOffset| Position {
                line: line_index.0,
                offset: o.into(),
            })
    })
}

fn clamp_position(rope: &Rope, position: Position) -> Position {
    clamp_position_helper(rope, position)
        .unwrap_or_else(|| char_offset_to_pos(rope, rope.len_chars()).unwrap_or_default())
}

fn clamp_position_helper(rope: &Rope, position: Position) -> Option<Position> {
    let line_index = LineIndex(position.line);

    let line_start = rope.line_to_char(line_index)?;
    let line = rope.line(line_index)?;
    let offset = position.offset;

    let abs_offset = line_start + min(offset, final_non_newline_offset_for_rope_line(line));

    let line_index = if rope.len_chars() == abs_offset {
        Some(LineIndex(rope.len_lines().0 - 1))
    } else {
        rope.char_to_line(abs_offset)
    };

    line_index.and_then(|line_index| {
        let start_of_line = rope.line_to_char(line_index)?;

        abs_offset
            .checked_sub(start_of_line)
            .map(|o: CharOffset| Position {
                line: line_index.0,
                offset: o.into(),
            })
    })
}

impl<'rope> TextBuffer {
    pub fn chars(&'rope self) -> impl Iterator<Item = char> + 'rope {
        self.rope.chars()
    }
}

impl TextBuffer {
    /// returns a `Some` if there was history to redo.
    pub fn redo(&mut self) -> Option<()> {
        self.history.get(self.history_index).cloned().map(|edit| {
            self.apply_edit(edit, ApplyKind::Playback);
            self.history_index += 1;
        })
    }

    /// returns a `Some` if there was history to undo.
    pub fn undo(&mut self) -> Option<()> {
        let new_index = self.history_index.checked_sub(1)?;
        self.history.get(new_index).cloned().map(|edit| {
            self.apply_edit(!edit, ApplyKind::Playback);
            self.history_index = new_index;
        })
    }
}

//
// View rendering
//

impl TextBuffer {
    pub fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        in_cursor_bounds(&self.rope, position)
    }

    #[perf_viz::record]
    pub fn find_index<P: Borrow<Position>>(&self, p: P) -> Option<ByteIndex> {
        let rope = &self.rope;
        pos_to_char_offset(rope, p.borrow()).and_then(|o| rope.char_to_byte(o))
    }
}

#[cfg(test)]
#[macro_use]
pub mod test_macros;
#[cfg(test)]
#[macro_use]
pub mod tests;