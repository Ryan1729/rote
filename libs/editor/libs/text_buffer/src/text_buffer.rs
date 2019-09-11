use crate::move_cursor::{
    forward, forward_n, get_next_selection_point, get_previous_selection_point,
};
use editor_types::{Cursor, SetPositionAction, Vec1};
use macros::{borrow, d, some_or, CheckedSub};
use panic_safe_rope::{ByteIndex, LineIndex, Rope, RopeLine, RopeSliceTrait};
use platform_types::{pos, AbsoluteCharOffset, CharOffset, Move, Position, ReplaceOrAdd};
use std::borrow::Borrow;
use std::cmp::{max, min};
use std::collections::VecDeque;

mod move_cursor;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Cursors {
    // Should be sorted in reverse order (positions later in a test file will have lower indexes)
    // and no two cursors should be overlapping or have overlapping highlight regions. The order
    // ensures that the edit don't screw the indexes up, and the lack of overlaps ensures that
    // edits don't overwrite each other in a single action.
    cursors: Vec1<Cursor>,
}

impl Cursors {
    pub fn new(mut cursors: Vec1<Cursor>) -> Self {
        cursors.sort();
        cursors.reverse();

        Cursors::merge_overlaps(&mut cursors);

        Cursors { cursors }
    }

    fn merge_overlaps(cursors: &mut Vec1<Cursor>) {
        let mut len;

        while {
            len = cursors.len();
            Cursors::merge_overlaps_once(cursors);

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

    /// Returns `None` iff the vec was empty.
    pub fn from_vec(cursors: Vec<Cursor>) -> Option<Self> {
        Vec1::try_from_vec(cursors).ok().map(Cursors::new)
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
}

borrow!(<Vec1<Cursor>> for Cursors : c in &c.cursors);

#[derive(Clone, Debug, Default)]
pub struct TextBuffer {
    rope: Rope,
    cursors: Cursors,
    history: VecDeque<Edit>,
    history_index: usize,
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

borrow!(<Vec1<Cursor>> for TextBuffer : b in b.cursors.borrow());

type OffsetPair = (Option<AbsoluteCharOffset>, Option<AbsoluteCharOffset>);

fn offset_pair(rope: &Rope, cursor: &Cursor) -> OffsetPair {
    (
        pos_to_char_offset(rope, &cursor.get_position()),
        cursor
            .get_highlight_position()
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

fn copy_string(rope: &Rope, range: AbsoluteCharOffsetRange) -> String {
    rope.slice(range.range())
        .map(|slice| {
            let s: String = slice.into();
            s
        })
        .unwrap_or_default()
}

impl TextBuffer {
    #[perf_viz::record]
    pub fn insert(&mut self, c: char) {
        self.insert_string(char_to_string(c));
    }

    #[perf_viz::record]
    pub fn insert_string(&mut self, s: String) {
        self.apply_edit(
            get_insert_edit(&self.rope, &self.cursors, |_| s.clone()),
            ApplyKind::Record,
        );
    }

    pub fn insert_at_each_cursor<F>(&mut self, func: F)
    where
        F: Fn(usize) -> String,
    {
        self.apply_edit(
            get_insert_edit(&self.rope, &self.cursors, func),
            ApplyKind::Record,
        );
    }

    #[perf_viz::record]
    pub fn delete(&mut self) {
        self.apply_edit(
            get_delete_edit(&self.rope, &self.cursors),
            ApplyKind::Record,
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

        self.apply_edit(edit, ApplyKind::Record);

        output
    }

    fn get_selections_and_cut_edit(&self) -> (Vec<String>, Edit) {
        let mut strings = Vec::with_capacity(self.cursors.len());

        let edit = get_cut_edit(&self.rope, &self.cursors);

        for range_edit in edit.range_edits.iter() {
            if let Some(RangeEdit { chars, .. }) = &range_edit.delete_range {
                strings.push(chars.to_owned());
            }
        }

        (strings, edit)
    }

    #[perf_viz::record]
    pub fn set_cursor<P: Borrow<Position>>(&mut self, position: P, replace_or_add: ReplaceOrAdd) {
        if let Some(cursors) = self.get_new_cursors(position, replace_or_add) {
            self.apply_cursor_edit(cursors);
        }
    }

    pub fn select_all(&mut self) {
        self.set_cursor(pos! {}, ReplaceOrAdd::Replace);
        self.extend_selection_for_all_cursors(Move::ToBufferEnd);
    }

    fn get_new_cursors<P: Borrow<Position>>(
        &self,
        position: P,
        replace_or_add: ReplaceOrAdd,
    ) -> Option<Vec1<Cursor>> {
        let position = position.borrow();

        nearest_valid_position_on_same_line(&self.rope, position).map(|p| match replace_or_add {
            ReplaceOrAdd::Replace => Vec1::new(Cursor::new(p)),
            ReplaceOrAdd::Add => {
                let mut cursors = self.cursors.get_cloned_cursors();
                cursors.push(Cursor::new(p));
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

    pub fn select_char_type_grouping<P: Borrow<Position>>(
        &mut self,
        position: P,
        replace_or_add: ReplaceOrAdd,
    ) {
        if let Some(mut new) = self.get_new_cursors(position, replace_or_add) {
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

    fn apply_cursor_edit(&mut self, new: Vec1<Cursor>) {
        // There is probably a way to save a copy here, by keeping the old one on the heap and
        // ref counting, but that seems overly complicated, given it has not been a problem so far.
        self.apply_edit(
            Change {
                old: self.cursors.clone(),
                new: Cursors::new(new),
            }
            .into(),
            ApplyKind::Record,
        );
    }

    pub fn tab_in(&mut self) {
        self.apply_edit(
            get_tab_in_edit(&self.rope, &self.cursors),
            ApplyKind::Record,
        );
    }

    pub fn tab_out(&mut self) {
        dbg!("TabOut not implemented");
    }

    #[perf_viz::record]
    fn apply_edit(&mut self, edit: Edit, kind: ApplyKind) {
        dbg!();
        // we assume that the edits are in the proper order so we won't mess up our indexes with our
        // own inserts and removals. I'm not positive that there being a single order that works
        // is possible for all possible edits, but in practice I think the edits we will actually
        // produce will work out. The tests should tell us if we're wrong!
        for range_edit in edit.range_edits.iter() {
            range_edit.apply(&mut self.rope);
        }

        self.cursors = edit.cursors.new.clone();

        match kind {
            ApplyKind::Record => {
                self.history.push_back(edit);
                self.history_index += 1;
            }
            ApplyKind::Playback => {}
        }
    }
}

#[perf_viz::record]
fn sort_cursors(cursors: Vec1<Cursor>) -> Vec1<Cursor> {
    let mut cursors = cursors.to_vec();

    cursors.sort();
    cursors.reverse();

    // This unwrap is fine because we knew it was a Vec1 at the start.
    Vec1::try_from_vec(cursors).unwrap()
}

/// Calls the `FnMut` once with a copy of each cursor and a reference to the same clone of the
/// `Rope`. Then the (potentially) modified cursors and another copy of the `original_cursors`
/// are wrapped up along with the returned `RangeEdit`s into the Edit.
#[perf_viz::record]
fn get_edit<F>(original_rope: &Rope, original_cursors: &Cursors, mut mapper: F) -> Edit
where
    F: FnMut(&mut Cursor, &mut Rope, usize) -> RangeEdits,
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
    let range_edits = cloned_cursors.mapped_mut(|c| {
        index -= 1;
        mapper(c, &mut cloned_rope, index)
    });
    perf_viz::end_record!("range_edits");

    perf_viz::record_guard!("construct result");
    Edit {
        range_edits,
        cursors: Change {
            new: Cursors::new(dbg!(cloned_cursors)),
            old: original_cursors.clone(),
        },
    }
}

fn get_standard_insert_range_edits(
    rope: &mut Rope,
    cursor: &mut Cursor,
    offset: AbsoluteCharOffset,
    chars: String,
    char_count: usize, //we take this a s a paam to support it being a const.
) -> RangeEdits {
    rope.insert(offset, &chars);
    move_cursor::right_n_times(&rope, cursor, char_count);

    let range = AbsoluteCharOffsetRange::new(offset, offset + char_count);

    RangeEdits {
        insert_range: Some(RangeEdit { chars, range }),
        ..d!()
    }
}

/// Returns an edit that, if applied, after deleting the highlighted region at each cursor if
/// there is one, inserts the given string at each of the cursors.
#[perf_viz::record]
fn get_insert_edit<F>(original_rope: &Rope, original_cursors: &Cursors, get_string: F) -> Edit
where
    F: Fn(usize) -> String,
{
    get_edit(
        original_rope,
        original_cursors,
        |cursor, rope, index| match offset_pair(original_rope, cursor) {
            (Some(o), highlight) if highlight.is_none() || Some(o) == highlight => {
                let s = get_string(index);
                let char_count = s.chars().count();

                get_standard_insert_range_edits(rope, cursor, o, s, char_count)
            }
            (Some(o1), Some(o2)) => {
                let s = get_string(index);

                let range_edit = delete_highlighted(rope, cursor, o1, o2);

                rope.insert(range_edit.range.min(), &s);
                let char_count = s.chars().count();
                move_cursor::right_n_times(&rope, cursor, char_count);

                let range = {
                    let min = range_edit.range.min();
                    AbsoluteCharOffsetRange::new(min, min + char_count)
                };

                RangeEdits {
                    insert_range: Some(RangeEdit { chars: s, range }),
                    delete_range: Some(range_edit),
                }
            }
            _ => d!(),
        },
    )
}

/// Returns an edit that, if applied, deletes the highlighted region at each cursor if there is one.
/// Otherwise the applying the edit will delete a single character at each cursor.
fn get_delete_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o), None) if o > 0 => {
                // Deleting the LF ('\n') of a CRLF ("\r\n") pair is a special case
                // where the cursor should not be moved backwards. Thsi is because
                // CR ('\r') and CRLF ("\r\n") both count as a single newline.
                // TODO would it better to just delete both at once? That seems like
                // it would require a moe comlicated special case elsewhere.
                let not_deleting_lf_of_cr_lf = {
                    o.checked_sub(CharOffset(2))
                        .and_then(|two_back| {
                            let mut chars = rope.slice(two_back..o)?.chars();

                            Some((chars.next()?, chars.next()?) != ('\r', '\n'))
                        })
                        .unwrap_or(true)
                };

                let delete_offset_range = AbsoluteCharOffsetRange::new(o - 1, o);
                let chars = copy_string(&rope, delete_offset_range);
                rope.remove(delete_offset_range.range());

                if not_deleting_lf_of_cr_lf {
                    move_cursor::directly(&rope, cursor, Move::Left);
                }

                RangeEdits {
                    delete_range: Some(RangeEdit {
                        chars,
                        range: delete_offset_range,
                    }),
                    ..d!()
                }
            }
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => RangeEdits {
                delete_range: Some(delete_highlighted(rope, cursor, o1, o2)),
                ..d!()
            },
            _ => d!(),
        }
    })
}

/// returns an edit that if applied will delete the highlighted region at each cursor if there is
/// one, and which does nothing otherwise.
fn get_cut_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => RangeEdits {
                delete_range: Some(delete_highlighted(rope, cursor, o1, o2)),
                ..d!()
            },
            _ => d!(),
        }
    })
}

const TAB_STR: &'static str = "    "; //four spaces
const TAB_STR_CHAR: char = ' ';
const TAB_STR_CHAR_COUNT: usize = 4; // this isn't const (yet?) TAB_STR.chars().count();

#[perf_viz::record]
fn get_tab_in_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        match offset_pair(original_rope, cursor) {
            (Some(o), highlight) if highlight.is_none() || Some(o) == highlight => {
                get_standard_insert_range_edits(
                    rope,
                    cursor,
                    o,
                    TAB_STR.to_owned(),
                    TAB_STR_CHAR_COUNT,
                )
            }
            (Some(o1), Some(o2)) => {
                let range = AbsoluteCharOffsetRange::new(o1, o2);

                let mut chars = String::with_capacity(range.max().0 - range.min().0);

                let line_indicies_op = line_indicies_touched_by(rope, range);

                let mut tab_insert_count = 0;
                for line_indicies in line_indicies_op {
                    let last_line_index = line_indicies.len() - 1;
                    for (i, index) in line_indicies.into_iter().enumerate() {
                        let line = some_or!(rope.line(index), continue);

                        let line_start = some_or!(rope.line_to_char(index), continue);
                        let relative_line_end = final_non_newline_offset_for_rope_line(line);
                        let line_end = line_start + relative_line_end;

                        let highlight_start_for_line = max(range.min(), line_start) - line_start;
                        let highlight_end_for_line = if line_end < range.max() {
                            relative_line_end
                        } else {
                            range.max() - line_start
                        };

                        let first_highlighted_non_white_space_offset: Option<CharOffset> =
                            get_first_non_white_space_offset_in_range(
                                line,
                                highlight_start_for_line..=highlight_end_for_line,
                            );

                        dbg!(
                            line,
                            line_start,
                            relative_line_end,
                            line_end,
                            range.min(),
                            range.max(),
                            highlight_start_for_line,
                            highlight_end_for_line,
                            first_highlighted_non_white_space_offset
                        );

                        let start = highlight_start_for_line.0;
                        let previous_offset = min(
                            first_highlighted_non_white_space_offset
                                .unwrap_or(CharOffset(usize::max_value())),
                            highlight_end_for_line,
                        );
                        let end = previous_offset.0 + TAB_STR_CHAR_COUNT;
                        for _ in start..end {
                            chars.push(TAB_STR_CHAR);
                        }
                        tab_insert_count += 1;

                        let o =
                            first_highlighted_non_white_space_offset.unwrap_or(relative_line_end);

                        let should_include_newlline =
                            highlight_end_for_line != relative_line_end || i != last_line_index;

                        let slice_end = if highlight_end_for_line >= relative_line_end
                            && should_include_newlline
                        {
                            line.len_chars()
                        } else {
                            highlight_end_for_line
                        };
                        let highlighted_line_after_whitespace: &str =
                            some_or!(line.slice(o..slice_end).and_then(|l| l.as_str()), continue);
                        dbg!(highlighted_line_after_whitespace);
                        chars.push_str(highlighted_line_after_whitespace);
                    }
                }

                let range_edit = delete_highlighted_no_cursor(rope, range);

                let range = range_edit
                    .range
                    .add_to_max(TAB_STR_CHAR_COUNT * tab_insert_count);

                rope.insert(range.min(), &chars);

                // We want the cursor to have the same orientation it started with.
                forward_n(rope, cursor.get_position(), TAB_STR_CHAR_COUNT).map(|p| {
                    cursor.set_position_custom(
                        p,
                        SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
                    );
                    cursor.sticky_offset += TAB_STR_CHAR_COUNT
                });
                cursor
                    .get_highlight_position()
                    .and_then(|p| forward_n(rope, p, TAB_STR_CHAR_COUNT))
                    .map(|p| cursor.set_highlight_position(p));

                dbg!(RangeEdits {
                    insert_range: Some(RangeEdit { chars, range }),
                    delete_range: Some(range_edit),
                })
            }
            _ => d!(),
        }
    })
}

fn line_indicies_touched_by(
    rope: &Rope,
    range: AbsoluteCharOffsetRange,
) -> Option<Vec1<LineIndex>> {
    let min_index = rope.char_to_line(range.min())?;
    let max_index = rope.char_to_line(range.max())?;

    let mut output = Vec1::new(min_index);

    if min_index == max_index {
        return Some(output);
    }

    output.extend((min_index.0 + 1..=max_index.0).map(LineIndex));
    Some(output)
}

fn get_first_non_white_space_offset_in_range<R: std::ops::RangeBounds<CharOffset>>(
    line: RopeLine,
    range: R,
) -> Option<CharOffset> {
    use std::ops::Bound::*;

    let skip = match range.start_bound() {
        Included(CharOffset(o)) => *o,
        Excluded(CharOffset(o)) => (*o).saturating_add(1),
        Unbounded => 0,
    };

    let take = match range.end_bound() {
        Included(CharOffset(o)) => *o,
        Excluded(CharOffset(o)) => (*o).checked_sub(1)?,
        Unbounded => usize::max_value(),
    } - skip;
    let final_offset = final_non_newline_offset_for_rope_line(line);

    for (i, c) in line
        .chars()
        .enumerate()
        .map(|(i, c)| (CharOffset(i), c))
        .skip(skip)
        .take(std::cmp::min(take, final_offset.0))
    {
        if !c.is_whitespace() {
            return Some(i);
        }

        if i + 1 > final_offset {
            break;
        }
    }

    None
}

/// returns `None` if the input position's line does not refer to a line in the `Rope`.
fn nearest_valid_position_on_same_line<P: Borrow<Position>>(rope: &Rope, p: P) -> Option<Position> {
    let p = p.borrow();

    final_non_newline_offset_for_line(rope, LineIndex(p.line)).map(|final_offset| Position {
        offset: min(p.offset, final_offset),
        ..*p
    })
}

/// returns a `RangeEdit` representing the deletion.
fn delete_highlighted(
    rope: &mut Rope,
    cursor: &mut Cursor,
    o1: AbsoluteCharOffset,
    o2: AbsoluteCharOffset,
) -> RangeEdit {
    let range = AbsoluteCharOffsetRange::new(o1, o2);

    let output = delete_highlighted_no_cursor(rope, range);

    cursor.set_position(char_offset_to_pos(&rope, range.min()).unwrap_or_default());

    output
}

/// returns the same thing as `delete_highlighted`
fn delete_highlighted_no_cursor(rope: &mut Rope, range: AbsoluteCharOffsetRange) -> RangeEdit {
    let chars = copy_string(rope, range);

    rope.remove(range.range());

    RangeEdit { chars, range }
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
    } else if
    // The rope library we are using treats these as line breaks, so we do too.
    // See also https://www.unicode.org/reports/tr14/tr14-32.html
    (last >= '\u{a}' && last <= '\r')
        || last == '\u{0085}'
        || last == '\u{2028}'
        || last == '\u{2029}'
    {
        len -= 1;
        return_if_0!();
    }

    len
}

#[perf_viz::record]
fn in_cursor_bounds<P: Borrow<Position>>(rope: &Rope, position: P) -> bool {
    let p = position.borrow();
    final_non_newline_offset_for_line(rope, LineIndex(p.line))
        .map(|l| p.offset <= l)
        .unwrap_or(false)
}

#[perf_viz::record]
fn pos_to_char_offset(rope: &Rope, position: &Position) -> Option<AbsoluteCharOffset> {
    Some(rope.line_to_char(LineIndex(position.line))? + position.offset)
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

impl<'rope> TextBuffer {
    pub fn chars(&'rope self) -> impl Iterator<Item = char> + 'rope {
        self.rope.chars()
    }
}

//
// Undo / Redo
//

mod absolute_char_offset_range {
    use super::AbsoluteCharOffset;

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub struct AbsoluteCharOffsetRange {
        min: AbsoluteCharOffset,
        max: AbsoluteCharOffset,
    }

    #[allow(dead_code)]
    impl AbsoluteCharOffsetRange {
        pub fn new(o1: AbsoluteCharOffset, o2: AbsoluteCharOffset) -> Self {
            let min = std::cmp::min(o1, o2);
            let max = std::cmp::max(o1, o2);

            AbsoluteCharOffsetRange { min, max }
        }

        pub fn range(&self) -> std::ops::Range<AbsoluteCharOffset> {
            self.min..self.max
        }

        pub fn min(&self) -> AbsoluteCharOffset {
            self.min
        }

        pub fn max(&self) -> AbsoluteCharOffset {
            self.max
        }

        pub fn add_to_min<A>(&self, min: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Add<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min + min, self.max)
        }

        pub fn add_to_max<A>(&self, max: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Add<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min, self.max + max)
        }
    }
}
use absolute_char_offset_range::AbsoluteCharOffsetRange;

#[derive(Clone, Default, Debug, PartialEq, Eq)]
struct Change<T> {
    old: T,
    new: T,
}

impl<T> std::ops::Not for Change<T> {
    type Output = Change<T>;

    fn not(self) -> Self::Output {
        let Change { old, new } = self;

        Change { old: new, new: old }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RangeEdit {
    chars: String,
    range: AbsoluteCharOffsetRange,
}

/// Some seemingly redundant information is stored here, for example the insert's range's maximum
/// is never read. But that information is needed if the edits are ever negated, which swaps the
/// insert and delete ranges.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
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

/// `range_edits` and the two `Vec1`s in `cursors` must all be the same length.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Edit {
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

impl TextBuffer {
    pub fn redo(&mut self) -> Option<()> {
        self.history.get(self.history_index).cloned().map(|edit| {
            self.apply_edit(edit, ApplyKind::Playback);
            self.history_index += 1;
        })
    }

    pub fn undo(&mut self) -> Option<()> {
        let new_index = self.history_index.checked_sub(1)?;
        self.history.get(dbg!(new_index)).cloned().map(|edit| {
            self.apply_edit(dbg!(!edit), ApplyKind::Playback);
            self.history_index = new_index;
        })
    }
}

//
// View rendering
//

impl TextBuffer {
    pub fn cursors(&self) -> &Vec1<Cursor> {
        self.borrow()
    }

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
mod test_macros;
#[cfg(test)]
mod tests;
