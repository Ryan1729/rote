use crate::move_cursor::{get_previous_selection_point, get_next_selection_point, forward};
use editor_types::{Cursor, SetPositionAction, Vec1};
use macros::{borrow, borrow_mut, d};
use panic_safe_rope::{Rope, RopeSliceTrait, RopeLine, LineIndex, ByteIndex};
use platform_types::{AbsoluteCharOffset, CharOffset, Move, Position, ReplaceOrAdd};
use std::borrow::Borrow;
use std::collections::VecDeque;

mod move_cursor;

pub type Cursors = Vec1<Cursor>;

#[derive(Default, Clone, Debug)]
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

borrow!(<Vec1<Cursor>> for TextBuffer : s in &s.cursors);
borrow_mut!(<Vec1<Cursor>> for TextBuffer : s in &mut s.cursors);

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
    let mut buf = [0;4];
    c.encode_utf8(&mut buf).to_owned()
}

fn copy_string(rope: &Rope, range: AbsoluteCharOffsetRange) -> String {
    rope.slice(range.range())
        .map(|slice| {let s: String = slice.into(); s})
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
            get_insert_edit(&self.rope, &self.cursors, s),
            ApplyKind::Record,
        );
    }

    #[perf_viz::record]
    pub fn delete(&mut self) {
        self.apply_edit(get_delete_edit(&self.rope, &self.cursors), ApplyKind::Record);
    }

    pub fn move_all_cursors(&mut self, r#move: Move) {
        self.move_cursors(CursorMoveSpec {
            what: MoveOrSelect::Move,
            how_many: AllOrOne::All,
        }, r#move);
    }

    pub fn extend_selection_for_all_cursors(&mut self, r#move: Move) {
        self.move_cursors(CursorMoveSpec {
            what: MoveOrSelect::Select,
            how_many: AllOrOne::All,
        }, r#move);
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
            if let Some(RangeEdit {chars, ..}) = &range_edit.delete_range {
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

    fn get_new_cursors<P: Borrow<Position>>(&self, position: P, replace_or_add: ReplaceOrAdd) -> Option<Cursors> {
        let position = position.borrow();

        nearest_valid_position_on_same_line(&self.rope, position).map(|p|{
            match replace_or_add {
                ReplaceOrAdd::Replace => {
                    Vec1::new(Cursor::new(p))
                }
                ReplaceOrAdd::Add => {
                    let mut cursors = self.cursors.clone();
                    cursors.push(Cursor::new(p));
                    cursors
                }
            }
        })
    }

    #[perf_viz::record]
    pub fn drag_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();

        if let Some(p) = nearest_valid_position_on_same_line(&self.rope, position) {
            let mut new = self.cursors.clone();
            for c in new.iter_mut() {
                c.set_position_custom(p, SetPositionAction::OldPositionBecomesHighlightIfItIsNone);
            }
            self.apply_cursor_edit(new);
        }
    }

    pub fn select_char_type_grouping<P: Borrow<Position>>(&mut self, position: P, replace_or_add: ReplaceOrAdd) {
        if let Some(mut new) = self.get_new_cursors(position, replace_or_add) {
            let c = new.last_mut();

            let rope = &self.rope;
            let old_position = c.get_position();

            let highlight_position =
            get_previous_selection_point(
                rope,
                // Both of these `.unwrap_or(old_position)` calls are necessary.
                // If we were to use an `and_then` call instead, then we would default to
                // `old_position` too often
                forward(rope, old_position).unwrap_or(old_position)
            ).unwrap_or(old_position);
            c.set_highlight_position(highlight_position);

            let pos =
                get_next_selection_point(rope, old_position).unwrap_or(old_position);
            c.set_position_custom(pos, SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition);

            self.apply_cursor_edit(new);
        }
    }

    #[perf_viz::record]
    pub fn move_cursor(&mut self, index: usize, r#move: Move) {
        self.move_cursors(CursorMoveSpec {
            what: MoveOrSelect::Move,
            how_many: AllOrOne::Index(index),
        }, r#move);
    }

    #[perf_viz::record]
    pub fn extend_selection(&mut self, index: usize, r#move: Move) {
        self.move_cursors(CursorMoveSpec {
            what: MoveOrSelect::Select,
            how_many: AllOrOne::Index(index),
        }, r#move);
    }

    fn move_cursors(&mut self, spec: CursorMoveSpec, r#move: Move) -> Option<()> {
        let mut new = self.cursors.clone();

        let action:
            for<'r, 's>
            fn(&'r Rope, &'s mut Cursor, Move)
         = match spec.what {
            MoveOrSelect::Move => move_cursor::or_clear_highlights,
            MoveOrSelect::Select => move_cursor::and_extend_selection,
        };

        match spec.how_many {
            AllOrOne::All => {
                for cursor in new.iter_mut() {
                    action(&self.rope, cursor, r#move);
                }
            },
            AllOrOne::Index(index) => {
                action(&self.rope, new.get_mut(index)?, r#move);
            }
        };

        self.apply_cursor_edit(new);

        Some(())
    }

    fn apply_cursor_edit(&mut self, new: Cursors) {
        // There is probably a way to save a copy here, by keeping the old one on the heap and
        // ref counting, but that seems overly complicated, given it has not been a problem so far.
        self.apply_edit(Change {old: self.cursors.clone(), new}.into(), ApplyKind::Record);
    }

    fn apply_edit(&mut self, edit: Edit, kind: ApplyKind) {
        // we assume that the edits are in the proper order so we won't mess up our indexes with our
        // own inserts and removeals. I'm not positive that there being a single order that works
        // is possible for all possible edits, but in practice I think the edits we will actually
        // produce will work out. The tests should tell us if we're wrong!
        for range_edit in edit.range_edits.iter() {
            if let Some(RangeEdit{range, ..}) = range_edit.delete_range {
                self.rope.remove(range.range());
            }

            if let Some(RangeEdit{ref chars, range, ..}) = &range_edit.insert_range {
                self.rope.insert(range.min(), chars);
            }
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

fn sort_cursors(cursors: Cursors) -> Cursors {
    let mut cursors = cursors.to_vec();

    cursors.sort();
    cursors.reverse();

    // This unwrap is fine because we knew it was a Vec1 at the start.
    Vec1::try_from_vec(cursors).unwrap()
}

/// Calls the `FnMut` once with a copy of each cursor and a reference to the same clone of the
/// `Rope`. Then the (potentially) modified cursors and another copy of the `original_cursors`
/// are wrapped up along with the returned `RangeEdit`s into the Edit.
fn get_edit<F>(original_rope: &Rope, original_cursors: &Cursors, mut mapper: F) -> Edit
where F: FnMut(&mut Cursor, &mut Rope) -> RangeEdits, {
    let mut cloned_rope = original_rope.clone();
    let mut cloned_cursors = sort_cursors(original_cursors.clone());

    // We need to sort cursors, so our `range_edits` are in the right order, so we can go
    // backwards, when we apply them so our indexes don't get messed up but our own inserts
    // and deletes.
    // Should we just always maintin that the cursors in sorted order?
    let range_edits = cloned_cursors.mapped_mut(|c| mapper(c, &mut cloned_rope));

    Edit {
        range_edits,
        cursors: Change {
            new: cloned_cursors,
            old: original_cursors.clone()
        }
    }
}

/// Returns an edit that, if applied, after deleting the highlighted region at each cursor if
/// there is one, inserts the given string at each of the cursors.
fn get_insert_edit(
    original_rope: &Rope,
    original_cursors: &Cursors,
    s: String
) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope| {
        match offset_pair(original_rope, cursor) {
            (Some(o), highlight)
                if highlight.is_none()
                    || Some(o) == highlight =>
            {
                rope.insert(o, &s);
                for _ in 0..s.len() {
                    move_cursor::directly(&rope, cursor, Move::Right);
                }

                RangeEdits {
                    insert_range: Some(RangeEdit {
                        chars: s.to_owned(),
                        range: AbsoluteCharOffsetRange::new(o, o + s.chars().count())
                    }),
                    ..d!()
                }
            }
            (Some(o1), Some(o2)) => {
                let range_edit = delete_highlighted(rope, cursor, o1, o2);

                rope.insert(range_edit.range.min(), &s);
                for _ in 0..s.len() {
                    move_cursor::directly(&rope, cursor, Move::Right);
                }

                RangeEdits {
                    insert_range: Some(RangeEdit {
                        chars: s.to_owned(),
                        range: {
                            let min = range_edit.range.min();
                            AbsoluteCharOffsetRange::new(min, min + s.chars().count())
                        }
                    }),
                    delete_range: Some(range_edit),
                }
            }
            _ => {
                d!()
            }
        }
    })
}

/// Returns an edit that, if applied, deletes the highlighted region at each cursor if there is one.
/// Otherwise the applying the edit will delete a single character at each cursor.
fn get_delete_edit(
    original_rope: &Rope,
    original_cursors: &Cursors
) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o), None) if o > 0 => {
                // Deleting the LF ('\n') of a CRLF ("\r\n") pair is a special case
                // where the cursor should not be moved backwards. Thsi is because
                // CR ('\r') and CRLF ("\r\n") both count as a single newline.
                // TODO would it better to just delete both at once? That seems like
                // it would require a moe comlicated special case elsewhere.
                let not_deleting_lf_of_cr_lf = {
                    o.checked_sub(AbsoluteCharOffset(2)).and_then(|two_back| {
                        let mut chars = rope.slice(two_back..o)?.chars();

                        Some(
                            (chars.next()?, chars.next()?) != ('\r', '\n')
                        )
                    }).unwrap_or(true)
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
                        range: delete_offset_range
                    }),
                    ..d!()
                }
            }
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                RangeEdits {
                    delete_range: Some(delete_highlighted(rope, cursor, o1, o2)),
                    ..d!()
                }
            }
            _ => {
                d!()
            }
        }
    })
}

/// returns an edit that if applied will delete the highlighted region at each cursor if there is
/// one, and which does nothing otherwise.
fn get_cut_edit(
    original_rope: &Rope,
    original_cursors: &Cursors
) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                RangeEdits {
                    delete_range: Some(delete_highlighted(rope, cursor, o1, o2)),
                    ..d!()
                }
            }
            _ => {
                d!()
            }
        }
    })
}

/// returns `None` if the input position's line does not refer to a line in the `Rope`.
fn nearest_valid_position_on_same_line<P: Borrow<Position>>(
    rope: &Rope,
    p: P,
) -> Option<Position> {
    let p = p.borrow();

    final_non_newline_offset_for_line(rope, p.line).map(|final_offset| Position {
        offset: std::cmp::min(p.offset, final_offset),
        ..*p
    })
}

/// returns a `RangeEdit` representing the deletion.
fn delete_highlighted(
    rope: &mut Rope,
    cursor: &mut Cursor,
    o1: AbsoluteCharOffset,
    o2: AbsoluteCharOffset
) -> RangeEdit {
    let range = AbsoluteCharOffsetRange::new(o1, o2);

    let chars = copy_string(rope, range);

    rope.remove(range.range());
    cursor.set_position(
        char_offset_to_pos(&rope, range.min()).unwrap_or_default(),
    );

    RangeEdit {
        chars,
        range
    }
}

/// Returns `None` if that line is not in the `Rope`.
fn final_non_newline_offset_for_line(rope: &Rope, line_index: usize) -> Option<CharOffset> {
    rope
        .lines()
        .nth(line_index)
        .map(final_non_newline_offset_for_rope_line)
}

fn final_non_newline_offset_for_rope_line(line: RopeLine) -> CharOffset {
    let mut len = line.len_chars();
    macro_rules! get_char_before_len {
        () => {
            if let Some(c) = line.char(len - 1) {
                c
            } else {
                // We know that the index we are passing in is less than `len_chars()`
                // so this case should not actually happen. But, we have a reasonalble
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

fn in_cursor_bounds<P: Borrow<Position>>(rope: &Rope, position: P) -> bool {
    let p = position.borrow();
    final_non_newline_offset_for_line(rope, p.line)
        .map(|l| p.offset <= l)
        .unwrap_or(false)
}

#[perf_viz::record]
fn pos_to_char_offset(rope: &Rope, position: &Position) -> Option<AbsoluteCharOffset> {
    Some(rope.line_to_char(LineIndex(position.line))? + position.offset)
}

#[perf_viz::record]
fn char_offset_to_pos(
    rope: &Rope,
    offset: AbsoluteCharOffset,
) -> Option<Position> {
    if rope.len_chars() == offset {
        Some(LineIndex(rope.len_lines().0 - 1))
    } else {
        rope.char_to_line(offset)
    }
    .and_then(|line_index| {
        let start_of_line = rope.line_to_char(line_index)?;

        offset.checked_sub(start_of_line).map(|o| Position {
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
        max: AbsoluteCharOffset
    }

    #[allow(dead_code)]
    impl AbsoluteCharOffsetRange {
        pub fn new(o1: AbsoluteCharOffset, o2: AbsoluteCharOffset) -> Self {
            let min = std::cmp::min(o1, o2);
            let max = std::cmp::max(o1, o2);

            AbsoluteCharOffsetRange {
                min,
                max
            }
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
    }
}
use absolute_char_offset_range::AbsoluteCharOffsetRange;




#[derive(Clone, Default, Debug, PartialEq, Eq)]
struct Change<T> {
    old: T,
    new: T
}

impl <T> std::ops::Not for Change<T> {
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct RangeEdits {
    insert_range: Option<RangeEdit>,
    delete_range: Option<RangeEdit>,
}

impl std::ops::Not for RangeEdits {
    type Output = RangeEdits;

    fn not(self) -> Self::Output {
        RangeEdits {
            insert_range: self.delete_range,
            delete_range: self.insert_range
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
        Edit{
            range_edits: cursors.new.mapped_ref(|_| d!()),
            cursors
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
            cursors: !self.cursors
        }
    }
}

impl TextBuffer {
    pub fn redo(&mut self) -> Option<()> {
        self.history
            .get(self.history_index)
            .cloned()
            .map(|edit| {
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
        pos_to_char_offset(rope, p.borrow())
            .and_then(|o| rope.char_to_byte(o))
    }
}

#[cfg(test)]
#[macro_use]
mod test_macros;
#[cfg(test)]
mod tests;
