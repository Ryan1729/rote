use editor_types::{ByteIndex, Cursor, CursorState, SetPositionAction, Vec1};
use if_changed;
use macros::{borrow, borrow_mut, d};
use panic_safe_rope::Rope;
use platform_types::{AbsoluteCharOffset, CharOffset, Move, Position};
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
    dbg!(
        rope.slice(range.usize_range())
            .map(|slice| {let s: String = slice.into(); s})
            .unwrap_or_default()
    )
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

    #[perf_viz::record]
    pub fn replace_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();

        if self.in_bounds(position) {
            self.apply_cursor_edit(Vec1::new(Cursor::new(*position)));
        } else if let Some(p) = nearest_valid_position_on_same_line(&self.rope, position) {
            self.apply_cursor_edit(Vec1::new(Cursor::new(p)));
        }
    }

    #[perf_viz::record]
    pub fn drag_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = {
            let position = position.borrow();

            if self.in_bounds(position) {
                Some(*position)
            } else {
                nearest_valid_position_on_same_line(&self.rope, position)
            }
        };

        if let Some(p) = position {
            let mut new = self.cursors.clone();
            for c in new.iter_mut() {
                if_changed::dbg!(p);
                if_changed::dbg!(&c);
                c.set_position_custom(p, SetPositionAction::OldPositionBecomesHighlightIfItIsNone);
            }
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
                action(&self.rope, dbg!(new.get_mut(index)?), r#move);
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
        for range_edit in edit.range_edits.iter() {
            if let Some(RangeEdit{range, ..}) = range_edit.delete_range {
                self.rope.remove(range.usize_range());
            }

            if let Some(RangeEdit{ref chars, range, ..}) = &range_edit.insert_range {
                self.rope.insert(range.usize_min(), chars);
            }
        }

        self.cursors = edit.cursors.new.clone();

        match kind {
            ApplyKind::Record => {
                dbg!(&mut self.history).push_back(edit);
                self.history_index += 1;
            }
            ApplyKind::Playback => {}
        }
    }
}

fn get_insert_edit(
    original_rope: &Rope,
    original_cursors: &Cursors,
    s: String
) -> Edit {
    let mut cloned_rope = original_rope.clone();
    let mut cloned_cursors = original_cursors.clone();

    let range_edits = cloned_cursors.mapped_mut(|cursor| {
        match offset_pair(original_rope, cursor) {
            (Some(AbsoluteCharOffset(o)), highlight)
                if highlight.is_none()
                    || Some(AbsoluteCharOffset(o)) == highlight =>
            {
                cloned_rope.insert(o, &s);
                move_cursor::directly(&cloned_rope, cursor, Move::Right);

                RangeEdits {
                    insert_range: Some(RangeEdit {
                        chars: s.to_owned(),
                        range: AbsoluteCharOffsetRange::usize_new(o, o + s.chars().count())
                    }),
                    ..d!()
                }
            }
            (Some(o1), Some(o2)) => {
                let range_edit = delete_highlighted(&mut cloned_rope, cursor, o1, o2);

                cloned_rope.insert(range_edit.range.usize_min(), &s);
                move_cursor::directly(&cloned_rope, cursor, Move::Right);

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
    });

    Edit {
        range_edits,
        cursors: Change {
            new: cloned_cursors,
            old: original_cursors.clone()
        }
    }
}

fn get_delete_edit(
    original_rope: &Rope,
    original_cursors: &Cursors
) -> Edit {
    let mut cloned_rope = original_rope.clone();
    let mut cloned_cursors = original_cursors.clone();

    let range_edits = cloned_cursors.mapped_mut(|cursor| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(AbsoluteCharOffset(o)), None) if o > 0 => {
                // Deleting the LF ('\n') of a CRLF ("\r\n") pair is a special case
                // where the cursor should not be moved backwards. Thsi is because
                // CR ('\r') and CRLF ("\r\n") both count as a single newline.
                // TODO would it better to just delete both at once? That seems like
                // it would require a moe comlicated special case elsewhere.
                let not_deleting_lf_of_cr_lf = {
                    o.checked_sub(2).and_then(|two_back| {
                        let mut chars = cloned_rope.slice(two_back..o)?.chars();

                        Some(
                            (chars.next()?, chars.next()?) != ('\r', '\n')
                        )
                    }).unwrap_or(true)
                };

                let delete_offset_range = AbsoluteCharOffsetRange::usize_new(o - 1, o);
                let chars = copy_string(&cloned_rope, delete_offset_range);
                cloned_rope.remove(delete_offset_range.usize_range());

                if not_deleting_lf_of_cr_lf {
                    move_cursor::directly(&cloned_rope, cursor, Move::Left);
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
                    delete_range: Some(delete_highlighted(&mut cloned_rope, cursor, o1, o2)),
                    ..d!()
                }
            }
            _ => {
                d!()
            }
        }
    });

    Edit {
        range_edits,
        cursors: Change {
            new: cloned_cursors,
            old: original_cursors.clone()
        }
    }
}

/// returns `None` if the input position's line does not refer to a line in the `Rope`.
fn nearest_valid_position_on_same_line<P: Borrow<Position>>(
    rope: &Rope,
    p: P,
) -> Option<Position> {
    let p = p.borrow();

    valid_len_chars_for_line(rope, p.line).map(|len| Position {
        offset: std::cmp::min(p.offset, CharOffset(len)),
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

    rope.remove(range.usize_range());
    cursor.set_position(
        char_offset_to_pos(&rope, range.min()).unwrap_or_default(),
    );

    RangeEdit {
        chars,
        range
    }
}

/// returns `None` if that line is not in the `Rope`.
fn valid_len_chars_for_line(rope: &Rope, line_index: usize) -> Option<usize> {
    rope.lines().nth(line_index).map(|line| {
        // we assume a line can contain at most one `'\n'`

        let mut len = line.len_chars();
        macro_rules! return_if_0 {
            () => {
                if len == 0 {
                    return 0;
                }
            };
        }

        return_if_0!();

        let last = line.char(len - 1);

        if last == '\n' {
            len -= 1;
            return_if_0!();

            let second_last = line.char(len - 1);

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
    })
}

fn in_cursor_bounds<P: Borrow<Position>>(rope: &Rope, position: P) -> bool {
    let p = position.borrow();
    valid_len_chars_for_line(rope, p.line)
        .map(|l| p.offset <= l)
        .unwrap_or(false)
}

fn nth_line_count(rope: &Rope, n: usize) -> Option<CharOffset> {
    rope.lines().nth(n).map(|l| CharOffset(l.len_chars()))
}

fn last_line_index_and_count(rope: &Rope) -> Option<(usize, CharOffset)> {
    rope.lines()
        .map(|l| CharOffset(l.len_chars()))
        .enumerate()
        .last()
}

#[perf_viz::record]
fn pos_to_char_offset(rope: &Rope, position: &Position) -> Option<AbsoluteCharOffset> {
    Some(AbsoluteCharOffset(rope.line_to_char(position.line)?) + position.offset)
}

#[perf_viz::record]
fn char_offset_to_pos(
    rope: &Rope,
    AbsoluteCharOffset(offset): AbsoluteCharOffset,
) -> Option<Position> {
    if rope.len_chars() == offset {
        Some(rope.len_lines() - 1)
    } else {
        dbg!(rope.char_to_line(offset))
    }
    .and_then(|line_index| {
        let start_of_line = rope.line_to_char(line_index)?;

        offset.checked_sub(start_of_line).map(|o| Position {
            line: line_index,
            offset: CharOffset(o),
        })
    })
}



impl<'rope> TextBuffer {
    pub fn chars(&'rope self) -> impl Iterator<Item = char> + 'rope {
        self.rope.chars()
    }
}

fn backward<P>(rope: &Rope, position: P) -> Option<Position>
where
    P: Borrow<Position>,
{
    let mut position = *position.borrow();

    while {
        position = if position.offset == 0 {
            if position.line == 0 {
                return None;
            }
            let line = position.line.saturating_sub(1);
            Position {
                line,
                offset: nth_line_count(rope, line).unwrap_or_default(),
            }
        } else {
            Position {
                offset: position.offset - 1,
                ..position
            }
        };

        !in_cursor_bounds(rope, position)
    } {}

    Some(position)
}

fn forward<P>(rope: &Rope, position: P) -> Option<Position>
where
    P: Borrow<Position>,
{
    let position = position.borrow();

    let mut new = Position {
        offset: position.offset + 1,
        ..*position
    };

    if !in_cursor_bounds(rope, &new) {
        new.line += 1;
        new.offset = d!();
        dbg!(new);
    }

    if in_cursor_bounds(rope, &new) {
        dbg!(Some(new));
        Some(new)
    } else {
        dbg!("None");
        None
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

        pub fn usize_new(o1: usize, o2: usize) -> Self {
            AbsoluteCharOffsetRange::new(AbsoluteCharOffset(o1), AbsoluteCharOffset(o2))
        }

        pub fn usize_range(&self) -> std::ops::Range<usize> {
            self.min.0..self.max.0
        }

        pub fn min(&self) -> AbsoluteCharOffset {
            self.min
        }

        pub fn max(&self) -> AbsoluteCharOffset {
            self.max
        }

        pub fn usize_min(&self) -> usize {
            self.min.0
        }

        pub fn usize_max(&self) -> usize {
            self.max.0
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
        Edit {
            range_edits: self.range_edits.mapped(|r_e| !r_e),
            cursors: !self.cursors
        }
    }
}

impl TextBuffer {
    pub fn redo(&mut self) -> Option<()> {
        dbg!(&self.history)
            .get(self.history_index)
            .cloned()
            .map(|edit| {
                self.apply_edit(edit, ApplyKind::Playback);
                self.history_index += 1;
            })
    }

    pub fn undo(&mut self) -> Option<()> {
        let new_index = self.history_index.checked_sub(1)?;
        self.history.get(new_index).cloned().map(|edit| {
            self.apply_edit(dbg!(!dbg!(edit)), ApplyKind::Playback);
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
            .and_then(|AbsoluteCharOffset(o)| rope.char_to_byte(o).map(ByteIndex))
    }
}

#[cfg(test)]
#[macro_use]
mod test_macros;
#[cfg(test)]
mod tests;
