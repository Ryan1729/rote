use editor_types::{ByteIndex, Cursor, CursorState, SetPositionAction, Vec1};
use if_changed;
use macros::{borrow, borrow_mut, d};
use panic_safe_rope::Rope;
use platform_types::{AbsoluteCharOffset, CharOffset, Move, Position};
use std::borrow::Borrow;
use std::collections::VecDeque;

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

impl TextBuffer {
    #[perf_viz::record]
    pub fn insert(&mut self, c: char) {
        let edit = Edit::Insert(self.cursors.mapped_ref(|cursor| CharEdit {
            c: c.into(),
            offsets: offset_pair(&self.rope, cursor),
        }));
        self.apply_edit(&edit);

        self.history.push_back(edit);
        self.history_index += 1;
    }

    #[perf_viz::record]
    pub fn delete(&mut self) {
        let edit = Edit::Delete(self.get_char_edits());

        self.apply_edit(&edit);

        self.history.push_back(edit);
        self.history_index += 1;
    }

    fn get_char_edits(&self) -> Vec1<CharEdit> {
        self.cursors().mapped_ref(|cursor| {
            let offsets = offset_pair(&self.rope, cursor);
            CharEdit {
                c: offsets
                    .0
                    .and_then(|AbsoluteCharOffset(o)| self.rope.char(o)),
                offsets,
            }
        })
    }

    pub fn move_all_cursors(&mut self, r#move: Move) {
        for i in 0..self.cursors().len() {
            self.move_cursor(i, r#move)
        }
    }

    pub fn extend_selection_for_all_cursors(&mut self, r#move: Move) {
        for i in 0..self.cursors().len() {
            self.extend_selection(i, r#move)
        }
    }

    fn apply_edit(&mut self, edit: &Edit) {
        match edit {
            Edit::Insert(edits) => {
                for (cursor, &CharEdit { c, offsets, .. }) in self.cursors.iter_mut().zip(edits) {
                    if let Some(c) = c {
                        match offsets {
                            (Some(AbsoluteCharOffset(o)), highlight)
                                if highlight.is_none()
                                    || Some(AbsoluteCharOffset(o)) == highlight =>
                            {
                                self.rope.insert_char(o, c);
                                move_cursor::directly(&self.rope, cursor, Move::Right);
                            }
                            (Some(o1), Some(o2)) => {
                                let min = std::cmp::min(o1, o2);
                                let max = std::cmp::max(o1, o2);

                                self.rope.remove(min.0..max.0);
                                cursor.set_position(
                                    char_offset_to_pos(&self.rope, &min).unwrap_or_default(),
                                );

                                self.rope.insert_char(min.0, c);
                                move_cursor::directly(&self.rope, cursor, Move::Right);
                            }
                            _ => {}
                        }
                    }
                }
            }
            Edit::Delete(edits) => {
                for (cursor, &CharEdit { offsets, .. }) in self.cursors.iter_mut().zip(edits) {
                    match offsets {
                        (Some(AbsoluteCharOffset(o)), None) if o > 0 => {
                            self.rope.remove((o - 1)..o);
                            move_cursor::directly(&self.rope, cursor, Move::Left);
                        }
                        (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                            let min = std::cmp::min(o1, o2);
                            let max = std::cmp::max(o1, o2);

                            self.rope.remove(dbg!(min.0..max.0));
                            cursor.set_position(
                                char_offset_to_pos(&self.rope, &min).unwrap_or_default(),
                            );
                        }
                        _ => {}
                    }
                }
            }
            &Edit::Select { r#move } => self.extend_selection_for_all_cursors(r#move),
        }
    }

    pub fn cursors(&self) -> &Vec1<Cursor> {
        self.borrow()
    }

    #[perf_viz::record]
    pub fn move_cursor(&mut self, index: usize, r#move: Move) {
        if let Some(cursor) = self.cursors.get_mut(index) {
            if let Some(p) = cursor.get_highlight_position() {
                use std::cmp::{max, min};
                match r#move {
                    Move::Up | Move::Left => {
                        //we might need to clear the highlight_position and set the cursor state
                        cursor.set_position(min(p, cursor.get_position()));
                    }
                    Move::Down | Move::Right => {
                        // see above comment
                        cursor.set_position(max(p, cursor.get_position()));
                    }
                    Move::ToLineStart
                    | Move::ToBufferStart
                    | Move::ToLineEnd
                    | Move::ToBufferEnd => {
                        move_cursor::directly(&self.rope, cursor, r#move);
                        cursor.state = d!();
                    }
                };
            } else {
                move_cursor::directly(&self.rope, cursor, r#move);
            }
        }
    }

    #[perf_viz::record]
    pub fn extend_selection(&mut self, index: usize, r#move: Move) {
        if let Some(cursor) = self.cursors.get_mut(index) {
            move_cursor::directly_custom(
                &self.rope,
                cursor,
                r#move,
                SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
            );
        }
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

    #[perf_viz::record]
    pub fn nearest_valid_position_on_same_line<P: Borrow<Position>>(
        &self,
        p: P,
    ) -> Option<Position> {
        let p = p.borrow();

        valid_len_chars_for_line(&self.rope, p.line).map(|len| Position {
            offset: std::cmp::min(p.offset, CharOffset(len)),
            ..*p
        })
    }

    #[perf_viz::record]
    pub fn replace_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = position.borrow();

        if self.in_bounds(position) {
            self.cursors = Vec1::new(Cursor::new(*position));
        } else if let Some(p) = self.nearest_valid_position_on_same_line(position) {
            self.cursors = Vec1::new(Cursor::new(p));
        }
    }

    #[perf_viz::record]
    pub fn drag_cursors<P: Borrow<Position>>(&mut self, position: P) {
        let position = {
            let position = position.borrow();

            if self.in_bounds(position) {
                Some(*position)
            } else {
                self.nearest_valid_position_on_same_line(position)
            }
        };

        if let Some(p) = position {
            for c in self.cursors.iter_mut() {
                if_changed::dbg!(p);
                if_changed::dbg!(&c);
                c.set_position_custom(p, SetPositionAction::OldPositionBecomesHighlightIfItIsNone);
            }
        }
    }
}

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
    AbsoluteCharOffset(offset): &AbsoluteCharOffset,
) -> Option<Position> {
    let offset = *offset;
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

mod move_cursor {
    use super::*;

    pub fn directly(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
        directly_custom(rope, cursor, r#move, SetPositionAction::ClearHighlight);
        dbg!(("directly_custom", &cursor));
    }
    pub fn directly_custom(
        rope: &Rope,
        cursor: &mut Cursor,
        r#move: Move,
        action: SetPositionAction,
    ) {
        let new_state = match r#move {
            Move::Up => move_up(rope, cursor, action),
            Move::Down => move_down(rope, cursor, action),
            Move::Left => move_left(rope, cursor, action),
            Move::Right => move_right(rope, cursor, action),
            Move::ToLineStart => move_to_line_start(rope, cursor, action),
            Move::ToLineEnd => move_to_line_end(rope, cursor, action),
            Move::ToBufferStart => move_to_rope_start(rope, cursor, action),
            Move::ToBufferEnd => move_to_rope_end(rope, cursor, action),
        };

        cursor.state = match new_state {
            Moved::No => CursorState::PressedAgainstWall,
            Moved::Yes => CursorState::None,
        };
    }

    enum Moved {
        No,
        Yes,
    }

    #[perf_viz::record]
    fn move_to(
        rope: &Rope,
        cursor: &mut Cursor,
        position: Position,
        action: SetPositionAction,
    ) -> Moved {
        if cursor.get_position() == position {
            // We might need to clear the highlight cursor, depending on the action, even though
            // the postion matches.
            cursor.set_position_custom(position, action);
        } else if in_cursor_bounds(rope, &position) {
            dbg!(&cursor);
            dbg!((position, action));
            cursor.set_position_custom(position, action);
            dbg!(&cursor);

            // Remember this offset so that we can try
            // to maintain it when moving across lines.
            cursor.sticky_offset = position.offset;

            return Moved::Yes;
        }

        Moved::No
    }

    /// Try moving to the same offset on the line below, falling back to its EOL.
    #[perf_viz::record]
    fn move_to_with_fallback(
        rope: &Rope,
        cursor: &mut Cursor,
        new_position: Position,
        action: SetPositionAction,
    ) -> Moved {
        let target_line = new_position.line;
        dbg!(action);
        dbg!((&cursor, new_position, action));
        let mut output = move_to(rope, cursor, new_position, action);
        if let Moved::No = output {
            let mut target_offset = d!();
            if let Some(count) = nth_line_count(rope, target_line) {
                target_offset = count;
            }
            output = move_to(
                rope,
                cursor,
                Position {
                    line: target_line,
                    offset: target_offset,
                },
                action,
            );

            // Moving the position successfully updates the sticky offset, but we
            // haven't actually moved to where we really wanted to go (offset-wise).
            // Restore the original desired offset; it might be available on the next try.
            cursor.sticky_offset = new_position.offset;
        }
        output
    }

    #[perf_viz::record]
    fn move_up(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        let pos = cursor.get_position();
        // Don't bother if we are already at the top.
        if pos.line == 0 {
            return Moved::No;
        }

        let target_line = pos.line - 1;
        let new_position = Position {
            line: target_line,
            offset: cursor.sticky_offset,
        };
        move_to_with_fallback(rope, cursor, new_position, action)
    }

    #[perf_viz::record]
    fn move_down(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        let target_line = cursor.get_position().line + 1;
        let new_position = Position {
            line: target_line,
            offset: cursor.sticky_offset,
        };

        move_to_with_fallback(rope, cursor, new_position, action)
    }
    #[perf_viz::record]
    fn move_left(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        if let Some(new_pos) = backward(rope, cursor.get_position()) {
            move_to(rope, cursor, new_pos, action)
        } else {
            Moved::No
        }
    }
    #[perf_viz::record]
    fn move_right(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        if let Some(new_pos) = forward(rope, cursor.get_position()) {
            move_to(rope, cursor, new_pos, action)
        } else {
            Moved::No
        }
    }
    #[perf_viz::record]
    fn move_to_line_start(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        move_to(
            rope,
            cursor,
            Position {
                offset: d!(),
                ..cursor.get_position()
            },
            action,
        )
    }
    #[perf_viz::record]
    fn move_to_line_end(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        let line = cursor.get_position().line;
        if let Some(offset) = nth_line_count(rope, line) {
            let mut new_position = Position { line, offset };
            if !in_cursor_bounds(rope, new_position) {
                new_position = backward(rope, new_position).unwrap_or_default();
            }
            move_to(rope, cursor, new_position, action)
        } else {
            Moved::No
        }
    }
    #[perf_viz::record]
    fn move_to_rope_start(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        // The default is the first position, and the first position is always there.
        move_to(rope, cursor, d!(), action)
    }
    #[perf_viz::record]
    fn move_to_rope_end(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
        dbg!("move_to_rope_end");
        if let Some((line, offset)) = last_line_index_and_count(rope) {
            let new_position = Position { line, offset };
            move_to(rope, cursor, new_position, action)
        } else {
            Moved::No
        }
    }
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

// The platform layer does not need the ability to set the cursor to arbitrary positions.
#[derive(Clone, Copy, Debug)]
enum MoveSpec {
    To(Position),
    Move(Move),
}
d!(for MoveSpec: MoveSpec::To(d!()));

#[derive(Clone, Copy, Debug, Default)]
struct CharEdit {
    c: Option<char>,
    offsets: OffsetPair,
}

#[derive(Clone, Debug)]
enum Edit {
    Insert(Vec1<CharEdit>),
    Delete(Vec1<CharEdit>),
    Select { r#move: Move },
}

impl std::ops::Not for Edit {
    type Output = Edit;

    fn not(self) -> Self::Output {
        match self {
            Edit::Insert(edits) => Edit::Delete(edits.mapped(|e| CharEdit {
                offsets: (
                    Some(e.offsets.0.map(|o| o + 1).unwrap_or_default()),
                    e.offsets.1,
                ),
                ..e
            })),
            Edit::Delete(edits) => Edit::Insert(edits),
            Edit::Select { r#move } => Edit::Select { r#move: !r#move },
        }
    }
}

impl TextBuffer {
    pub fn redo(&mut self) -> Option<()> {
        dbg!(&self.history)
            .get(self.history_index)
            .cloned()
            .map(|edit| {
                self.apply_edit(&dbg!(edit));
                self.history_index += 1;
            })
    }

    pub fn undo(&mut self) -> Option<()> {
        let new_index = self.history_index.checked_sub(1)?;
        self.history.get(new_index).cloned().map(|edit| {
            self.apply_edit(&dbg!(!edit));
            self.history_index = new_index;
        })
    }
}

#[cfg(test)]
#[macro_use]
mod test_macros;
#[cfg(test)]
mod tests;
