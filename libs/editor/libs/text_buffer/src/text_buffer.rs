use editor_types::{ByteIndex, Cursor, CursorState, MultiCursorBuffer, Vec1};
use macros::{borrow, borrow_mut, d};
use panic_safe_rope::Rope;
use platform_types::{AbsoluteCharOffset, CharOffset, Move, Position};
use std::borrow::Borrow;

#[derive(Default)]
pub struct TextBuffer {
    rope: Rope,
    cursors: Vec1<Cursor>,
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

fn offset_pair(
    rope: &Rope,
    cursor: &Cursor,
) -> (Option<AbsoluteCharOffset>, Option<AbsoluteCharOffset>) {
    (
        pos_to_char_offset(rope, &cursor.position),
        cursor
            .highlight_position
            .and_then(|p| pos_to_char_offset(rope, &p)),
    )
}

impl MultiCursorBuffer for TextBuffer {
    #[perf_viz::record]
    fn insert(&mut self, ch: char) {
        for cursor in &mut self.cursors {
            match offset_pair(&self.rope, cursor) {
                (Some(AbsoluteCharOffset(o)), highlight)
                    if highlight.is_none() || Some(AbsoluteCharOffset(o)) == highlight =>
                {
                    self.rope.insert_char(o, ch);
                    move_right(&self.rope, cursor);
                }
                (Some(o1), Some(o2)) => {
                    let min = std::cmp::min(o1, o2);
                    let max = std::cmp::max(o1, o2);

                    self.rope.remove(min.0..max.0);
                    cursor.position = char_offset_to_pos(&self.rope, &min).unwrap_or_default();
                    cursor.highlight_position = None;

                    self.rope.insert_char(min.0, ch);
                    move_right(&self.rope, cursor);
                }
                _ => {}
            }
        }
    }

    #[perf_viz::record]
    fn delete(&mut self) {
        for cursor in &mut self.cursors {
            match offset_pair(&self.rope, cursor) {
                (Some(AbsoluteCharOffset(o)), None) if o > 0 => {
                    self.rope.remove((o - 1)..o);
                    move_left(&self.rope, cursor);
                }
                (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                    let min = std::cmp::min(o1, o2);
                    let max = std::cmp::max(o1, o2);

                    self.rope.remove(dbg!(min.0..max.0));
                    cursor.position = char_offset_to_pos(&self.rope, &min).unwrap_or_default();
                    cursor.highlight_position = None;
                }
                _ => {}
            }
        }
    }

    #[perf_viz::record]
    fn move_cursor(&mut self, index: usize, r#move: Move) {
        if let Some(cursor) = self.cursors.get_mut(index) {
            if let Some(p) = cursor.highlight_position {
                cursor.highlight_position = None;

                match r#move {
                    Move::Up | Move::Left => {
                        if p <= cursor.position {
                            cursor.position = p;
                        }
                    }
                    Move::Down | Move::Right => {
                        if p >= cursor.position {
                            cursor.position = p;
                        }
                    }
                    Move::ToLineStart
                    | Move::ToBufferStart
                    | Move::ToLineEnd
                    | Move::ToBufferEnd => {
                        move_cursor_directly(&self.rope, cursor, r#move);
                        cursor.state = d!();
                    }
                };
            } else {
                move_cursor_directly(&self.rope, cursor, r#move);
            }
        }
    }

    #[perf_viz::record]
    fn extend_selection(&mut self, index: usize, r#move: Move) {
        if let Some(cursor) = self.cursors.get_mut(index) {
            set_selection_to_here_if_not_set(cursor);

            move_cursor_directly(&self.rope, cursor, r#move);
        }
    }

    fn in_bounds<P: Borrow<Position>>(&self, position: P) -> bool {
        in_cursor_bounds(&self.rope, position)
    }

    #[perf_viz::record]
    fn find_index<P: Borrow<Position>>(&self, p: P) -> Option<ByteIndex> {
        let rope = &self.rope;
        pos_to_char_offset(rope, p.borrow())
            .and_then(|AbsoluteCharOffset(o)| rope.char_to_byte(o).map(ByteIndex))
    }

    #[perf_viz::record]
    fn nearest_valid_position_on_same_line<P: Borrow<Position>>(&self, p: P) -> Option<Position> {
        let p = p.borrow();

        valid_len_chars_for_line(&self.rope, p.line).map(|len| Position {
            offset: std::cmp::min(p.offset, CharOffset(len)),
            ..*p
        })
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

fn move_cursor_directly(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
    let new_state = match r#move {
        Move::Up => move_up(rope, cursor),
        Move::Down => move_down(rope, cursor),
        Move::Left => move_left(rope, cursor),
        Move::Right => move_right(rope, cursor),
        Move::ToLineStart => move_to_line_start(rope, cursor),
        Move::ToLineEnd => move_to_line_end(rope, cursor),
        Move::ToBufferStart => move_to_rope_start(rope, cursor),
        Move::ToBufferEnd => move_to_rope_end(rope, cursor),
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
fn move_to(rope: &Rope, cursor: &mut Cursor, position: Position) -> Moved {
    if cursor.position != position && in_cursor_bounds(rope, &position) {
        cursor.position = position;

        // Remember this offset so that we can try
        // to maintain it when moving across lines.
        cursor.sticky_offset = position.offset;

        return Moved::Yes;
    }
    Moved::No
}

/// Try moving to the same offset on the line below, falling back to its EOL.
#[perf_viz::record]
fn move_to_with_fallback(rope: &Rope, cursor: &mut Cursor, new_position: Position) -> Moved {
    let target_line = new_position.line;

    let mut output = move_to(rope, cursor, new_position);
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
        );

        // Moving the position successfully updates the sticky offset, but we
        // haven't actually moved to where we really wanted to go (offset-wise).
        // Restore the original desired offset; it might be available on the next try.
        cursor.sticky_offset = new_position.offset;
    }
    output
}

#[perf_viz::record]
fn move_up(rope: &Rope, cursor: &mut Cursor) -> Moved {
    let pos = cursor.position;
    // Don't bother if we are already at the top.
    if pos.line == 0 {
        return Moved::No;
    }

    let target_line = pos.line - 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };
    move_to_with_fallback(rope, cursor, new_position)
}

#[perf_viz::record]
fn move_down(rope: &Rope, cursor: &mut Cursor) -> Moved {
    let target_line = cursor.position.line + 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };

    move_to_with_fallback(rope, cursor, new_position)
}
#[perf_viz::record]
fn move_left(rope: &Rope, cursor: &mut Cursor) -> Moved {
    if let Some(new_pos) = backward(rope, cursor.position) {
        move_to(rope, cursor, new_pos)
    } else {
        Moved::No
    }
}
#[perf_viz::record]
fn move_right(rope: &Rope, cursor: &mut Cursor) -> Moved {
    if let Some(new_pos) = forward(rope, cursor.position) {
        move_to(rope, cursor, new_pos)
    } else {
        Moved::No
    }
}
#[perf_viz::record]
fn move_to_line_start(rope: &Rope, cursor: &mut Cursor) -> Moved {
    move_to(
        rope,
        cursor,
        Position {
            offset: d!(),
            ..cursor.position
        },
    )
}
#[perf_viz::record]
fn move_to_line_end(rope: &Rope, cursor: &mut Cursor) -> Moved {
    let line = cursor.position.line;
    if let Some(offset) = nth_line_count(rope, line) {
        let mut new_position = Position { line, offset };
        if !in_cursor_bounds(rope, new_position) {
            new_position = backward(rope, new_position).unwrap_or_default();
        }
        move_to(rope, cursor, new_position)
    } else {
        Moved::No
    }
}
#[perf_viz::record]
fn move_to_rope_start(rope: &Rope, cursor: &mut Cursor) -> Moved {
    // The default is the first position, and the first position is always there.
    move_to(rope, cursor, d!())
}
#[perf_viz::record]
fn move_to_rope_end(rope: &Rope, cursor: &mut Cursor) -> Moved {
    if let Some((line, offset)) = last_line_index_and_count(rope) {
        let new_position = Position { line, offset };
        move_to(rope, cursor, new_position)
    } else {
        Moved::No
    }
}

fn set_selection_to_here_if_not_set(cursor: &mut Cursor) {
    if cursor.highlight_position.is_none() {
        cursor.highlight_position = Some(cursor.position);
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

#[cfg(test)]
#[macro_use]
mod test_macros;
#[cfg(test)]
mod tests;
