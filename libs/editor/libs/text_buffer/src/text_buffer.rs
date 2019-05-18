use editor_types::{ByteIndex, Cursor, MultiCursorBuffer, Vec1};
use macros::{borrow, borrow_mut, d};
use panic_safe_rope::Rope;
use platform_types::{CharOffset, Move, Position};
use std::borrow::Borrow;

#[derive(Default)]
pub struct TextBuffer {
    rope: Rope,
    cursors: Vec1<Cursor>,
}

borrow!(<Vec1<Cursor>> for TextBuffer : s in &s.cursors);
borrow_mut!(<Vec1<Cursor>> for TextBuffer : s in &mut s.cursors);

impl MultiCursorBuffer for TextBuffer {
    #[perf_viz::record]
    fn insert(&mut self, ch: char) {
        for cursor in &mut self.cursors {
            if let Some(CharOffset(o)) = pos_to_char_offset(&self.rope, &cursor.position) {
                self.rope.insert_char(o, ch);
                move_right(&self.rope, cursor);
            }
        }
    }

    #[perf_viz::record]
    fn delete(&mut self) {
        for cursor in &mut self.cursors {
            match pos_to_char_offset(&self.rope, &cursor.position) {
                Some(CharOffset(o)) if o > 0 => {
                    self.rope.remove((o - 1)..o);
                    move_left(&self.rope, cursor);
                }
                _ => {}
            }
        }
    }

    #[perf_viz::record]
    fn move_all_cursors(&mut self, r#move: Move) {
        for i in 0..self.cursors.len() {
            self.move_cursor(i, r#move)
        }
    }

    #[perf_viz::record]
    fn move_cursor(&mut self, index: usize, r#move: Move) {
        if let Some(cursor) = self.cursors.get_mut(index) {
            match r#move {
                Move::Up => move_up(&self.rope, cursor),
                Move::Down => move_down(&self.rope, cursor),
                Move::Left => move_left(&self.rope, cursor),
                Move::Right => move_right(&self.rope, cursor),
                Move::ToLineStart => move_to_line_start(&self.rope, cursor),
                Move::ToLineEnd => move_to_line_end(&self.rope, cursor),
                Move::ToBufferStart => move_to_rope_start(&self.rope, cursor),
                Move::ToBufferEnd => move_to_rope_end(&self.rope, cursor),
            }
        }
    }

    #[perf_viz::record]
    fn find_index<P: Borrow<Position>>(&self, p: P) -> Option<ByteIndex> {
        find_index(&self.rope, p)
    }

    #[perf_viz::record]
    fn nearest_valid_position_on_same_line<P: Borrow<Position>>(&self, p: P) -> Option<Position> {
        let p = p.borrow();
        let line = self.rope.lines().nth(p.line)?;

        Some(Position {
            offset: std::cmp::min(p.offset, CharOffset(line.len_chars())),
            ..*p
        })
    }
}

fn in_bounds<P: Borrow<Position>>(rope: &Rope, position: P) -> bool {
    find_index(rope, position) != None
}

fn find_index<P: Borrow<Position>>(rope: &Rope, p: P) -> Option<ByteIndex> {
    pos_to_char_offset(rope, p.borrow())
        .and_then(|CharOffset(o)| rope.char_to_byte(o).map(ByteIndex))
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
fn pos_to_char_offset(rope: &Rope, position: &Position) -> Option<CharOffset> {
    Some(CharOffset(rope.line_to_char(position.line)?) + position.offset)
}

enum Moved {
    No,
    Yes,
}

#[perf_viz::record]
fn move_to(rope: &Rope, cursor: &mut Cursor, position: Position) -> Moved {
    if in_bounds(rope, &position) {
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
fn move_to_with_fallback(rope: &Rope, cursor: &mut Cursor, new_position: Position) {
    let target_line = new_position.line;
    if let Moved::No = move_to(rope, cursor, new_position) {
        let mut target_offset = d!();
        if let Some(count) = nth_line_count(rope, target_line) {
            target_offset = count;
        }
        move_to(
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
}

#[perf_viz::record]
fn move_up(rope: &Rope, cursor: &mut Cursor) {
    let pos = cursor.position;
    // Don't bother if we are already at the top.
    if pos.line == 0 {
        return;
    }

    let target_line = pos.line - 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };
    move_to_with_fallback(rope, cursor, new_position)
}

#[perf_viz::record]
fn move_down(rope: &Rope, cursor: &mut Cursor) {
    let target_line = cursor.position.line + 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };

    move_to_with_fallback(rope, cursor, new_position)
}
#[perf_viz::record]
fn move_left(rope: &Rope, cursor: &mut Cursor) {
    move_to(rope, cursor, backward(rope, cursor.position));
}
#[perf_viz::record]
fn move_right(rope: &Rope, cursor: &mut Cursor) {
    move_to(rope, cursor, forward(rope, cursor.position));
}
#[perf_viz::record]
fn move_to_line_start(rope: &Rope, cursor: &mut Cursor) {
    move_to(
        rope,
        cursor,
        Position {
            offset: d!(),
            ..cursor.position
        },
    );
}
#[perf_viz::record]
fn move_to_line_end(rope: &Rope, cursor: &mut Cursor) {
    let line = cursor.position.line;
    if let Some(offset) = nth_line_count(rope, line) {
        let new_position = Position { line, offset };
        move_to(rope, cursor, new_position);
    }
}
#[perf_viz::record]
fn move_to_rope_start(_rope: &Rope, cursor: &mut Cursor) {
    // The default is the first position, and the first position is always there.
    cursor.position = d!();
}
#[perf_viz::record]
fn move_to_rope_end(rope: &Rope, cursor: &mut Cursor) {
    if let Some((line, offset)) = last_line_index_and_count(rope) {
        let new_position = Position { line, offset };
        move_to(rope, cursor, new_position);
    }
}

impl<'rope> TextBuffer {
    pub fn chars(&'rope self) -> impl Iterator<Item = char> + 'rope {
        self.rope.chars()
    }
}

fn backward<P>(rope: &Rope, position: P) -> Position
where
    P: Borrow<Position>,
{
    let position = position.borrow();

    if position.offset == 0 {
        if position.line == 0 {
            return *position;
        }
        let line = position.line.saturating_sub(1);
        Position {
            line,
            offset: nth_line_count(rope, line).unwrap_or_default(),
        }
    } else {
        Position {
            offset: position.offset - 1,
            ..*position
        }
    }
}

fn forward<P>(rope: &Rope, position: P) -> Position
where
    P: Borrow<Position>,
{
    let position = position.borrow();

    let mut new = Position {
        offset: position.offset + 1,
        ..*position
    };

    //  we expect the rest of the system to bounds check on positions
    if !in_bounds(rope, &new) {
        new.line += 1;
        new.offset = d!();
    }

    new
}
