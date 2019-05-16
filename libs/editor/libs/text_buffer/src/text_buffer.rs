use editor_types::{ByteIndex, Cursor, MultiCursorBuffer, Vec1};
use macros::{borrow, borrow_mut, d};
use platform_types::{CharOffset, Move, Position};
use ropey::Rope;
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
            self.rope
                .insert_char(pos_to_char_offset(&self.rope, &cursor.position).0, ch);
            move_right(&self.rope, cursor);
        }
    }

    #[perf_viz::record]
    fn delete(&mut self) {
        for cursor in &mut self.cursors {
            let char_index = pos_to_char_offset(&self.rope, &cursor.position).0;
            if char_index > 0 {
                self.rope.remove((char_index - 1)..char_index);
                move_left(&self.rope, cursor);
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
                Move::ToBufferStart => move_to_buffer_start(&self.rope, cursor),
                Move::ToBufferEnd => move_to_buffer_end(&self.rope, cursor),
            }
        }
    }

    #[perf_viz::record]
    fn in_bounds<P: Borrow<Position>>(&self, p: P) -> bool {
        //self.rope.in_bounds(p)
        unimplemented!()
    }

    #[perf_viz::record]
    fn find_index<P: Borrow<Position>>(&self, p: P) -> Option<ByteIndex> {
        //self.rope.find_index(p)
        unimplemented!();
    }

    #[perf_viz::record]
    fn nearest_valid_position_on_same_line<P: Borrow<Position>>(&self, p: P) -> Option<Position> {
        //self.rope.nearest_valid_position_on_same_line(p)
        unimplemented!();
    }
}

#[perf_viz::record]
fn pos_to_char_offset(rope: &Rope, position: &Position) -> CharOffset {
    CharOffset(rope.line_to_char(position.line)) + position.offset
}

enum Moved {
    No,
    Yes,
}

#[perf_viz::record]
fn move_to(rope: &Rope, cursor: &mut Cursor, position: Position) -> Moved {
    // if rope.in_bounds(&position) {
    //     cursor.position = position;
    //
    //     // Remember this offset so that we can try
    //     // to maintain it when moving across lines.
    //     cursor.sticky_offset = position.offset;
    //
    //     return Moved::Yes;
    // }
    Moved::No
}

#[perf_viz::record]
fn move_up(rope: &Rope, cursor: &mut Cursor) {
    // let pos = cursor.position;
    // // Don't bother if we are already at the top.
    // if pos.line == 0 {
    //     return;
    // }
    //
    // let target_line = pos.line - 1;
    // let new_position = Position {
    //     line: target_line,
    //     offset: cursor.sticky_offset,
    // };
    //
    // // Try moving to the same offset on the line below, falling back to its EOL.
    // if let Moved::No = move_to(rope, cursor, new_position) {
    //     let mut target_offset = 0;
    //     if let Some(count) = rope.nth_line_count(target_line) {
    //         target_offset = count;
    //     }
    //     move_to(
    //         rope,
    //         cursor,
    //         Position {
    //             line: target_line,
    //             offset: CharOffset(target_offset),
    //         },
    //     );
    //
    //     // Moving the position successfully updates the sticky offset, but we
    //     // haven't actually moved to where we really wanted to go (offset-wise).
    //     // Restore the original desired offset; it might be available on the next try.
    //     cursor.sticky_offset = new_position.offset;
    // }
}

#[perf_viz::record]
fn move_down(rope: &Rope, cursor: &mut Cursor) {
    // let target_line = cursor.position.line + 1;
    // let new_position = Position {
    //     line: target_line,
    //     offset: cursor.sticky_offset,
    // };
    //
    // // Try moving to the same offset on the line below, falling back to its EOL.
    // if let Moved::No = move_to(rope, cursor, new_position) {
    //     let mut target_offset = 0;
    //     let current_line = rope.nth_line_count(target_line);
    //     if let Some(line) = current_line {
    //         target_offset = line;
    //     }
    //     move_to(
    //         rope,
    //         cursor,
    //         Position {
    //             line: target_line,
    //             offset: CharOffset(target_offset),
    //         },
    //     );
    //
    //     // Moving the position successfully updates the sticky offset, but we
    //     // haven't actually moved to where we really wanted to go (offset-wise).
    //     // Restore the original desired offset; it might be available on the next try.
    //     cursor.sticky_offset = new_position.offset;
    // }
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
    unimplemented!();
    // let line = cursor.position.line;
    // if let Some(count) = rope.nth_line_count(line) {
    //     let new_position = Position {
    //         line,
    //         offset: CharOffset(count),
    //     };
    //     move_to(rope, cursor, new_position);
    // }
}
#[perf_viz::record]
fn move_to_buffer_start(_rope: &Rope, cursor: &mut Cursor) {
    // The fisrt position is always valid
    cursor.position = Position {
        line: 0,
        offset: d!(),
    };
    cursor.sticky_offset = d!();
}
#[perf_viz::record]
fn move_to_buffer_end(rope: &Rope, cursor: &mut Cursor) {
    unimplemented!();
    // if let Some((line, count)) = rope.last_line_index_and_count() {
    //     let new_position = Position {
    //         line,
    //         offset: CharOffset(count),
    //     };
    //     move_to(rope, cursor, new_position);
    // }
}

impl<'buffer> TextBuffer {
    pub fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        self.rope.chars()
    }
}

fn backward<P>(rope: &Rope, position: P) -> Position
where
    P: Borrow<Position>,
{
    unimplemented!();
    // let position = position.borrow();
    //
    // if position.offset == 0 {
    //     if position.line == 0 {
    //         return *position;
    //     }
    //     let line = position.line.saturating_sub(1);
    //     Position {
    //         line,
    //         offset: CharOffset(rope.nth_line_count(line).unwrap_or_default()),
    //     }
    // } else {
    //     Position {
    //         offset: position.offset - 1,
    //         ..*position
    //     }
    // }
}

fn forward<P>(rope: &Rope, position: P) -> Position
where
    P: Borrow<Position>,
{
    unimplemented!();
    // let position = position.borrow();
    //
    // let mut new = Position {
    //     offset: position.offset + 1,
    //     ..*position
    // };
    //
    // //  we expect the rest of the system to bounds check on positions
    // if !rope.in_bounds(&new) {
    //     new.line += 1;
    //     new.offset = d!();
    // }
    //
    // new
}
