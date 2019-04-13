use editor_types::Cursor;
use gap_buffer::{backward, forward, GapBuffer};
use macros::{d, dg};
use platform_types::{
    position_to_screen_space, screen_space_to_position, BufferView, CharDim, CharOffset, Cmd,
    Input, Move, Position, ScreenSpaceXY, UpdateAndRenderOutput, View,
};
use std::borrow::Borrow;
use vec1::Vec1;

#[derive(Default)]
struct Buffer {
    gap_buffer: GapBuffer,
    cursors: Vec1<Cursor>,
}

impl Buffer {
    #[perf_viz::record]
    fn insert(&mut self, ch: char) {
        for cursor in &mut self.cursors {
            self.gap_buffer.insert(ch, &cursor.position);
            move_right(&self.gap_buffer, cursor);
        }
    }

    #[perf_viz::record]
    fn delete(&mut self) {
        for cursor in &mut self.cursors {
            self.gap_buffer.delete(&cursor.position);
            move_left(&self.gap_buffer, cursor);
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
                Move::Up => move_up(&self.gap_buffer, cursor),
                Move::Down => move_down(&self.gap_buffer, cursor),
                Move::Left => move_left(&self.gap_buffer, cursor),
                Move::Right => move_right(&self.gap_buffer, cursor),
                Move::ToLineStart => move_to_line_start(&self.gap_buffer, cursor),
                Move::ToLineEnd => move_to_line_end(&self.gap_buffer, cursor),
                Move::ToBufferStart => move_to_buffer_start(&self.gap_buffer, cursor),
                Move::ToBufferEnd => move_to_buffer_end(&self.gap_buffer, cursor),
            }
        }
    }

    #[perf_viz::record]
    fn in_bounds<P: Borrow<Position>>(&self, p: P) -> bool {
        self.gap_buffer.in_bounds(p)
    }

    #[perf_viz::record]
    fn nearest_valid_position_on_same_line<P: Borrow<Position>>(&self, p: P) -> Option<Position> {
        self.gap_buffer.nearest_valid_position_on_same_line(p)
    }
}

impl Buffer {
    #[allow(dead_code)]
    fn grapheme_before(&self, c: &Cursor) -> Option<&str> {
        self.gap_buffer.grapheme_before(c)
    }

    #[allow(dead_code)]
    fn grapheme_after(&self, c: &Cursor) -> Option<&str> {
        self.gap_buffer.grapheme_after(c)
    }

    #[allow(dead_code)]
    fn grapheme_before_gap(&self) -> Option<&str> {
        self.gap_buffer.grapheme_before_gap()
    }

    #[allow(dead_code)]
    fn grapheme_after_gap(&self) -> Option<&str> {
        self.gap_buffer.grapheme_after_gap()
    }
}

enum Moved {
    No,
    Yes,
}

#[perf_viz::record]
fn move_to(gap_buffer: &GapBuffer, cursor: &mut Cursor, position: Position) -> Moved {
    if gap_buffer.in_bounds(&position) {
        cursor.position = position;

        // Remember this offset so that we can try
        // to maintain it when moving across lines.
        cursor.sticky_offset = position.offset;

        return Moved::Yes;
    }
    Moved::No
}

#[perf_viz::record]
fn move_up(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
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

    // Try moving to the same offset on the line below, falling back to its EOL.
    if let Moved::No = move_to(gap_buffer, cursor, new_position) {
        let mut target_offset = 0;
        if let Some(count) = gap_buffer.nth_line_count(target_line) {
            target_offset = count;
        }
        move_to(
            gap_buffer,
            cursor,
            Position {
                line: target_line,
                offset: CharOffset(target_offset),
            },
        );

        // Moving the position successfully updates the sticky offset, but we
        // haven't actually moved to where we really wanted to go (offset-wise).
        // Restore the original desired offset; it might be available on the next try.
        cursor.sticky_offset = new_position.offset;
    }
}

#[perf_viz::record]
fn move_down(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    let target_line = cursor.position.line + 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };

    // Try moving to the same offset on the line below, falling back to its EOL.
    if let Moved::No = move_to(gap_buffer, cursor, new_position) {
        let mut target_offset = 0;
        let current_line = gap_buffer.nth_line_count(target_line);
        if let Some(line) = current_line {
            target_offset = line;
        }
        move_to(
            gap_buffer,
            cursor,
            Position {
                line: target_line,
                offset: CharOffset(target_offset),
            },
        );

        // Moving the position successfully updates the sticky offset, but we
        // haven't actually moved to where we really wanted to go (offset-wise).
        // Restore the original desired offset; it might be available on the next try.
        cursor.sticky_offset = new_position.offset;
    }
}
#[perf_viz::record]
fn move_left(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    move_to(gap_buffer, cursor, backward(gap_buffer, cursor.position));
}
#[perf_viz::record]
fn move_right(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    move_to(gap_buffer, cursor, forward(gap_buffer, cursor.position));
}
#[perf_viz::record]
fn move_to_line_start(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    move_to(
        gap_buffer,
        cursor,
        Position {
            offset: d!(),
            ..cursor.position
        },
    );
}
#[perf_viz::record]
fn move_to_line_end(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    let line = cursor.position.line;
    if let Some(count) = gap_buffer.nth_line_count(line) {
        let new_position = Position {
            line,
            offset: CharOffset(count),
        };
        move_to(gap_buffer, cursor, new_position);
    }
}
#[perf_viz::record]
fn move_to_buffer_start(_gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    // The fisrt position is always valid
    cursor.position = Position {
        line: 0,
        offset: d!(),
    };
    cursor.sticky_offset = d!();
}
#[perf_viz::record]
fn move_to_buffer_end(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    if let Some((line, count)) = gap_buffer.last_line_index_and_count() {
        let new_position = Position {
            line,
            offset: CharOffset(count),
        };
        move_to(gap_buffer, cursor, new_position);
    }
}

impl<'buffer> Buffer {
    fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        self.gap_buffer.chars()
    }
}

#[derive(Default)]
pub struct State {
    buffers: Vec1<Buffer>,
    current_burrer_index: usize,
    scroll_x: f32,
    scroll_y: f32,
    screen_w: f32,
    screen_h: f32,
    mouse_x: f32,
    mouse_y: f32,
    char_dim: CharDim,
}

impl State {
    #[perf_viz::record]
    fn current_buffer(&self) -> Option<&Buffer> {
        self.buffers.get(self.current_burrer_index)
    }
    #[perf_viz::record]
    fn current_buffer_mut(&mut self) -> Option<&mut Buffer> {
        self.buffers.get_mut(self.current_burrer_index)
    }
}

pub fn new() -> State {
    d!()
}

#[perf_viz::record]
pub fn render_view(state: &State, view: &mut View) {
    use platform_types::BufferViewKind;
    let status_line_y = state.screen_h - state.char_dim.h;
    view.buffers.clear();

    match state.current_buffer() {
        Some(buffer) => {
            view.buffers.push(BufferView {
                kind: BufferViewKind::Edit,
                screen_position: (state.scroll_x, state.scroll_y),
                bounds: (std::f32::INFINITY, std::f32::INFINITY),
                color: [0.3, 0.3, 0.9, 1.0],
                chars: buffer.chars().collect::<String>(),
            });

            for position in buffer.cursors.iter().map(|c| c.position) {
                let screen_position = position_to_screen_space(
                    position,
                    state.char_dim,
                    (state.scroll_x, state.scroll_y),
                )
                .into();

                view.buffers.push(BufferView {
                    kind: BufferViewKind::Cursor,
                    screen_position,
                    bounds: (state.screen_w, state.char_dim.h),
                    color: [0.9, 0.3, 0.3, 1.0],
                    chars: "▏".to_string(),
                });
            }

            view.buffers.push(BufferView {
                kind: BufferViewKind::StatusLine,
                screen_position: (0.0, status_line_y),
                bounds: (state.screen_w, state.char_dim.h),
                color: [0.3, 0.9, 0.3, 1.0],
                chars: {
                    use std::fmt::Write;
                    let mut chars = String::with_capacity(state.screen_w as usize);

                    let _cannot_actually_fail = write!(
                        chars,
                        "m{:?} c{:?} ",
                        (state.mouse_x, state.mouse_y),
                        (state.char_dim.w, state.char_dim.h)
                    );

                    let _cannot_actually_fail = write!(
                        chars,
                        "g({:?}|{:?}) ",
                        buffer.grapheme_before_gap(),
                        buffer.grapheme_after_gap()
                    );

                    chars = buffer.cursors.iter().fold(chars, |mut acc, c| {
                        let _cannot_actually_fail = write!(
                            acc,
                            "{} ({:?}|{:?})",
                            c,
                            buffer.gap_buffer.find_index(c).and_then(|o| if o == 0 {
                                None
                            } else {
                                Some(o - 1)
                            }),
                            buffer.gap_buffer.find_index(c),
                        );
                        acc
                    });

                    chars
                },
            });
        }
        None => {
            view.buffers.push(BufferView {
                kind: BufferViewKind::StatusLine,
                screen_position: (0.0, status_line_y),
                bounds: (state.screen_w, state.char_dim.h),
                color: [0.9, 0.3, 0.3, 1.0],
                chars: "No buffer selected.".to_owned(),
            });
        }
    };
}

macro_rules! set_if_present {
    ($source:ident => $target:ident.$field:ident) => {
        if let Some($field) = $source.$field {
            $target.$field = $field;
        }
    };
}

#[perf_viz::record]
pub fn update_and_render(state: &mut State, input: Input) -> UpdateAndRenderOutput {
    if cfg!(debug_assertions) {
        if let Input::SetMousePos(_) = input {

        } else {
            dg!(input);
        }
    }
    match input {
        Input::None => {}
        Input::Quit => {}
        Input::Insert(c) => {
            if let Some(b) = state.current_buffer_mut() {
                b.insert(c);
            }
        }
        Input::Delete => {
            if let Some(b) = state.current_buffer_mut() {
                b.delete();
            }
        }
        Input::MoveAllCursors(r#move) => {
            if let Some(b) = state.current_buffer_mut() {
                b.move_all_cursors(r#move);
            }
        }
        Input::ScrollVertically(amount) => {
            state.scroll_y -= amount;
        }
        Input::ScrollHorizontally(amount) => {
            state.scroll_x += amount;
        }
        Input::ResetScroll => {
            state.scroll_x = 0.0;
            state.scroll_y = 0.0;
        }
        Input::SetSizes(sizes) => {
            set_if_present!(sizes => state.screen_w);
            set_if_present!(sizes => state.screen_h);
            set_if_present!(sizes => state.char_dim);
        }
        Input::SetMousePos(ScreenSpaceXY { x, y }) => {
            state.mouse_x = x;
            state.mouse_y = y;
        }
        Input::ReplaceCursors(xy) => {
            let position =
                screen_space_to_position(xy, state.char_dim, (state.scroll_x, state.scroll_y));
            if let Some(b) = state.current_buffer_mut() {
                if b.in_bounds(position) {
                    b.cursors = Vec1::new(Cursor::new(position));
                } else if let Some(p) = b.nearest_valid_position_on_same_line(position) {
                    b.cursors = Vec1::new(Cursor::new(p));
                }
            }
        }
    }

    let mut view = d!();

    render_view(state, &mut view);

    (view, Cmd::NoCmd)
}
