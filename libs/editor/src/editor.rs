use editor_types::{Cursor, Position};
use gap_buffer::GapBuffer;
use macros::{d, dg};
use platform_types::{BufferView, Cmd, Input, Move, View};
use unicode_segmentation::UnicodeSegmentation;
use vec1::Vec1;

#[derive(Default)]
struct Buffer {
    gap_buffer: GapBuffer,
    cursors: Vec1<Cursor>,
}

impl Buffer {
    fn new() -> Self {
        d!()
    }

    fn insert(&mut self, ch: char) {
        for cursor in &mut self.cursors {
            self.gap_buffer.insert(ch, &cursor.position);
            move_right(&self.gap_buffer, cursor);
        }
    }

    fn delete(&mut self) {
        for cursor in &mut self.cursors {
            self.gap_buffer.delete(&cursor.position);
            move_left(&self.gap_buffer, cursor);
        }
    }

    fn move_all_cursors(&mut self, r#move: Move) {
        for i in 0..self.cursors.len() {
            self.move_cursor(i, r#move)
        }
    }

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

    fn grapheme_before(&self, c: &Cursor) -> Option<&str> {
        self.gap_buffer.grapheme_before(c)
    }

    fn grapheme_after(&self, c: &Cursor) -> Option<&str> {
        self.gap_buffer.grapheme_after(c)
    }
}

enum Moved {
    No,
    Yes,
}

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
        let current_line = gap_buffer.lines().nth(target_line);
        if let Some(line) = current_line {
            target_offset = line.graphemes(true).count();
        }
        move_to(
            gap_buffer,
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

fn move_down(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    let target_line = cursor.position.line + 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };

    // Try moving to the same offset on the line below, falling back to its EOL.
    if let Moved::No = move_to(gap_buffer, cursor, new_position) {
        let mut target_offset = 0;
        let current_line = gap_buffer.lines().nth(target_line);
        if let Some(line) = current_line {
            target_offset = line.graphemes(true).count();
        }
        move_to(
            gap_buffer,
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
fn move_left(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    let pos = cursor.position;
    // Don't bother if we are already at the left edge.
    if pos.offset == 0 {
        return;
    }

    move_to(
        gap_buffer,
        cursor,
        Position {
            offset: pos.offset - 1,
            ..pos
        },
    );
}
fn move_right(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    let pos = cursor.position;
    move_to(
        gap_buffer,
        cursor,
        Position {
            offset: pos.offset + 1,
            ..pos
        },
    );
}
fn move_to_line_start(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    move_to(
        gap_buffer,
        cursor,
        Position {
            offset: 0,
            ..cursor.position
        },
    );
}
fn move_to_line_end(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    let line = cursor.position.line;
    let current_line = gap_buffer.lines().nth(line);
    if let Some(current_line) = current_line {
        let new_position = Position {
            line,
            offset: current_line.graphemes(true).count(),
        };
        move_to(gap_buffer, cursor, new_position);
    }
}
fn move_to_buffer_start(_gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    // The fisrt position is always valid
    cursor.position = Position { line: 0, offset: 0 };
    cursor.sticky_offset = 0;
}
fn move_to_buffer_end(gap_buffer: &GapBuffer, cursor: &mut Cursor) {
    if let Some((line, line_data)) = gap_buffer.lines().enumerate().last() {
        let new_position = Position {
            line,
            offset: line_data.graphemes(true).count(),
        };
        move_to(gap_buffer, cursor, new_position);
    }
}

impl<'buffer> Buffer {
    fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        self.gap_buffer.chars()
    }
}

pub struct State {
    buffers: Vec1<Buffer>,
    current_burrer_index: usize,
    scroll_x: f32,
    scroll_y: f32,
    screen_w: f32,
    screen_h: f32,
    ///We are currently assuming the font is monospace!
    char_w: f32,
    line_h: f32,
}

impl State {
    fn current_buffer(&self) -> Option<&Buffer> {
        self.buffers.get(self.current_burrer_index)
    }
    fn current_buffer_mut(&mut self) -> Option<&mut Buffer> {
        self.buffers.get_mut(self.current_burrer_index)
    }
}

pub fn new() -> State {
    State {
        buffers: Vec1::new(Buffer::new()),
        current_burrer_index: 0,
        scroll_x: 0.0,
        scroll_y: 0.0,
        screen_w: 0.0,
        screen_h: 0.0,
        char_w: 0.0,
        line_h: 0.0,
    }
}

pub fn update_and_render(state: &mut State, input: Input) -> (View, Cmd) {
    use platform_types::BufferViewKind;
    match dg!(input) {
        Input::None => {}
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
            macro_rules! set_if_present {
                ($field:ident) => {
                    if let Some($field) = sizes.$field {
                        state.$field = $field;
                    }
                };
            }
            set_if_present!(screen_w);
            set_if_present!(screen_h);
            set_if_present!(char_w);
            set_if_present!(line_h);
        }
    }

    let status_line_y = state.screen_h - state.line_h;

    (
        View {
            buffers: state.current_buffer().map_or_else(
                || {
                    vec![BufferView {
                        kind: BufferViewKind::StatusLine,
                        screen_position: (0.0, status_line_y),
                        bounds: (state.screen_w, state.line_h),
                        color: [0.9, 0.3, 0.3, 1.0],
                        chars: "No buffer selected.".to_owned(),
                    }]
                },
                |buffer| {
                    let mut views = vec![BufferView {
                        kind: BufferViewKind::Edit,
                        screen_position: (state.scroll_x, state.scroll_y),
                        bounds: (std::f32::INFINITY, std::f32::INFINITY),
                        color: [0.3, 0.3, 0.9, 1.0],
                        chars: buffer.chars().collect::<String>(),
                    }];

                    for position in buffer.cursors.iter().map(|c| c.position) {
                        // Weird *graphical-only* stuff given a >2^24 long line and/or >2^24
                        // lines seems better than an error box or something like that.
                        #[allow(clippy::cast_precision_loss)]
                        let screen_position = (
                            position.offset as f32 * state.char_w + state.scroll_x,
                            position.line as f32 * state.line_h + state.scroll_y,
                        );

                        views.push(BufferView {
                            kind: BufferViewKind::Cursor,
                            screen_position,
                            bounds: (state.screen_w, state.line_h),
                            color: [0.9, 0.3, 0.3, 1.0],
                            chars: "▏".to_string(),
                        });
                    }

                    views.push(BufferView {
                        kind: BufferViewKind::StatusLine,
                        screen_position: (0.0, status_line_y),
                        bounds: (state.screen_w, state.line_h),
                        color: [0.3, 0.9, 0.3, 1.0],
                        chars: buffer.cursors.iter().fold(
                            String::with_capacity(state.screen_w as usize),
                            |mut acc, c| {
                                use std::fmt::Write;
                                let _cannot_actually_fail = write!(
                                    acc,
                                    "{} ({:?}|{:?})",
                                    c,
                                    buffer.grapheme_before(c),
                                    buffer.grapheme_after(c),
                                );
                                acc
                            },
                        ),
                    });

                    views
                },
            ),
        },
        Cmd::NoCmd,
    )
}
