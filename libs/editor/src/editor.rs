use platform_types::{d, dg, BufferView, Cmd, Input, Move, View};
use unicode_segmentation::UnicodeSegmentation;
use vec1::Vec1;
use std::borrow::Borrow;

//TODO replace with a real Gap Buffer
struct GapBuffer {
    data: String,
}

impl GapBuffer {
    fn insert(&mut self, c: char) {
        self.data.push(c);
    }

    fn delete(&mut self) {
        self.data.pop();
    }

    fn in_bounds(&self, position: &Position) -> bool {
        self.find_offset(position) != None
    }

    /// Maps a position to its raw data index This can be affected by multi-`char` graphemes,
    /// so the result does **not** make sense to be assigned to a `Position`s offset field.
    //TODO make "GraphemeAwareOffset" newtype? Or maybe we should make the position offset a newtype?
    fn find_offset<P: Borrow<Position>>(&self, position: P) -> Option<usize> {
        let pos = position.borrow();
        let mut line = 0;
        let mut line_offset = 0;

        for (offset, grapheme) in self.data.grapheme_indices(true) {
            if line == pos.line && line_offset == pos.offset {
                return Some(offset);
            }

            if grapheme == "\n" || grapheme == "\r\n" {
                line += 1;
                line_offset = 0;
            } else {
                line_offset += 1;
            }
        }

        // there's the extra space after the end of the buffer, which *is* a valid insertion point.
        if line == pos.line && line_offset == pos.offset {
            return Some(self.data.len());
        }

        None
    }
}

impl<'buffer> GapBuffer {
    fn lines(&'buffer self) -> impl Iterator<Item = &str> + 'buffer {
        self.data.lines()
    }

    fn chars(&'buffer self) -> impl Iterator<Item = char> + 'buffer {
        self.data.chars()
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn move_down_bug() {
        let buffer = GapBuffer {
            data: include_str!("../../../text/slipsum.txt").into(),
        };

        let mut cursor = d!();

        for _ in 0..60 {
            move_right(&buffer, &mut cursor);
        }

        move_down(&buffer, &mut cursor);
        move_down(&buffer, &mut cursor);
        move_down(&buffer, &mut cursor);
        move_down(&buffer, &mut cursor);

        assert_eq!(cursor.position.line, 4);
        assert_eq!(cursor.position.offset, 45);
    }
}

struct Buffer {
    gap_buffer: GapBuffer,
    cursors: Vec1<Cursor>,
}

impl Buffer {
    fn new() -> Self {
        Self {
            gap_buffer: GapBuffer {
                data: include_str!("../../../text/slipsum.txt").into(),
            },
            cursors: Vec1::new(d!()),
        }
    }

    fn insert(&mut self, c: char) {
        self.gap_buffer.insert(c);
    }

    fn delete(&mut self) {
        self.gap_buffer.delete();
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
        let offset = self.gap_buffer.find_offset(c.position);
        offset.and_then(|o| {
            if o == 0 {
                None
            } else {
                self.gap_buffer.data.graphemes(true).nth(o - 1)
            }
        })
    }

    fn grapheme_after(&self, c: &Cursor) -> Option<&str> {
        let offset = self.gap_buffer.find_offset(c.position);
        offset.and_then(|o| self.gap_buffer.data.graphemes(true).nth(o))
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

/// `offset` indicates a location before or after characters, not at the charaters.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Position {
    pub line: usize,
    pub offset: usize,
}

#[derive(Clone, Debug, Default)]
pub struct Cursor {
    pub position: Position,
    sticky_offset: usize,
}

impl Cursor {
    fn new(position: Position) -> Self {
        Cursor {
            position,
            sticky_offset: position.offset,
        }
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
                    let mut views = vec![
                        BufferView {
                            kind: BufferViewKind::Edit,
                            screen_position: (state.scroll_x, state.scroll_y),
                            bounds: (std::f32::INFINITY, status_line_y - state.scroll_x),
                            color: [0.3, 0.3, 0.9, 1.0],
                            chars: buffer.chars().collect::<String>(),
                        },
                        BufferView {
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
                                        "{}:{} ({:?}|{:?})",
                                        c.position.line,
                                        c.position.offset,
                                        buffer.grapheme_before(c),
                                        buffer.grapheme_after(c)
                                    );
                                    acc
                                },
                            ),
                        },
                    ];

                    for position in buffer.cursors.iter().map(|c| c.position) {
                        // Weird *graphical-only* stuff given a >2^24 long line and/or >2^24
                        // lines seems better than an error box or something like that.
                        #[allow(clippy::cast_precision_loss)]
                        let screen_position = (
                            position.offset as f32 * state.char_w,
                            position.line as f32 * state.line_h,
                        );

                        views.push(BufferView {
                            kind: BufferViewKind::StatusLine,
                            screen_position,
                            bounds: (state.screen_w, state.line_h),
                            color: [0.9, 0.3, 0.3, 1.0],
                            chars: "▏".to_string(),
                        });
                    }

                    views
                },
            ),
        },
        Cmd::NoCmd,
    )
}
