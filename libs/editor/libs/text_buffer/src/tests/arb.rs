// This module is inside `tests`
use super::*;

// TODO move all `arb` fns in here

prop_compose! {
    pub fn cursor(LineIndex(max_line): LineIndex, CharOffset(max_offset): CharOffset)(
        position in arb_pos(max_line, max_offset),
        highlight_position in arb_pos(max_line, max_offset),
        sticky_offset in arb_char_offset(max_offset),
        state in arb_cursor_state()
    ) -> Cursor {
        let mut c = Cursor::new(position);
        c.set_highlight_position(highlight_position);
        c.sticky_offset = sticky_offset;
        c.state = state;
        c
    }
}

pub fn cursors(max_len: usize) -> impl Strategy<Value = Cursors> {
    vec1(
        // passing `max_len` in for both of these ensures that we get all possibly valid cursors
        // but itmeans we will get many invalid ones.
        cursor(LineIndex(max_len), CharOffset(max_len)),
        // It doesn't semm particularly useful to have more cursors than text positions.
        max_len
    )
}

prop_compose! {
    pub fn no_history_text_buffer()
    (rope in arb_rope())
    (cursors in arb::cursors(rope.chars().count()), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn text_buffer_with_valid_cursors()
    (rope in arb_rope())
    (
        cursors in {
            let (max_line, max_offset) = rope.lines().enumerate().fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
                (i, std::cmp::min(line.len_chars(), acc_offset))
            });

            vec1(
                cursor(LineIndex(max_line), max_offset),
                std::cmp::max(max_line * max_offset.0, 1)
            )
        },
        r in Just(rope)
    ) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}
