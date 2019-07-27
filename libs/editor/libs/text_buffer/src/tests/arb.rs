// This module is inside `tests`
use super::*;

// TODO move all `arb` fns in here

prop_compose! {
    pub fn rope()(s in any::<String>()) -> Rope {
        r!(s)
    }
}

prop_compose! {
    pub fn non_0_to_9_char_rope()(s in "[^0-9]*") -> Rope {
        r!(s)
    }
}

fn vec1<D: Debug>(strat: impl Strategy<Value = D>, max_len: usize) -> impl Strategy<Value = Vec1<D>> {
    collection::vec(strat, 1..std::cmp::max(2, max_len))
        .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
}

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

pub fn valid_cursors_for_rope(rope: &Rope, max_len: usize) -> impl Strategy<Value = Cursors> {
    let (max_line, max_offset) =
        rope
        .lines()
        .enumerate()
        .fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
            (i, std::cmp::min(line.len_chars(), acc_offset))
        });

    vec1(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::min(max_len, std::cmp::max(max_line * max_offset.0, 1))
    )
}

pub fn many_valid_cursors_for_rope(rope: &Rope, max_len: usize) -> impl Strategy<Value = Cursors> {
    let (max_line, max_offset) =
        rope
        .lines()
        .enumerate()
        .fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
            (i, std::cmp::min(line.len_chars(), acc_offset))
        });
    let len = max_line + 1;

    collection::vec(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::max(len/2, 1)..std::cmp::min(std::cmp::max(2, len), max_len)
    )
        .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
}

prop_compose! {
    pub fn no_history_text_buffer()
    (rope in rope())
    (cursors in arb::cursors(rope.chars().count()), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn text_buffer_with_valid_cursors()
    (rope in rope())
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

prop_compose! {
    pub fn text_buffer_with_valid_cursors_and_no_0_to_9_chars(max_len: usize)
    (rope in non_0_to_9_char_rope())
    (
        cursors in valid_cursors_for_rope(&rope, max_len),
        r in Just(rope)
    ) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn text_buffer_with_many_valid_cursors_and_no_0_to_9_chars(max_len: usize)
    (rope in non_0_to_9_char_rope())
    (
        cursors in many_valid_cursors_for_rope(&rope, max_len),
        r in Just(rope)
    ) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub(crate) fn range_edit(max_len: usize)
    (chars in ".*", range in arb_absolute_char_offset_range(max_len)) -> RangeEdit {
        RangeEdit {
            chars,
            range
        }
    }
}

prop_compose! {
    pub(crate) fn range_edits(max_len: usize)
    (insert_range in option::of(range_edit(max_len)), delete_range in option::of(range_edit(max_len))) -> RangeEdits {
        RangeEdits {
            insert_range,
            delete_range,
        }
    }
}

prop_compose! {
    pub(crate) fn edit()
    (len in 1..SOME_AMOUNT)
    (range_edits in vec1(range_edits(len), len), cursors in arb_change!(arb::cursors(len))) -> Edit {
        Edit {
            range_edits,
            cursors,
        }
    }
}
