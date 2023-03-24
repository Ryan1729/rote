use super::*;
use editor_types::{AbsoluteCharOffset};
use panic_safe_rope::{RopeSliceTrait, LineIndex, CharOffset};
use proptest::{collection, Strategy, prop_compose};
use pub_arb_platform_types::{cursor_state};
use pub_arb_text_pos::{char_offset, pos};
use pub_arb_vec1::{vec1};
use rope_pos::char_offset_to_pos;

use core::borrow::Borrow;

use vec1::{Vec1};

prop_compose! {
    pub fn cursor(LineIndex(max_line): LineIndex, CharOffset(max_offset): CharOffset)(
        position in pos(max_line, max_offset),
        highlight_position in pos(max_line, max_offset),
        sticky_offset in char_offset(max_offset),
        state in cursor_state()
    ) -> Cursor {
        let mut c = Cursor::new(position);
        c.set_highlight_position(highlight_position);
        c.sticky_offset = sticky_offset;
        c.state = state;
        c
    }
}

pub fn vec1_of_cursors(max_len: usize) -> impl Strategy<Value = Vec1<Cursor>> {
    vec1(
        // passing `max_len` in for both of these ensures that we get all possibly valid cursors
        // but itmeans we will get many invalid ones.
        cursor(LineIndex(max_len), CharOffset(max_len)),
        // It doesn't semm particularly useful to have more cursors than text positions.
        max_len,
    )
}

fn get_max_line_and_max_offset(rope: &Rope) -> (usize, CharOffset) {
    rope.lines()
        .enumerate()
        .fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
            (i, std::cmp::min(line.len_chars(), acc_offset))
        })
}

pub fn valid_cursors_for_rope<'rope, R: 'rope + Borrow<Rope>>(
    rope: R,
    max_len: usize,
) -> impl Strategy<Value = Cursors> + 'rope {
    let rope = rope.borrow();
    let (max_line, max_offset) = get_max_line_and_max_offset(rope);

    let rope2 = rope.clone();

    vec1(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::min(max_len, std::cmp::max(max_line * max_offset.0, 1)),
    )
    .prop_map(move |c| Cursors::new(&rope2, c))
}

pub fn many_valid_cursors_for_rope<'rope, R: 'rope + Borrow<Rope>>(
    rope: R,
    max_len: usize,
) -> impl Strategy<Value = Cursors> + 'rope {
    let rope = rope.borrow();

    let (max_line, max_offset) = get_max_line_and_max_offset(rope);
    let len = max_line + 1;

    let rope2 = rope.clone();

    collection::vec(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::max(len / 2, 1)..std::cmp::min(std::cmp::max(2, len), max_len),
    )
    .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
    .prop_map(move |c| Cursors::new(&rope2, c))
}

pub fn many_valid_non_highlight_cursors_for_rope<'rope, R: 'rope + Borrow<Rope>>(
    rope: R,
    max_len: usize,
) -> impl Strategy<Value = Cursors> + 'rope {
    let rope = rope.borrow();

    let (max_line, max_offset) = get_max_line_and_max_offset(rope);
    let len = max_line + 1;

    let rope2 = rope.clone();

    collection::vec(
        non_highlight_cursor(LineIndex(max_line), max_offset),
        std::cmp::max(len / 2, 1)..std::cmp::min(std::cmp::max(2, len), max_len),
    )
    .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
    .prop_map(move |c| Cursors::new(&rope2, c))
}

pub fn all_but_end_cursors_for_rope(rope: &Rope) -> Vec<Cursor> {
    let len = rope.len_chars().0;
    let mut output = Vec::with_capacity(len);
    if len > 1 {
        for i in 1..(len - 1) {
            let offset = AbsoluteCharOffset(i);
            if let Some(position) = char_offset_to_pos(rope, offset) {
                output.push(Cursor::new(position));
            } else {
                break;
            }
        }
    }

    output
}

prop_compose! {
    pub fn non_highlight_cursor(LineIndex(max_line): LineIndex, CharOffset(max_offset): CharOffset)(
        position in pos(max_line, max_offset),
        sticky_offset in char_offset(max_offset),
        state in cursor_state()
    ) -> Cursor {
        let mut c = Cursor::new(position);
        c.sticky_offset = sticky_offset;
        c.state = state;
        c
    }
}