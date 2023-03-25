#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]

#![deny(array_into_iter)]
use super::{cursor_assert, r, *};

use arb::{TestEdit, SOME_AMOUNT};
use move_cursor::last_position;
use editor_types::{cur};
use cursors::curs;
use rope_pos::{char_offset_to_pos, clamp_position, OffsetPair};
use panic_safe_rope::{RopeSliceTrait};
use platform_types::{pos, CursorState, vec1};
use pretty_assertions::assert_eq;
use proptest::{any_char, collection, option, prop_compose, proptest, Just, Strategy};

use pub_arb_std::non_line_break_char;

/// This macro is meant to make it easier to copy-paste proptest failing proptest inputs into
/// their oun test. With this macro, only the `!` character needs to be added after copying an
/// `InsertString` input.
#[allow(non_snake_case)]
#[macro_export]
macro_rules! InsertString {
    ($s: expr) => {
        TestEdit::InsertString($s.into())
    };
}

// `Rope`s share backing buffers when cloned, so we want to avoid that.
pub fn deep_clone(buffer: &TextBuffer) -> TextBuffer {
    let s: std::borrow::Cow<str> = buffer.borrow_rope().into();
    let mut new_buffer = TextBuffer {
        rope: CursoredRope::from(Rope::from_str(&s)),
        ..buffer.clone()
    };

    new_buffer.rope.set_cursors(buffer.borrow_cursors().clone());

    new_buffer
}

prop_compose! {
    pub fn arb_rope_and_offset()
        (s in ".*")
        (offset in 0..=r!(&s).len_chars().0, s in Just(s)) -> (Rope, AbsoluteCharOffset) {
        (r!(s), AbsoluteCharOffset(offset))
    }
}

pub fn arb_rope_and_pos() -> impl Strategy<Value = (Rope, Position)> {
    ".*".prop_flat_map(|s: String| {
        let line_count = r!(s).len_lines().0;
        (0..line_count, Just(s)).prop_flat_map(move |(line_index, s)| {
            let line_len = r!(s)
                .lines()
                .nth(line_index)
                //The index comes from `len_lines()` so it should always produce a `Some`!
                .unwrap()
                .len_chars();

            let max_offset = line_len - if line_index < line_count - 1 { 1 } else { 0 };

            (0..=max_offset.0, Just(s)).prop_map(move |(offset, s)| {
                (
                    r!(s),
                    Position {
                        line: line_index,
                        offset: CharOffset(offset),
                    },
                )
            })
        })
    })
}

prop_compose! {
    fn arb_rope_and_pos_and_offset()
    ((rope, pos) in arb_rope_and_pos())
    (offset in 0..=rope.len_chars().0, (r, p) in (Just(rope), Just(pos))) -> (Rope, Position, AbsoluteCharOffset) {
        (r, p, AbsoluteCharOffset(offset))
    }
}

pub mod arb;
#[cfg(feature = "uncategorized_tests")]
mod uncategorized_tests;
#[cfg(feature = "cursor_manipulation")]
mod cursor_manipulation;
#[cfg(feature = "edit_tests")]
mod edit_tests;
#[cfg(feature = "inserting_then_deleting")]
mod inserting_then_deleting;
