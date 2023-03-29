#![cfg_attr(feature = "pub_arb", allow(dead_code))]
#![cfg_attr(feature = "pub_arb", allow(unused_macros))]
#![cfg_attr(feature = "pub_arb", allow(unused_imports))]

#![deny(array_into_iter)]
use super::{r, *};

use editor_types::{cur};
use panic_safe_rope::{RopeSliceTrait};
use platform_types::{pos};
use proptest::{any_char, collection, prop_compose, Just, Strategy};

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
/// TODO? catergorize these?
mod uncategorized_tests;
#[cfg(feature = "cursor_manipulation")]
mod cursor_manipulation;
#[cfg(feature = "edit_tests")]
/// TODO? Not all edit tests are here now. So this could use a rename.
mod edit_tests;
#[cfg(feature = "strip_trailing_whitespace_does_not_increase_the_amount_of_characters")]
mod strip_trailing_whitespace_does_not_increase_the_amount_of_characters;
#[cfg(feature = "does_not_lose_characters")]
mod does_not_lose_characters;
#[cfg(feature = "included_files")]
mod included_files;

mod undo_redo {
    use super::*;

    use arb::{TestEdit};
    use macros::{dbg};
    use crate::{assert_text_buffer_eq_ignoring_history, t_b, TextBuffer};
    use pretty_assertions::assert_eq;

    #[cfg(feature = "do_proptests")]
    use proptest::proptest;
    #[cfg(not(feature = "do_proptests"))]
    macro_rules! proptest {
        ($($tokens: tt)*) => {}
    }
    #[cfg(feature = "do_proptests")]
    use arb::{TestEditSpec, SOME_AMOUNT};

    use std::borrow::Borrow;

    #[cfg(feature = "undo_redo_general")]
    mod general;
    #[cfg(feature = "undo_redo_works")]
    mod works;
    #[cfg(feature = "undo_redo_does_not_allow_applying_stale_redos")]
    mod does_not_allow_applying_stale_redos;
}
#[cfg(feature = "inserting_then_deleting")]
mod inserting_then_deleting;
