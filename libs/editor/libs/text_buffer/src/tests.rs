#![allow(dead_code)]
use super::{cursor_assert, r, t_b, *};
use platform_types::pos;
use editor_types::CursorState;
use proptest::prelude::*;
use proptest::{prop_compose, proptest, option, collection};
use std::fmt::Debug;

/// This is expected to be used where the amount does not really matter, except that it must be
/// enough that the behaviour we want to test has enough space.
/// Ciode that assumes this is > 1 is known to exist as of this writing.
const SOME_AMOUNT: usize = 16;
/// This is expected to be used where the amount does not really matter, except that it must be
/// greater than `SOME_AMOUNT` so that out-of-bounds checks, etc. get tested.
const MORE_THAN_SOME_AMOUNT: usize = 24;

/// This macro is meant to make it easier to copy-paste proptest failing proptest inputs into
/// their oun test. With this macro, only the `!` character needs to be added after copying an
/// `InsertString` input.
#[allow(non_snake_case)]
macro_rules! InsertString {
    ($s: expr) => (TestEdit::InsertString($s.into()));
}

prop_compose! {
    fn arb_rope()(s in any::<String>()) -> Rope {
        r!(s)
    }
}

prop_compose! {
    fn arb_absolute_char_offset(max_len: usize)(offset in 0..=max_len) -> AbsoluteCharOffset {
        AbsoluteCharOffset(offset)
    }
}

prop_compose! {
    fn arb_absolute_char_offset_range(max_len: usize)
    (o1 in arb_absolute_char_offset(max_len), o2 in arb_absolute_char_offset(max_len)) -> AbsoluteCharOffsetRange {
        AbsoluteCharOffsetRange::new(o1, o2)
    }
}

prop_compose! {
    fn arb_rope_and_offset()
        (s in ".*")
        (offset in 0..=r!(&s).len_chars(), s in Just(s)) -> (Rope, AbsoluteCharOffset) {
        (r!(s), AbsoluteCharOffset(offset))
    }
}

fn arb_rope_and_pos() -> impl Strategy<Value = (Rope, Position)> {
    ".*".prop_flat_map(|s: String| {
        let line_count = r!(s).len_lines();
        (0..line_count, Just(s)).prop_flat_map(move |(line_index, s)| {
            let line_len = r!(s)
                .lines()
                .nth(line_index)
                //The index comes from `len_lines()` so it should always produce a `Some`!
                .unwrap()
                .len_chars();

            let max_offset = line_len - if line_index < line_count - 1 { 1 } else { 0 };

            (0..=max_offset, Just(s)).prop_map(move |(offset, s)| {
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
    fn arb_char_offset(max_len: usize)(offset in 0..=max_len) -> CharOffset {
        CharOffset(offset)
    }
}

// This is duplicated from `platform_types`'s `tests` module. There is a way to avoid thi s
// duplication, but its complex enough that it does not seem worth ti for this code.
// https://stackoverflow.com/a/42329538
prop_compose! {
    fn arb_pos(max_line: usize, max_offset: usize)
    (line in 0..=max_line, offset in 0..=max_offset) -> Position {
        Position{ line, offset: CharOffset(offset) }
    }
}

fn arb_cursor_state() -> impl Strategy<Value = CursorState> {
    prop_oneof![
        Just(CursorState::None),
        Just(CursorState::PressedAgainstWall),
    ]
}

#[test]
fn offset_at_end_of_line_works() {
    let rope = r!("\u{b}");

    assert_eq!(
        pos_to_char_offset(&rope, &pos! {l 1 o 0}),
        Some(AbsoluteCharOffset(1))
    );

    assert_eq!(
        char_offset_to_pos(&rope, AbsoluteCharOffset(1)),
        Some(pos! {l 1 o 0})
    )
}

#[test]
fn offset_in_middle_of_single_line_with_non_ascii_works() {
    let rope = r!("0¡¡");

    assert_eq!(
        char_offset_to_pos(&rope, AbsoluteCharOffset(2)),
        Some(pos! {l 0 o 2})
    )
}

#[test]
fn pos_to_char_offset_works_on_middle_of_single_line() {
    let rope = r!("0A");

    assert_eq!(
        pos_to_char_offset(&rope, &pos! {l 0 o 1}),
        Some(AbsoluteCharOffset(1))
    )
}

#[test]
fn char_offset_to_pos_works_on_middle_of_single_line() {
    let rope = r!("0A");

    assert_eq!(
        char_offset_to_pos(&rope, AbsoluteCharOffset(1)),
        Some(pos! {l 0 o 1})
    )
}

fn pos_to_to_char_offset_to_pos(rope: &Rope, p: Position) {
    if let Some(o) = pos_to_char_offset(&rope, &p) {
        assert_eq!(char_offset_to_pos(&rope, o), Some(p))
    }
}

#[test]
fn offset_in_middle_of_single_line_works() {
    let rope = r!("0A");
    let p = pos! {l 0 o 1};
    pos_to_to_char_offset_to_pos(&rope, p);
}

#[test]
fn char_offset_to_pos_works_on_final_offset() {
    let rope = r!("A");

    assert_eq!(
        char_offset_to_pos(&rope, AbsoluteCharOffset(1)),
        Some(pos! {l 0 o 1})
    )
}

#[test]
fn final_offset_works() {
    let rope = r!("A");
    let p = pos! {l 0 o 1};
    pos_to_to_char_offset_to_pos(&rope, p);
}

proptest! {
    #[test]
    fn char_offset_to_pos_to_char_offset((rope, offset) in arb_rope_and_offset()) {
        if let Some(p) = char_offset_to_pos(&rope, offset) {
            assert_eq!(pos_to_char_offset(&rope, &p), Some(offset))
        }
    }

    #[test]
    fn pos_to_to_char_offset_to_pos_works((rope, pos) in arb_rope_and_pos()) {
        if let Some(o) = pos_to_char_offset(&rope, &pos) {
            assert_eq!(char_offset_to_pos(&rope, o), Some(pos))
        }
    }
}

fn arb_move() -> impl Strategy<Value = Move> {
    prop_oneof![
        Just(Move::Up),
        Just(Move::Down),
        Just(Move::Left),
        Just(Move::Right),
        Just(Move::ToLineStart),
        Just(Move::ToLineEnd),
        Just(Move::ToBufferStart),
        Just(Move::ToBufferEnd),
        Just(Move::ToPreviousLikelyEditLocation),
        Just(Move::ToNextLikelyEditLocation),
    ]
}

// Because I couldn't figure out the types for this. And it looks like `proptest` ends up making
// custom structs for each instance of things like this.
macro_rules! arb_change {
    ($strat: expr) => {
        ($strat, $strat).prop_map(|(old, new)| Change {
            old,
            new,
        })
    }
}

fn vec1<D: Debug>(strat: impl Strategy<Value = D>, max_len: usize) -> impl Strategy<Value = Vec1<D>> {
    collection::vec(strat, 1..std::cmp::max(2, max_len))
        .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
}

prop_compose! {
    fn arb_offset_pair()(
        o1 in option::of(arb_absolute_char_offset(SOME_AMOUNT)),
        o2 in option::of(arb_absolute_char_offset(SOME_AMOUNT))
    ) -> OffsetPair {
        (o1, o2)
    }
}

prop_compose! {
    fn arb_cursor(max_len: usize)(
        position in arb_pos(max_len, max_len),
        highlight_position in arb_pos(max_len, max_len),
        sticky_offset in arb_char_offset(max_len),
        state in arb_cursor_state()
    ) -> Cursor {
        let mut c = Cursor::new(position);
        c.set_highlight_position(highlight_position);
        c.sticky_offset = sticky_offset;
        c.state = state;
        c
    }
}


fn arb_cursors(max_len: usize) -> impl Strategy<Value = Cursors> {
    // It doesn't semm particularly useful to have more cursors than text positions.
    vec1(arb_cursor(max_len), max_len)
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_no_newline() {
    let rope = r!("1234");

    assert_eq!(final_non_newline_offset_for_line(&rope, 0), CharOffset(4).into());
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_a_line_feed() {
    let rope = r!("1234\n5678");

    assert_eq!(final_non_newline_offset_for_line(&rope, 0), CharOffset(4).into());
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_a_carriage_return_line_feed() {
    let rope = r!("1234\r\n5678");

    assert_eq!(final_non_newline_offset_for_line(&rope, 0), CharOffset(4).into());
}

#[test]
fn final_non_newline_offset_for_line_works_if_asked_about_anon_existant_line() {
    let rope = r!("1234\r\n5678");

    assert_eq!(final_non_newline_offset_for_line(&rope, 2), None);
}

mod undo_redo;
mod cursor_manipulation;
