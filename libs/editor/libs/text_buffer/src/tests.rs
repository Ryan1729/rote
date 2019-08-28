#![allow(dead_code)]
use super::{cursor_assert, r, t_b, *};
use crate::move_cursor::last_position;

use editor_types::{vec1, CursorState};
use platform_types::pos;
use proptest::prelude::*;
use proptest::{collection, option, prop_compose, proptest};
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
    ($s: expr) => {
        TestEdit::InsertString($s.into())
    };
}

// `Rope`s share backing buffers when cloned, so we want to avoid that.
fn deep_clone(buffer: &TextBuffer) -> TextBuffer {
    let s: std::borrow::Cow<str> = (&buffer.rope).into();
    TextBuffer {
        rope: Rope::from_str(&s),
        ..buffer.clone()
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
        (offset in 0..=r!(&s).len_chars().0, s in Just(s)) -> (Rope, AbsoluteCharOffset) {
        (r!(s), AbsoluteCharOffset(offset))
    }
}

fn arb_rope_and_pos() -> impl Strategy<Value = (Rope, Position)> {
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
        ($strat, $strat).prop_map(|(old, new)| Change { old, new })
    };
}

prop_compose! {
    fn arb_offset_pair()(
        o1 in option::of(arb_absolute_char_offset(SOME_AMOUNT)),
        o2 in option::of(arb_absolute_char_offset(SOME_AMOUNT))
    ) -> OffsetPair {
        (o1, o2)
    }
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_no_newline() {
    let rope = r!("1234");

    assert_eq!(
        final_non_newline_offset_for_line(&rope, LineIndex(0)),
        CharOffset(4).into()
    );
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_a_line_feed() {
    let rope = r!("1234\n5678");

    assert_eq!(
        final_non_newline_offset_for_line(&rope, LineIndex(0)),
        CharOffset(4).into()
    );
}

#[test]
fn final_non_newline_offset_for_line_works_on_a_string_with_a_carriage_return_line_feed() {
    let rope = r!("1234\r\n5678");

    assert_eq!(
        final_non_newline_offset_for_line(&rope, LineIndex(0)),
        CharOffset(4).into()
    );
}

#[test]
fn final_non_newline_offset_for_line_works_if_asked_about_a_non_existant_line() {
    let rope = r!("1234\r\n5678");

    assert_eq!(final_non_newline_offset_for_line(&rope, LineIndex(2)), None);
}

proptest! {
    #[test]
    fn nearest_valid_position_on_same_line_is_identity_for_positions_in_bounds(
        rope in arb::rope(),
        position in arb_pos(SOME_AMOUNT, SOME_AMOUNT)
    ) {
        let manually_checked = if in_cursor_bounds(&rope, position) {
            Some(position)
        } else {
            nearest_valid_position_on_same_line(&rope, position)
        };

        assert_eq!(nearest_valid_position_on_same_line(&rope, position), manually_checked);
    }
}

#[test]
fn this_multi_cursor_example_produces_the_correct_final_string() {
    let mut buffer = t_b!("000\n111\n222\n333\n");

    buffer.set_cursor(pos! {l 1 o 1}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 2 o 2}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 3 o 3}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 4 o 0}, ReplaceOrAdd::Add);

    buffer.insert('5');

    let expected = "5000\n1511\n2252\n3335\n5".to_owned();
    let actual: String = buffer.rope.into();

    assert_eq!(actual, expected);
}

/*

fn buffer_preserves_this_equality(buffer: &TextBuffer) {
    let mut buffer = deep_clone(buffer);
    let cursors_len = buffer.cursors.len();

    // This intentionally may double count, since not doing that will hide bugs.
    let highlighted_char_count = buffer.cursors.iter().fold(0, |acc, c| {
        if let (Some(o1), Some(o2)) = offset_pair(&buffer.rope, c) {
            let range = AbsoluteCharOffsetRange::new(o1, o2);

            acc + range.max().saturating_sub(range.min()).0
        } else {
            acc
        }
    });

    let initial_len_chars = buffer.rope.len_chars().0;

    buffer.insert('a');

    let final_len_chars = buffer.rope.len_chars().0;

    let expected = dbg!(initial_len_chars) as isize - dbg!(highlighted_char_count) as isize + dbg!(cursors_len) as isize;

    assert_eq!(
        final_len_chars as isize,
        expected,
        "we expected \"{}\" to be {} chars long",
        {let s: String = buffer.rope.into(); s},
        expected
    );
}

proptest!{
    #[test]
    fn multicursor_insertion_should_preserve_this_equality(
        buffer in arb::text_buffer_with_valid_cursors()
    ) {
        buffer_preserves_this_equality(&buffer);
    }
}

proptest!{
    #[test]
    fn multicursor_insertion_should_preserve_this_equality_even_if_there_are_lots_of_cursors(
        buffer in arb::text_buffer_with_many_valid_cursors_and_no_0_to_9_chars()
    ) {
        buffer_preserves_this_equality(&buffer);
    }
}

#[test]
fn this_buffer_preserves_this_equality() {
    let mut buffer = t_b!("abcde");

    buffer.set_cursor(pos!{l 0 o 2}, ReplaceOrAdd::Replace);
    buffer.set_cursor(pos!{l 0 o 4}, ReplaceOrAdd::Add);

    buffer.extend_selection_for_all_cursors(Move::ToBufferStart);

    buffer_preserves_this_equality(&buffer);
}

#[test]
fn this_generated_buffer_preserves_this_equality() {
    let mut buffer = t_b!(":\nA :\n");

    buffer.cursors = Vec1::try_from_vec(vec![
        {
            let mut c = Cursor::new(pos!{l 0 o 0});
            c.set_highlight_position(pos!{l 1 o 0});
            c
        },
        {
            let mut c = Cursor::new(pos!{l 2 o 0});
            c.set_highlight_position(pos!{l 0 o 0});
            c
        }
    ]).unwrap();

    buffer_preserves_this_equality(&buffer);
}

*/

fn buffer_inserts_all_the_requested_numbers_in_order(buffer: &TextBuffer) {
    let mut buffer = deep_clone(buffer);

    let cursors_len = buffer.cursors.len();
    assert!(
        cursors_len <= 9,
        "this test is only valid if there are less than 10 cursors."
    );

    let get_string = |i: usize| i.to_string();

    buffer.insert_at_each_cursor(get_string);

    let expected_strings = (0..cursors_len).map(get_string);

    let buffer_string: String = buffer.rope.into();

    let mut chars = buffer_string.chars();

    'outer: for s in expected_strings {
        while {
            if let Some(ch) = chars.next() {
                if ch.to_string() == s {
                    continue 'outer;
                }
                true
            } else {
                false
            }
        } {}

        assert!(
            false,
            "{:?} does not contain {:?} or has them in the wrong order",
            buffer_string, s
        );
    }
}

proptest! {
    #[test]
    fn inserting_sequential_numbers_into_a_field_of_non_numbers_inserts_all_the_requested_numbers_in_order(
        buffer in arb::text_buffer_with_valid_cursors_and_no_0_to_9_chars(9)
    ) {
        buffer_inserts_all_the_requested_numbers_in_order(&buffer);
    }
}

proptest! {
    #[test]
    fn inserting_sequential_numbers_into_a_field_of_non_numbers_inserts_all_the_requested_numbers_in_order_even_if_there_are_lots_of_cursors(
        buffer in arb::text_buffer_with_many_valid_cursors_and_no_0_to_9_chars(9)
    ) {
        buffer_inserts_all_the_requested_numbers_in_order(&buffer);
    }
}

#[test]
fn inserting_sequential_numbers_into_a_field_of_non_numbers_inserts_all_the_requested_numbers_in_order_on_this_example(
) {
    let mut buffer = t_b!("\n\n");

    buffer.cursors = Cursors::from_vec(vec![
        {
            let mut c = Cursor::new(pos! {l 0 o 0});
            c.set_highlight_position(pos! {l 1 o 0});
            c
        },
        {
            let mut c = Cursor::new(pos! {l 1 o 0});
            c.set_highlight_position(pos! {l 0 o 0});
            c
        },
    ])
    .unwrap();

    buffer_inserts_all_the_requested_numbers_in_order(&buffer);
}

#[test]
fn inserting_sequential_numbers_into_this_buffer_inserts_all_the_requested_numbers_in_order() {
    let mut buffer = t_b!("abcde");

    buffer.set_cursor(pos! {l 0 o 2}, ReplaceOrAdd::Replace);
    buffer.set_cursor(pos! {l 0 o 4}, ReplaceOrAdd::Add);

    buffer.extend_selection_for_all_cursors(Move::ToBufferStart);

    buffer_inserts_all_the_requested_numbers_in_order(&buffer);
}

const OUT_OF_ORDER: u8 = 0b1;
const HAS_OVERLAPS: u8 = 0b10;

fn cursor_vec1_maintains_invariants(cursors: &Vec1<Cursor>) -> u8 {
    use std::cmp::Ordering;
    let mut spans = cursors
        .mapped_ref(|c| {
            let p = c.get_position();
            let h = c.get_highlight_position_or_position();

            (std::cmp::min(p, h), std::cmp::max(p, h))
        })
        .into_vec();

    let mut output = 0;

    for window in spans.windows(2) {
        if let &[(later_min, later_max), (earlier_min, earlier_max)] = window {
            match earlier_min
                .cmp(&later_min)
                .then_with(|| earlier_max.cmp(&later_max))
            {
                Ordering::Less => {}
                Ordering::Equal => {
                    output |= HAS_OVERLAPS;
                }
                Ordering::Greater => {
                    output |= OUT_OF_ORDER;
                }
            }
        } else {
            unreachable!();
        }
    }

    // This prevents false overlap results given things are out of order
    spans.sort_by(|(min1, max1), (min2, max2)| min1.cmp(&min2).then_with(|| max1.cmp(&max2)));

    let mut current = None;

    for (span_min, span_max) in dbg!(spans).into_iter().rev() {
        if let Some((c_min, _c_max)) = current {
            if span_max >= c_min {
                output |= HAS_OVERLAPS;
            }
        } else {
            current = Some((span_min, span_max));
        }
    }

    output
}

// meta
#[test]
fn cursor_vec1_maintains_invariants_detects_invariant_violations() {
    assert_eq!(
        HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(&vec1![
            Cursor::new(pos! {l 1 o 2}),
            Cursor::new(pos! {l 1 o 2})
        ])
    );

    assert_eq!(
        OUT_OF_ORDER,
        cursor_vec1_maintains_invariants(&vec1![
            Cursor::new(pos! {l 1 o 2}),
            Cursor::new(pos! {l 9 o 0})
        ])
    );

    assert_eq!(
        OUT_OF_ORDER | HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(&vec1![
            {
                let mut c = Cursor::new(pos! {l 1 o 2});
                c.set_highlight_position(pos! {l 9 o 1});
                c
            },
            Cursor::new(pos! {l 9 o 0})
        ])
    );

    assert_eq!(
        OUT_OF_ORDER | HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(&vec1![
            {
                let mut c = Cursor::new(pos! {l 1 o 2});
                c.set_highlight_position(pos! {l 9 o 0});
                c
            },
            Cursor::new(pos! {l 9 o 0})
        ])
    );
}

fn cursors_maintains_invariants(cursors: &Cursors) -> u8 {
    cursor_vec1_maintains_invariants(&cursors.cursors)
}

macro_rules! assert_cursor_invarints_maintained {
    ($cursors: expr) => {{
        let flags = cursors_maintains_invariants(&$cursors);

        assert_eq!(
            0,
            flags,
            "{} {}",
            if flags & OUT_OF_ORDER == OUT_OF_ORDER {
                "OUT_OF_ORDER"
            } else {
                ""
            },
            if flags & HAS_OVERLAPS == HAS_OVERLAPS {
                "HAS_OVERLAPS"
            } else {
                ""
            }
        )
    }};
}

proptest! {
    #[test]
    fn editing_the_buffer_preserves_the_cursors_invariants(
        mut buffer in arb::text_buffer_with_many_cursors(),
        edits in arb::test_edits(99, arb::TestEditSpec::All)
    ) {
        assert_cursor_invarints_maintained!(buffer.cursors);

        for edit in edits {
            arb::TestEdit::apply(&mut buffer, edit);
        }

        assert_cursor_invarints_maintained!(buffer.cursors);
    }
}

proptest! {
    #[test]
    fn cursors_new_maintains_invariants(
        vec1_of_cursors in arb::vec1_of_cursors(SOME_AMOUNT)
    ) {
        assert_cursor_invarints_maintained!(Cursors::new(vec1_of_cursors));
    }
}

#[test]
fn cursors_new_merges_these_cursors() {
    let cursors = Cursors::new(vec1![d!(), d!(), d!()]);

    assert_eq!(cursors.len(), 1);
}

#[test]
fn cursors_new_merges_these_identical_cursors_correctly() {
    let cursors = Cursors::new(vec1![
        Cursor::new(pos! {l 1 o 2}),
        Cursor::new(pos! {l 1 o 2}),
        d!()
    ]);

    assert_eq!(cursors.cursors, vec1![Cursor::new(pos! {l 1 o 2}), d!()]);
}

#[test]
fn adding_a_cursor_inside_a_highlight_does_not_change_the_selection() {
    let mut buffer = t_b!("12345678");

    macro_rules! selection_is_unchanged {
        (p right) => {
            assert_eq!(
                1,
                buffer.cursors.len(),
                "A cursor didn't get merged: {:#?}",
                &buffer.cursors
            );
            let c = buffer.cursors.first();

            assert_eq!(c.get_position(), pos! {l 0 o 7});
            assert_eq!(c.get_highlight_position_or_position(), pos! {l 0 o 1});
        };
        (p left) => {
            assert_eq!(
                1,
                buffer.cursors.len(),
                "A cursor didn't get merged: {:#?}",
                &buffer.cursors
            );
            let c = buffer.cursors.first();

            assert_eq!(c.get_position(), pos! {l 0 o 1});
            assert_eq!(c.get_highlight_position_or_position(), pos! {l 0 o 7});
        };
    }

    // Position on right
    buffer.move_all_cursors(Move::Right);

    for _ in 0..6 {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    // Establishing that it starts correct.
    selection_is_unchanged!(p right);

    buffer.set_cursor(pos! {l 0 o 4}, ReplaceOrAdd::Add);

    selection_is_unchanged!(p right);

    buffer.set_cursor(pos! {l 0 o 1}, ReplaceOrAdd::Add);

    selection_is_unchanged!(p right);

    buffer.set_cursor(pos! {l 0 o 7}, ReplaceOrAdd::Add);

    selection_is_unchanged!(p right);


    // Position on left
    buffer.move_all_cursors(Move::Right);

    for _ in 0..6 {
        buffer.extend_selection_for_all_cursors(Move::Left);
    }

    // Establishing that it starts correct.
    selection_is_unchanged!(p left);

    buffer.set_cursor(pos! {l 0 o 4}, ReplaceOrAdd::Add);

    selection_is_unchanged!(p left);

    buffer.set_cursor(pos! {l 0 o 1}, ReplaceOrAdd::Add);

    selection_is_unchanged!(p left);

    buffer.set_cursor(pos! {l 0 o 7}, ReplaceOrAdd::Add);

    selection_is_unchanged!(p left);
}

// Non-breaking space =
const NBSP: char = '\u{A0}';

#[test]
fn get_tab_in_edit_produces_the_expected_edit_from_this_buffer_with_different_leading_whitespace() {
    let text = format!("0\n 1\n  2\n   3\n    4\n\n{0}\n{0}1\n {0}2\n", NBSP);

    let mut buffer = t_b!(text.to_owned());
    buffer.select_all();

    let rope = &buffer.rope;

    let cursors = &buffer.cursors;

    let edit = get_tab_in_edit(rope, cursors);

    let expected = {
        let mut cursor = Cursor::new(pos! {});
        cursor.set_highlight_position(last_position(rope));

        let new_chars =
            "    0\n     1\n      2\n       3\n        4\n    \n     \n     1\n      2\n"
                .to_owned();

        let insert_range = Some(RangeEdit {
            range: AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(new_chars.len())),
            chars: new_chars,
        });

        let delete_range = Some(RangeEdit {
            range: AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(text.len())),
            chars: text,
        });

        Edit {
            range_edits: Vec1::new(RangeEdits {
                insert_range,
                delete_range,
            }),
            cursors: Change {
                new: Cursors::new(Vec1::new(cursor)),
                old: cursors.clone(),
            },
        }
    };

    assert_eq!(edit, expected);
}

mod arb;

mod cursor_manipulation;
mod undo_redo;
