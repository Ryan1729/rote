#![allow(dead_code)]
use super::{cursor_assert, r, t_b, *};
use platform_types::pos;
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

#[test]
fn insert_with_matching_cursor_and_highlight_sets_highlight_to_none() {
    let mut buffer: TextBuffer = d!();

    {
        let c = &mut buffer.cursors[0];
        c.set_highlight_position(c.get_position());
        assert_eq!(c.get_highlight_position(), None);
    }

    buffer.insert('1');

    {
        let c = &buffer.cursors[0];
        assert_eq!(c.get_highlight_position(), None);
    }

    buffer.insert('2');

    {
        let c = &buffer.cursors[0];
        assert_eq!(c.get_highlight_position(), None);
    }
}

#[test]
fn insertion_with_forward_selection_deletes_selected_text() {
    // Arrange
    let mut buffer: TextBuffer = d!();
    buffer.insert('1');
    buffer.insert('2');
    buffer.insert('5'); // We will attempt to fix this as part of the test
    buffer.insert('4');

    // TODO move these sanity checks into a separate test?
    cursor_assert! {
        buffer,
        p: pos! {l 0 o 4},
        h: None
    }

    buffer.move_cursor(0, Move::Left);
    buffer.move_cursor(0, Move::Left);

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 2},
        h: None
    }

    buffer.extend_selection(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
        h: Some(pos! {l 0 o 2})
    }

    // Act
    buffer.insert('3');

    // Assert
    let s: String = buffer.rope.into();
    assert_eq!(s, "1234");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
        h: None
    }
}

#[test]
fn insert_string_places_cursor_at_the_end() {
    // Arrange
    let mut buffer = t_b!("125");

    buffer.move_cursor(0, Move::Right);
    buffer.move_cursor(0, Move::Right);

    // Act
    buffer.insert_string("34".into());

    // Assert
    let s: String = buffer.rope.into();
    assert_eq!(s, "12345");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 4},
        h: None
    }
}

#[test]
fn newline_places_cursor_in_correct_spot() {
    let mut buffer = t_b!("123");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 0},
    }

    for _ in 0..5 {
        buffer.move_cursor(0, Move::Right);
    }

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
    }

    buffer.insert('\n');

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 0},
    }
}

#[test]
fn right_does_not_go_to_the_next_line_if_there_is_not_one() {
    let mut buffer = t_b!("123\n567");

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 0},
    }

    buffer.move_cursor(0, Move::Down);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 0},
    }

    buffer.move_cursor(0, Move::Right);
    buffer.move_cursor(0, Move::Right);
    buffer.move_cursor(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 3},
    }

    buffer.move_cursor(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 3},
    }
}

#[test]
fn in_cursor_bounds_does_not_allow_going_past_a_line_feed() {
    let rope = r!("123\n567");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 5}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 4}), false);

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 3}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_does_not_allow_going_past_a_carriage_return_line_feed() {
    let rope = r!("123\r\n678");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 5}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 4}), false);

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 3}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_works_on_line_feed() {
    let rope = r!("\n");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 0}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 1}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 2}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_works_on_carriage_return_line_feed() {
    let rope = r!("\r\n");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 0}), true);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 1}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 0 o 2}), false);
    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), true);
}

#[test]
fn in_cursor_bounds_does_not_allow_going_to_a_non_existant_line() {
    let rope = r!("123");

    assert_eq!(in_cursor_bounds(&rope, pos! {l 1 o 0}), false);
}

fn moving_across_lines(mut buffer: TextBuffer) {
    cursor_assert! {
        buffer,
        p: pos! {l 0 o 0},
    }

    for _ in 0..3 {
        buffer.move_cursor(0, Move::Right);
    }

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
    }

    buffer.move_cursor(0, Move::Right);

    cursor_assert! {
        buffer,
        p: pos! {l 1 o 0},
    }

    buffer.move_cursor(0, Move::Left);

    cursor_assert! {
        buffer,
        p: pos! {l 0 o 3},
    }
}

#[test]
fn moving_across_line_feeds_works() {
    moving_across_lines(t_b!("123\n567"));
}

#[test]
fn moving_across_carriage_return_line_feeds_works() {
    moving_across_lines(t_b!("123\r\n567"));
}

macro_rules! all_cursor_movements {
    ($line_separator: literal) => {
        let mut buffer: TextBuffer = t_b!(concat!("123", $line_separator, "567"));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Right);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Down);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Up);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::Left);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToLineStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToBufferEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            s: CursorState::None
        }

        buffer.move_cursor(0, Move::ToBufferStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            s: CursorState::None
        }
    };
}

#[test]
fn all_cursor_movements_across_line_feeds_works() {
    all_cursor_movements!("\n");
}

#[test]
fn all_cursor_movements_across_carriage_return_line_feeds_works() {
    all_cursor_movements!("\r\n");
}

macro_rules! multiline_selection {
    ($line_separator: literal) => {
        let mut buffer: TextBuffer = t_b!(concat!("123", $line_separator, "567"));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None
        }

        buffer.extend_selection(0, Move::ToBufferEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: pos! {l 0 o 0}
        }

        buffer.move_cursor(0, Move::ToBufferEnd);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.extend_selection(0, Move::ToBufferStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: pos! {l 1 o 3}
        }

        buffer.move_cursor(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            h: None
        }

        buffer.extend_selection(0, Move::Right);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: pos! {l 0 o 3}
        }

        buffer.extend_selection(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: pos! {l 0 o 3}
        }

        buffer.extend_selection(0, Move::ToLineStart);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: pos! {l 0 o 3}
        }

        buffer.move_cursor(0, Move::Down);

        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.extend_selection(0, Move::Left);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            h: pos! {l 1 o 0},
        }

        buffer.extend_selection(0, Move::ToLineStart);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: pos! {l 1 o 0}
        }

        buffer.extend_selection(0, Move::ToLineEnd);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 3},
            h: pos! {l 1 o 0}
        }
    };
}

#[test]
fn multiline_selection_across_line_feeds_works() {
    multiline_selection!("\n");
}

#[test]
fn multiline_selection_across_carriage_return_line_feeds_works() {
    multiline_selection!("\r\n");
}

macro_rules! moving_by_words {
    ($line_separator: literal) => {
        use Move::*;
        let mut buffer: TextBuffer = t_b!(concat!(
            // non word before and after
            "(123)", $line_separator,
            // has non word before
            "\"456", $line_separator,
            // has non word after
            "789\"", $line_separator,
            // multiple words and tyes of non word charactera
            "{[(012), (345)]}", $line_separator,
        ));
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None
        }

        buffer.extend_selection(0, ToNextWordBoundary);

        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: pos! {l 0 o 0}
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 5},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 6},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 8},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 10},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 13},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 16},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 4 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 4 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        // Now back the other way

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 13},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 10},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 9},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 6},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 3 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 3},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 2 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 4},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 1},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        let mut alt_buffer: TextBuffer = t_b!(concat!(
            // starts with line
            $line_separator,
            //does not end with new line
            "123",
        ));

        alt_buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, Move::ToNextWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: CursorState::PressedAgainstWall
        }

        // Back to the beginning again

        alt_buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: d!()
        }

        alt_buffer.move_cursor(0, Move::ToPreviousWordBoundary);
        cursor_assert! {
            alt_buffer,
            p: pos! {l 0 o 0},
            h: None,
            s: CursorState::PressedAgainstWall
        }
    };
}

#[test]
fn moving_by_words_across_line_feeds_works() {
    moving_by_words!("\n");
}

#[test]
fn moving_by_words_across_carriage_return_line_feeds_works() {
    moving_by_words!("\r\n");
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
        Just(Move::ToPreviousWordBoundary),
        Just(Move::ToNextWordBoundary),
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

mod undo_redo;
