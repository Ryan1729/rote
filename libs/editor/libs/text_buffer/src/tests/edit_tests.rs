use super::*;
use cursors::curs;
use arb::get_counts;
use edit::{
    TAB_STR_CHAR_COUNT,
    RangeEdit,
    RangeEdits,
    get_cut_edit,
    get_delete_edit,
    get_delete_lines_edit,
    get_insert_edit,
    get_tab_in_edit,
    get_tab_out_edit,
    line_indicies_touched_by
};
use crate::{
    assert_text_buffer_eq_ignoring_history,
    r,
    t_b,
    tests::{
        arb::{TestEdit, TestEditSpec, *},
        deep_clone, SOME_AMOUNT, *,
    },
    TextBuffer,
};
use panic_safe_rope::is_linebreak_char;
use editor_types::{cur, Cursor};
use macros::{some_or, dbg};
use move_cursor::last_position;
use platform_types::{CursorState, vec1};
use pub_arb_edit::{edit_from_pieces, edit_range_edits_mut};
use rope_pos::{
    get_first_non_white_space_offset_in_range,
    get_last_non_white_space_offset_in_range
};

use pretty_assertions::assert_eq;
use proptest::prelude::*;
use proptest::{option, prop_compose, proptest};

use std::{
    collections::HashSet,
    borrow::Borrow,
};

#[test]
fn line_indicies_touched_by_counts_the_line_ahead_if_the_newline_is_included() {
    let rope = r!("0\n 1\n");

    let range = AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(2));

    let line_indicies = line_indicies_touched_by(&rope, range).unwrap();

    let expected = line_indicies_touched_by(
        &rope,
        AbsoluteCharOffsetRange::new(
            d!(),
            // Everything but the final `'\n'`.
            rope.len_chars() - 1,
        ),
    )
    .unwrap();

    assert_eq!(line_indicies, expected);
}

// Non-breaking space =
const NBSP: char = '\u{A0}';

#[test]
fn get_tab_in_edit_produces_the_expected_edit_from_this_buffer_with_different_leading_whitespace() {
    let text = format!("0\n 1\n  2\n   3\n    4\n\n{0}\n{0}1\n {0}2\n", NBSP);

    let mut buffer = t_b!(text.to_owned());
    buffer.select_all();

    let cursors = buffer.borrow_cursors();

    let edit = get_tab_in_edit(&buffer.rope);

    let expected = {
        let new_chars =
            "    0\n     1\n      2\n       3\n        4\n    \n     \n     1\n      2\n    "
                .to_owned();

        let new_rope = r!(new_chars);
        let mut cursor = Cursor::new(last_position(&new_rope).unwrap());
        cursor.set_highlight_position(Position {
            offset: CharOffset(TAB_STR_CHAR_COUNT),
            ..d!()
        });

        let insert_range = Some(RangeEdit {
            range: AbsoluteCharOffsetRange::new(
                d!(),
                AbsoluteCharOffset(new_chars.chars().count()),
            ),
            chars: new_chars,
        });

        let delete_range = Some(RangeEdit {
            range: AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(text.chars().count())),
            chars: text,
        });

        edit_from_pieces(
            Vec1::new(RangeEdits {
                insert_range,
                delete_range,
            }),
            Change {
                new: Cursors::new(&new_rope, Vec1::new(cursor)),
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_tab_in_edit_produces_the_expected_edit_with_multiple_cursors_in_this_buffer_with_different_leading_whitespace(
) {
    let text = format!("0\n 1\n  2\n   3\n    4\n\n{0}\n{0}1\n {0}2\n", NBSP);
    let start_of_empty_line = pos! {l 5 o 0};

    let mut buffer = t_b!(text.to_owned());
    buffer.set_cursor(start_of_empty_line, ReplaceOrAdd::Add);
    const RIGHT_COUNT: usize = 19;
    for _ in 0..RIGHT_COUNT {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    //pre-condition
    assert_eq!(
        buffer.borrow_cursors(),
        &Cursors::new(
            buffer.borrow_rope(),
            vec1![
                cur! {
                    l 9 o 0 h start_of_empty_line, ->|(Move::Right)
                },
                cur!(l 4 o 5 h l 0 o 0)
            ]
        )
    );

    let cursors = &buffer.borrow_cursors();

    let edit = get_tab_in_edit(&buffer.rope);

    let expected = {
        // Many things here rely on the example text being ASCII.
        const EXPECTED_TEXT: &'static str =
            "    0\n     1\n      2\n       3\n        4\n    \n     \n     1\n      2\n    ";
        const EXPECTED_CLEAVE_POINT: usize = RIGHT_COUNT + TAB_STR_CHAR_COUNT * 5;

        let new_cursors = {
            let expected_rope = r!(EXPECTED_TEXT.to_owned());
            let mut first_cursor = Cursor::new(pos! {l 0, o TAB_STR_CHAR_COUNT});
            let mut last_cursor = Cursor::new(start_of_empty_line);
            for _ in 0..TAB_STR_CHAR_COUNT {
                move_cursor::directly(&expected_rope, &mut last_cursor, Move::Right);
            }

            for _ in 0..EXPECTED_CLEAVE_POINT - TAB_STR_CHAR_COUNT {
                move_cursor::and_extend_selection(&expected_rope, &mut first_cursor, Move::Right);
                // we expect the last cursor to hit the end here.
                move_cursor::and_extend_selection(&expected_rope, &mut last_cursor, Move::Right);
            }
            // but we don't actually want to require that
            last_cursor.state = d!();

            Cursors::new(
                &expected_rope,
                vec1![last_cursor.clone(), first_cursor.clone()],
            )
        };
        dbg!(&new_cursors);
        // precondition
        assert_eq!(new_cursors.len(), 2);

        let first_range_edits = {
            let new_chars = (&EXPECTED_TEXT[..EXPECTED_CLEAVE_POINT]).to_owned();

            let insert_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(
                    d!(),
                    AbsoluteCharOffset(EXPECTED_CLEAVE_POINT),
                ),
                chars: new_chars,
            });

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(d!(), AbsoluteCharOffset(RIGHT_COUNT)),
                chars: (&text[..RIGHT_COUNT]).to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        let last_range_edits = {
            const START_OF_SECOND_HIGHLIGHT: usize = RIGHT_COUNT + 1;
            let chars = (&EXPECTED_TEXT[EXPECTED_CLEAVE_POINT + 1..]).to_owned();
            let insert_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(
                    AbsoluteCharOffset(START_OF_SECOND_HIGHLIGHT),
                    AbsoluteCharOffset(START_OF_SECOND_HIGHLIGHT + chars.chars().count()),
                ),
                chars,
            });

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(
                    AbsoluteCharOffset(START_OF_SECOND_HIGHLIGHT),
                    AbsoluteCharOffset(text.chars().count()),
                ),
                chars: (&text[START_OF_SECOND_HIGHLIGHT..]).to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        edit_from_pieces(
            vec1![last_range_edits, first_range_edits],
            Change {
                new: new_cursors,
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_tab_in_edit_produces_the_expected_change_when_two_cursors_are_on_the_same_line_in_this_case() {
    let text = format!("0\n  2\n");

    let mut buffer = t_b!(text.to_owned());
    buffer.set_cursor(pos! {l 1 o 2}, ReplaceOrAdd::Add);
    const RIGHT_COUNT: usize = 3;
    for _ in 0..RIGHT_COUNT {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    let edit = get_tab_in_edit(&buffer.rope);

    dbg!(&edit);

    apply_edit(&mut buffer.rope, &edit, None);

    let s: String = buffer.borrow_rope().into();
    //2 + (2 * 4) = 10 ___1234567890
    assert_eq!(s, "    0\n          2\n    ");
}

#[test]
fn get_tab_in_edit_produces_the_expected_change_when_three_cursors_are_on_the_same_line_in_this_case() {
    let text = format!("0\n       7\n");

    let mut buffer = t_b!(text.to_owned());
    buffer.set_cursor(pos! {l 1 o 2}, ReplaceOrAdd::Add);
    buffer.set_cursor(pos! {l 1 o 6}, ReplaceOrAdd::Add);
    const RIGHT_COUNT: usize = 3;
    for _ in 0..RIGHT_COUNT {
        buffer.extend_selection_for_all_cursors(Move::Right);
    }

    let edit = get_tab_in_edit(&buffer.rope);

    dbg!(&edit);

    apply_edit(&mut buffer.rope, &edit, None);

    let s: String = buffer.borrow_rope().into();
    //7 + (3 * 4) = 19 ___1234567890123456789
    assert_eq!(s, "    0\n                   7\n    ");
}

#[test]
fn get_delete_edit_produces_the_expected_change_in_this_case() {
    let mut buffer = t_b!("\n", vec1![cur!{l 0 o 0 h l 1 o 0}]);

    let edit = get_delete_edit(&buffer.rope);

    dbg!(&edit);

    apply_edit(&mut buffer.rope, &edit, None);

    let s: String = buffer.borrow_rope().into();

    assert_eq!(s, "");
}

fn select_all_followed_by_delete_deletes_everything_on(mut buffer: TextBuffer) {
    TestEdit::apply(&mut buffer, TestEdit::SelectAll);
    TestEdit::apply(&mut buffer, TestEdit::Delete);

    assert_eq!(buffer.borrow_rope().len_chars(), 0);
}

proptest! {
    #[test]
    fn select_all_followed_by_delete_deletes_everything(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        select_all_followed_by_delete_deletes_everything_on(buffer);
    }
}


fn select_all_followed_by_delete_lines_deletes_everything_on(mut buffer: TextBuffer) {
    TestEdit::apply(&mut buffer, TestEdit::SelectAll);
    TestEdit::apply(&mut buffer, TestEdit::DeleteLines);

    assert_eq!(buffer.borrow_rope().len_chars(), 0);
}

proptest! {
    #[test]
    fn select_all_followed_by_delete_lines_deletes_everything(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        select_all_followed_by_delete_lines_deletes_everything_on(buffer);
    }
}

#[test]
fn delete_lines_deletes_everything_in_this_two_line_case() {
    let mut buffer = t_b!("\na", vec1![cur!{l 0 o 0 h l 1 o 1}]);

    TestEdit::apply(&mut buffer, TestEdit::DeleteLines);

    assert_eq!(buffer.borrow_rope().len_chars(), 0);
}

#[test]
fn delete_lines_deletes_everything_in_this_reduced_two_line_case() {
    let mut buffer = t_b!("\na", vec1![cur!{l 0 o 0 h l 1 o 1}]);

    buffer.delete_lines(None);

    assert_eq!(buffer.borrow_rope().len_chars(), 0);
}

#[test]
fn get_first_non_white_space_offset_in_range_works_on_these_examples() {
    let rope = r!("\n \n 1\n  \n  2\n0\n1 \n2  \n333   \n");

    let mut lines = rope.lines();

    // Short for assert. We can be this terse since this is lexically scoped.
    macro_rules! a {
        ($line: expr, $range: expr => $expected: expr) => {{
            use std::ops::{RangeBounds, Bound::*};
            let start = match $range.start_bound() {
                Included(o) => Included(CharOffset(*o)),
                Excluded(o) => Excluded(CharOffset(*o)),
                Unbounded => Unbounded,
            };
            let end = match $range.end_bound() {
                Included(o) => Included(CharOffset(*o)),
                Excluded(o) => Excluded(CharOffset(*o)),
                Unbounded => Unbounded,
            };

            assert_eq!(
                get_first_non_white_space_offset_in_range(
                    $line,
                    (start, end)
                ),
                $expected
            );
        }}
    }

    let empty_line = lines.next().unwrap();
    a!(empty_line, .. => None);

    let one_space_line = lines.next().unwrap();
    a!(one_space_line, .. => None);

    let one_space_then_non_whitespace_line = lines.next().unwrap();
    a!(one_space_then_non_whitespace_line, 0..1 => None);
    a!(one_space_then_non_whitespace_line, .. => Some(CharOffset(1)));

    let two_spaces_line = lines.next().unwrap();
    a!(two_spaces_line, 0..1 => None);
    a!(two_spaces_line, .. => None);

    let two_spaces_then_non_whitespace_line = lines.next().unwrap();
    a!(two_spaces_then_non_whitespace_line, 0..1 => None);
    a!(two_spaces_then_non_whitespace_line, 0..2 => None);
    a!(two_spaces_then_non_whitespace_line, 1..2 => None);
    a!(two_spaces_then_non_whitespace_line, .. => Some(CharOffset(2)));
    a!(two_spaces_then_non_whitespace_line, 1.. => Some(CharOffset(2)));

    let one_non_whitespace_line = lines.next().unwrap();
    a!(one_non_whitespace_line, 0..1 => None);
    a!(one_non_whitespace_line, 0..2 => Some(CharOffset(0)));
    a!(one_non_whitespace_line, .. => Some(CharOffset(0)));

    let non_whitespace_then_one_space_line = lines.next().unwrap();
    a!(non_whitespace_then_one_space_line, 0..1 => None);
    a!(non_whitespace_then_one_space_line, 0..2 => Some(CharOffset(0)));
    a!(non_whitespace_then_one_space_line, .. => Some(CharOffset(0)));

    let non_whitespace_then_two_space_line = lines.next().unwrap();
    a!(non_whitespace_then_two_space_line, 0..1 => None);
    a!(non_whitespace_then_two_space_line, 0..2 => Some(CharOffset(0)));
    a!(non_whitespace_then_two_space_line, .. => Some(CharOffset(0)));

    let three_non_whitespace_then_three_space_line = lines.next().unwrap();
    a!(three_non_whitespace_then_three_space_line, 0..1 => None);
    a!(three_non_whitespace_then_three_space_line, 0..2 => Some(CharOffset(0)));
    a!(three_non_whitespace_then_three_space_line, .. => Some(CharOffset(0)));

    let empty_line_no_newline = lines.next().unwrap();
    a!(empty_line_no_newline, .. => None);

    assert_eq!(lines.next(), None, "test all the cases!");
}

#[test]
fn get_last_non_white_space_offset_in_range_works_on_these_examples() {
    let rope = r!("\n \n 1\n  \n  2\n0\n1 \n2  \n333   \n");

    let mut lines = rope.lines();

    // Short for assert. We can be this terse since this is lexically scoped.
    macro_rules! a {
        ($line: expr, $range: expr => $expected: expr) => {{
            use std::ops::{RangeBounds, Bound::*};
            let start = match $range.start_bound() {
                Included(o) => Included(CharOffset(*o)),
                Excluded(o) => Excluded(CharOffset(*o)),
                Unbounded => Unbounded,
            };
            let end = match $range.end_bound() {
                Included(o) => Included(CharOffset(*o)),
                Excluded(o) => Excluded(CharOffset(*o)),
                Unbounded => Unbounded,
            };

            assert_eq!(
                get_last_non_white_space_offset_in_range(
                    $line,
                    (start, end)
                ),
                $expected
            );
        }}
    }

    let empty_line = lines.next().unwrap();
    a!(empty_line, .. => None);

    let one_space_line = lines.next().unwrap();
    a!(one_space_line, .. => None);

    let one_space_then_non_whitespace_line = lines.next().unwrap();
    a!(one_space_then_non_whitespace_line, 0..1 => None);
    a!(one_space_then_non_whitespace_line, .. => Some(CharOffset(1)));

    let two_spaces_line = lines.next().unwrap();
    a!(two_spaces_line, 0..1 => None);
    a!(two_spaces_line, 0..2 => None);
    a!(two_spaces_line, .. => None);

    let two_spaces_then_non_whitespace_line = lines.next().unwrap();
    a!(two_spaces_then_non_whitespace_line, 0..1 => None);
    a!(two_spaces_then_non_whitespace_line, 0..2 => None);
    a!(two_spaces_then_non_whitespace_line, 1..2 => None);
    a!(two_spaces_then_non_whitespace_line, .. => Some(CharOffset(2)));
    a!(two_spaces_then_non_whitespace_line, 1.. => Some(CharOffset(2)));

    let one_non_whitespace_line = lines.next().unwrap();
    a!(one_non_whitespace_line, 0..1 => None);
    a!(one_non_whitespace_line, 0..2 => Some(CharOffset(0)));
    a!(one_non_whitespace_line, .. => Some(CharOffset(0)));

    let non_whitespace_then_one_space_line = lines.next().unwrap();
    a!(non_whitespace_then_one_space_line, 0..1 => None);
    a!(non_whitespace_then_one_space_line, 0..2 => Some(CharOffset(0)));
    a!(non_whitespace_then_one_space_line, .. => Some(CharOffset(0)));

    let non_whitespace_then_two_space_line = lines.next().unwrap();
    a!(non_whitespace_then_two_space_line, 0..1 => None);
    a!(non_whitespace_then_two_space_line, 0..2 => Some(CharOffset(0)));
    a!(non_whitespace_then_two_space_line, .. => Some(CharOffset(0)));

    let three_non_whitespace_then_three_space_line = lines.next().unwrap();
    a!(three_non_whitespace_then_three_space_line, 0..1 => None);
    a!(three_non_whitespace_then_three_space_line, 0..2 => Some(CharOffset(0)));
    a!(three_non_whitespace_then_three_space_line, 0..3 => Some(CharOffset(1)));
    a!(three_non_whitespace_then_three_space_line, 0..4 => Some(CharOffset(2)));
    a!(three_non_whitespace_then_three_space_line, .. => Some(CharOffset(2)));

    let empty_line_no_newline = lines.next().unwrap();
    a!(empty_line_no_newline, .. => None);

    assert_eq!(lines.next(), None, "test all the cases!");
}

fn tab_in_preserves_line_count_on(mut buffer: TextBuffer) {
    let line_count = buffer.borrow_rope().len_lines();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabIn);

        assert_eq!(line_count, buffer.borrow_rope().len_lines(), "iteration {}", i);
    }
}

proptest! {
    #[test]
    fn tab_in_preserves_line_count(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        tab_in_preserves_line_count_on(buffer);
    }
}

#[test]
fn tab_in_preserves_line_count_on_the_empty_rope() {
    tab_in_preserves_line_count_on(t_b!(""));
}

fn tab_out_preserves_line_count_on(mut buffer: TextBuffer) {
    let line_count = buffer.borrow_rope().len_lines();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        assert_eq!(
            line_count,
            buffer.borrow_rope().len_lines(),
            "iteration {}, rope: {:?}",
            i,
            buffer.rope
        );
    }
}

proptest! {
    #[test]
    fn tab_out_preserves_line_count(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        tab_out_preserves_line_count_on(buffer);
    }
}

#[test]
fn tab_out_preserves_line_count_on_the_empty_rope() {
    tab_out_preserves_line_count_on(t_b!(""));
}

#[test]
fn tab_out_preserves_line_count_on_this_generated_example() {
    let mut buffer = t_b!("�<AGL");

    buffer.set_cursors_from_vec1(vec1![
        Cursor::new_with_highlight(pos! {l 1 o 4}, pos! {l 2 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 3}, pos! {l 0 o 0})
    ]);

    tab_out_preserves_line_count_on(buffer);
}

#[test]
fn tab_out_preserves_line_count_on_this_shorter_generated_example() {
    let mut buffer = t_b!("\u{2028}");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 0 o 0},
        pos! {l 5 o 10}
    )]);

    tab_out_preserves_line_count_on(buffer);
}

#[test]
fn tab_out_preserves_line_count_on_this_reduced_example() {
    let mut buffer = t_b!("\n");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 0 o 0},
        pos! {l 3 o 0}
    )]);

    tab_out_preserves_line_count_on(buffer);
}

fn tab_in_then_tab_out_is_identity_on_regarding_ropes(initial_buffer: TextBuffer) {
    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);
    dbg!(&buffer);
    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(buffer.borrow_rope(), initial_buffer.borrow_rope());
}

proptest! {
    #[test]
    fn tab_in_then_tab_out_is_identity_on_all_space_buffers_regarding_ropes(
        initial_buffer in arb::all_space_text_buffer_with_many_cursors(),
    ) {
        tab_in_then_tab_out_is_identity_on_regarding_ropes(initial_buffer);
    }
}

#[test]
fn tab_in_then_tab_out_is_identity_on_a_single_newline_regarding_ropes() {
    tab_in_then_tab_out_is_identity_on_regarding_ropes(t_b!("\n"));
}

#[test]
fn tab_in_then_tab_out_is_identity_on_this_generated_example_regarding_ropes() {
    let mut buffer = t_b!("\n   \n\n");
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 0}, cur! {l 2 o 0 h l 0 o 0}]);
    tab_in_then_tab_out_is_identity_on_regarding_ropes(buffer);
}

#[test]
fn tab_in_acts_as_expected_on_this_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("\n   \n\n");
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 0}, cur! {l 2 o 0 h l 0 o 0}]);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    assert_eq!(&buffer.borrow_rope().to_string(), "    \n       \n    \n    ");
    assert_eq!(
        buffer.borrow_cursors(),
        &Cursors::new(
            buffer.borrow_rope(),
            vec1![cur! {l 3 o 4}, cur! {l 2 o 4 h l 0 o 4}]
        )
    );
}

#[test]
fn tab_in_acts_as_expected_on_this_simplified_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("\n   \n");
    buffer.set_cursors_from_vec1(vec1![cur! {l 2 o 0 h l 0 o 0}]);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    assert_eq!(buffer.borrow_rope().to_string(), "    \n       \n    ");
    assert_eq!(
        buffer.borrow_cursors(),
        &Cursors::new(
            buffer.borrow_rope(),
            vec1![cur! {l 2 o 4 h l 0 o 4}]
        )
    );
}

#[test]
fn tab_out_acts_as_expected_on_this_simplified_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("       \n    ");

    buffer.set_cursors_from_vec1(vec1![cur! {l 1 o 4 h l 0 o 4}]);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(&buffer.borrow_rope().to_string(), "   \n");
}

#[test]
fn tab_out_acts_as_expected_on_this_further_simplified_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("\n    ");

    buffer.set_cursors_from_vec1(vec1![cur! {l 1 o 4 h l 0 o 0}]);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(&buffer.borrow_rope().to_string(), "\n");
}

fn get_expected_tab_out_edit() -> Edit {
    let large_rope = get_large_rope();

    edit_from_pieces(
        vec1![
            RangeEdits {
                insert_range: Some(
                    RangeEdit {
                        chars: "\n".to_owned(),
                        range: AbsoluteCharOffsetRange::new_usize(0, 1),
                    },
                ),
                delete_range: Some(
                    RangeEdit {
                        chars: "\n    ".to_owned(),
                        range: AbsoluteCharOffsetRange::new_usize(0, 5),
                    },
                ),
            },
        ],
        Change {
            old: curs!{
                large_rope,
                cur!{l 1 o 4 h l 0 o 0},
            },
            new: curs!{
                large_rope,
                cur!{l 1 o 0 h l 0 o 0},
            },
        },
    )
}

#[test]
fn get_tab_out_edit_returns_the_expected_tab_edit_on_this_further_simplified_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("\n    ");

    buffer.set_cursors_from_vec1(vec1![cur! {l 1 o 4 h l 0 o 0}]);

    let edit = get_tab_out_edit(&buffer.rope);

    assert_eq!(edit, get_expected_tab_out_edit());
}

#[test]
fn changing_the_range_on_this_tab_out_edit_fixes_the_problem_from_this_further_simplified_example_based_on_the_above_generated_test() {
    let mut buffer = t_b!("\n    ");

    buffer.set_cursors_from_vec1(vec1![cur! {l 1 o 4 h l 0 o 0}]);

    let mut edit = get_tab_out_edit(&buffer.rope);

    let range_edit = &mut edit_range_edits_mut(&mut edit)[0];

    if let Some(d_r) = range_edit.delete_range.as_mut() {
        d_r.range = AbsoluteCharOffsetRange::new(
            AbsoluteCharOffset(0),
            AbsoluteCharOffset(5)
        );
    } else {
        assert!(false);
    };

    apply_edit(&mut buffer.rope, &edit, None);

    assert_eq!(&buffer.borrow_rope().to_string(), "\n");
}

#[test]
fn applying_this_tab_out_edit_has_the_expected_effect() {
    let mut buffer = t_b!("\n    ");

    buffer.set_cursors_from_vec1(vec1![cur! {l 1 o 4 h l 0 o 0}]);

    let edit = get_expected_tab_out_edit();

    apply_edit(&mut buffer.rope, &edit, None);

    assert_eq!(&buffer.borrow_rope().to_string(), "\n");
}

fn get_2_spaces_then_a_single_newline_and_particular_cursors() -> TextBuffer {
    let mut buffer = t_b!("  \n");
    // The reson these cursors are interesting is that they result in an offset pair of
    // `(None, Some(...))` after the first edit. This is not a desirable state, so we may want to
    // just make that state impossible.
    buffer.set_cursors_from_vec1(vec1![
        Cursor::new_with_highlight(pos! {l 6 o 3}, pos! {l 0 o 4}),
        Cursor::new_with_highlight(pos! {l 0 o 3}, pos! {l 0 o 0})
    ]);
    buffer
}

#[test]
fn tab_in_then_tab_out_is_identity_on_2_spaces_then_a_single_newline_and_particular_cursors_regarding_ropes(
) {
    tab_in_then_tab_out_is_identity_on_regarding_ropes(
        get_2_spaces_then_a_single_newline_and_particular_cursors(),
    );
}

#[test]
fn tab_in_is_as_expected_on_2_spaces_then_a_single_newline_and_particular_cursors_regarding_ropes()
{
    let mut buffer = get_2_spaces_then_a_single_newline_and_particular_cursors();

    TestEdit::apply(&mut buffer, TestEdit::TabIn);
    // I'm not positive this is actually what we should expect given the cursors are both starting
    // on the first line
    assert_eq!(buffer.borrow_rope(), r!("      \n    "));
}

#[test]
fn tab_out_results_in_2_spaces_then_a_single_newline_in_this_case() {
    let mut buffer = get_2_spaces_then_a_single_newline_and_particular_cursors();
    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    dbg!(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    assert_eq!(buffer.borrow_rope(), r!("  \n"));
}

#[test]
fn tab_out_then_tab_in_is_as_expected_on_three_spaces() {
    //                         three spaces    vvv
    let mut initial_buffer: TextBuffer = t_b!("   ");
    initial_buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 3}]);
    //                         four spaces      vvvv
    let mut expected_buffer: TextBuffer = t_b!("    ");
    expected_buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 4}]);

    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    TestEdit::apply(&mut buffer, TestEdit::TabIn);

    assert_eq!(buffer.rope, expected_buffer.rope);
}

#[test]
fn tab_out_is_as_expected_on_three_spaces() {
    //                     three spaces    vvv
    let mut initial_buffer: TextBuffer = t_b!("   ");
    initial_buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 3}]);
    let mut buffer = deep_clone(&initial_buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    let middle_buffer = deep_clone(&buffer);

    assert_text_buffer_eq_ignoring_history!(middle_buffer, t_b!(""));
}

proptest! {
    #[test]
    fn tab_in_preserves_non_white_space(
        mut buffer in arb::text_buffer_with_many_cursors(),
    ) {
        let expected: String = buffer.borrow_rope().chars().filter(|c| !c.is_whitespace()).collect();

        for i in 0..SOME_AMOUNT {
            TestEdit::apply(&mut buffer, TestEdit::TabIn);

            let actual: String = buffer.borrow_rope().chars().filter(|c| !c.is_whitespace()).collect();

            assert_eq!(actual, expected, "iteration {}", i);
        }
    }
}

fn tab_out_preserves_non_white_space_on(mut buffer: TextBuffer) {
    let expected: String = buffer.borrow_rope().chars()
        .filter(|c| !c.is_whitespace()).collect();

    for i in 0..SOME_AMOUNT {
        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        let actual: String = buffer.borrow_rope().chars()
            .filter(|c| !c.is_whitespace()).collect();

        assert_eq!(actual, expected, "iteration {}", i);
    }
}

proptest! {
    #[test]
    fn tab_out_preserves_non_white_space(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        tab_out_preserves_non_white_space_on(buffer);
    }
}

#[test]
fn tab_out_preserves_non_white_space_on_this_generated_example() {
    let mut buffer = t_b!(" 2b");
    buffer.set_cursors_from_vec1(vec1![
        Cursor::new(pos! {l 2 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 24}, pos! {l 0 o 1})
    ]);

    tab_out_preserves_non_white_space_on(buffer);
}

#[test]
fn tab_out_preserves_non_white_space_on_this_reduced_example() {
    let mut buffer = t_b!(" 2b");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 1 o 1},
        pos! {l 0 o 1}
    )]);

    tab_out_preserves_non_white_space_on(buffer);
}

#[test]
fn tab_out_preserves_non_white_space_on_this_reduced_in_a_different_way_example() {
    let mut buffer = t_b!(" 2b");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 1 o 0},
        pos! {l 0 o 1}
    )]);

    tab_out_preserves_non_white_space_on(buffer);
}

mod strip_trailing_whitespace_preserves_line_count {
    use super::{assert_eq, *};
    fn get_newline_count(rope: &Rope) -> usize {
        rope.chars().filter(|&c| c == '\n').count()
    }

    fn get_linebreak_count(rope: &Rope) -> usize {
        rope.chars().filter(|&c| is_linebreak_char(c)).count()
    }

    fn on(mut buffer: TextBuffer) {
        // The main thing is the line_count assert. If this is confusing, move the
        // other asserts into their own test I guess?
        let line_count = buffer.borrow_rope().len_lines();

        let newline_count = get_newline_count(buffer.borrow_rope());

        let has_non_nl_linebreaks = newline_count != get_linebreak_count(buffer.borrow_rope());

        for i in 0..SOME_AMOUNT {
            TestEdit::apply(&mut buffer, TestEdit::StripTrailingWhitespace);

            assert_eq!(buffer.borrow_rope().len_lines(), line_count, "line_count on iteration {}", i);

            if has_non_nl_linebreaks {
                continue;
            }

            assert_eq!(get_newline_count(buffer.borrow_rope()), newline_count, "newline_count on iteration {}", i);
        }
    }

    proptest! {
        #[test]
        fn arb_text_buffer_with_many_cursors(
            buffer in arb::text_buffer_with_many_cursors(),
        ) {
            on(buffer);
        }
    }

    #[test]
    fn on_this_found_example() {
        let mut buffer = t_b!("\u{2029}");
        buffer.set_cursors_from_vec1(vec1![cur!{l 1 o 0 h l 0 o 0}]);

        on(buffer);
    }

    #[test]
    fn on_this_found_asciified_example() {
        let mut buffer = t_b!("\r");
        buffer.set_cursors_from_vec1(vec1![cur!{l 1 o 0 h l 0 o 0}]);

        on(buffer);
    }

    #[test]
    fn on_this_blank_line_between_example() {
        let mut buffer = t_b!("a    \n     \nb    ");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 2 o 5}]);

        on(buffer);
    }

    #[test]
    fn on_this_blank_line_between_example_reduction() {
        let mut buffer = t_b!("a    \n     \nb    ");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 2 o 5}]);

        let line_count = buffer.borrow_rope().len_lines();

        let stw_edit = edit::get_strip_trailing_whitespace_edit(&buffer.rope);

        let inserted_chars = &stw_edit.range_edits().first().insert_range.as_ref().unwrap().chars;

        let actual = inserted_chars.lines().count();
        assert_eq!(actual, line_count.0, "{:?} has the wrong number of lines", inserted_chars);
    }

    #[test]
    fn on_this_blank_line_before_example() {
        let mut buffer = t_b!("     \nb    ");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 1 o 5}]);

        on(buffer);
    }

    #[test]
    fn on_this_blank_line_after_example() {
        let mut buffer = t_b!("a    \n     ");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 1 o 5}]);

        on(buffer);
    }

    #[test]
    fn on_this_blank_line_example() {
        let mut buffer = t_b!("     \n");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 1 o 0}]);

        on(buffer);
    }

    #[test]
    fn on_this_multiline_trailing_newline_partial_select_example() {
        let mut buffer = t_b!("a    \n     \nb    \nnon-selected\n");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 3 o 0}]);

        on(buffer);
    }

    #[test]
    fn on_this_multiline_trailing_newline_partial_select_example_reduction() {
        let mut buffer = t_b!("a    \n     \nb    \nnon-selected\n");
        buffer.set_cursors_from_vec1(vec1![cur!{l 0 o 0 h l 3 o 0}]);

        let line_count = buffer.borrow_rope().len_lines();

        let stw_edit = edit::get_strip_trailing_whitespace_edit(&buffer.rope);

        let inserted_chars = &stw_edit.range_edits().first().insert_range.as_ref().unwrap().chars;
        assert_eq!(inserted_chars, "a\n\nb\n");

        buffer.rope.apply(&stw_edit);

        assert_eq!(buffer.borrow_rope().len_lines(), line_count, "line_count mismatch");
    }
}

fn get_code_like_example() -> TextBuffer {
    // like this:
    /*
    {
        {
            A
        }
    }
    */
    let mut buffer = t_b!("{\n    {\n        A\n    }\n}\n");
    buffer.set_cursors_from_vec1(vec1![Cursor::new_with_highlight(
        pos! {l 1 o 0},
        pos! {l 3 o 5}
    )]);
    buffer
}

#[test]
fn tab_out_produces_the_expected_string_on_this_code_like_example() {
    let mut buffer = get_code_like_example();

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    assert_eq!(buffer.borrow_rope(), r!("{\n{\n    A\n}\n}\n"));
}

#[test]
fn tab_out_places_the_cursors_correctly_on_this_code_like_example() {
    let mut buffer = get_code_like_example();

    TestEdit::apply(&mut buffer, TestEdit::TabOut);
    assert_eq!(buffer.borrow_cursors(), &curs!(buffer.borrow_rope(), cur!(l 1 o 0 h l 3 o 1)));
}

#[test]
fn tab_in_places_the_cursors_correctly_on_this_edge_case_example() {
    let mut buffer = t_b!("{\n    A\n}\n");
    buffer.set_cursors_from_vec1(vec1![cur!(l 1 o 3 h l 2 o 1 ),]);

    TestEdit::apply(&mut buffer, TestEdit::TabIn);
    assert_eq!(buffer.borrow_cursors(), &curs!(buffer.borrow_rope(), cur!(l 1 o 7 h l 2 o 5 )));
}

// several of the next tests were based on a generated test about the effect of n inserts and then n deletes
#[test]
fn get_insert_edit_produces_the_expected_edit_on_this_cr_lf_edit_example() {
    let mut buffer = t_b!("\rA");
    buffer.set_cursors_from_vec1(vec1![Cursor::new(pos! {l 1 o 0})]);

    let cursors = &buffer.borrow_cursors();

    // Act
    let edit = get_insert_edit(&buffer.rope, |_| "\n".to_owned());

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("\r\nA".to_owned());

            Cursors::new(&expected_rope, vec1![cur! {l 1 o 0}])
        };

        let range_edits = {
            let insert_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(AbsoluteCharOffset(1), AbsoluteCharOffset(2)),
                chars: "\n".to_owned(),
            });

            let delete_range = None;
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        edit_from_pieces(
            vec1![range_edits],
            Change {
                new: new_cursors,
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

// this was based on a generated test about the effect of n inserts and then n deletes
#[test]
fn get_delete_edit_produces_the_expected_edit_on_this_cr_lf_edit_example() {
    // Arrange
    let mut buffer = t_b!("A");

    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\r'));
    arb::TestEdit::apply(&mut buffer, TestEdit::Insert('\n'));

    let cursors = &buffer.borrow_cursors();

    // Act
    let edit = get_delete_edit(&buffer.rope);

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("\rA".to_owned());

            Cursors::new(&expected_rope, vec1![cur! {l 1 o 0}])
        };

        let range_edits = {
            let insert_range = None;

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(AbsoluteCharOffset(1), AbsoluteCharOffset(2)),
                chars: "\n".to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        edit_from_pieces(
            vec1![range_edits],
            Change {
                new: new_cursors,
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_delete_lines_edit_produces_the_expected_edit_on_this_backslash_example() {
    // Arrange
    let buffer = t_b!("\n\\\n\n\\a", vec1![
        cur!{l 3 o 2},
        cur!{l 3 o 1}
    ]);

    let cursors = &buffer.borrow_cursors();

    // Act
    let edit = get_delete_lines_edit(&buffer.rope);

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("\n\\\n".to_owned());

            Cursors::new(&expected_rope, vec1![cur! {l 2 o 0}])
        };

        let range_edits = {
            let insert_range = None;

            let delete_range = Some(RangeEdit {
                range: AbsoluteCharOffsetRange::new(AbsoluteCharOffset(3), AbsoluteCharOffset(6)),
                chars: "\n\\a".to_owned(),
            });
            RangeEdits {
                insert_range,
                delete_range,
            }
        };

        edit_from_pieces(
            vec1![range_edits],
            Change {
                new: new_cursors,
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_insert_edit_produces_the_expected_edit_on_this_multi_byte_char_example() {
    let mut buffer = t_b!("Aa 0");
    buffer.set_cursors_from_vec1(vec1![cur! {l 0 o 2}, cur! {l 0 o 1}]);

    let cursors = &buffer.borrow_cursors();

    // Act
    let edit = get_insert_edit(&buffer.rope, |_| "¡".to_owned());

    // Assert
    let expected = {
        let new_cursors = {
            //                     "A¡|a¡| 0"
            let expected_rope = r!("A¡a¡ 0".to_owned());

            Cursors::new(
                &expected_rope,
                vec1![Cursor::new(pos! {l 0 o 4}), Cursor::new(pos! {l 0 o 2})],
            )
        };

        let range_edits = {
            vec1![
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(2),
                            AbsoluteCharOffset(3)
                        ),
                        chars: "¡".to_owned(),
                    }),
                    delete_range: None,
                },
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(1),
                            AbsoluteCharOffset(2)
                        ),
                        chars: "¡".to_owned(),
                    }),
                    delete_range: None,
                }
            ]
        };

        edit_from_pieces(
            range_edits,
            Change {
                new: new_cursors,
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

#[test]
fn get_insert_edit_produces_the_expected_edit_on_this_multi_cursor_cr_lf_example() {
    let mut buffer = t_b!("1\r2\r3\r4");
    buffer.set_cursors_from_vec1(vec1![cur! {l 3 o 0}, cur! {l 2 o 0}, cur! {l 1 o 0}]);

    let cursors = &buffer.borrow_cursors();

    // Act
    let edit = get_insert_edit(&buffer.rope, |_| "\n".to_owned());

    // Assert
    let expected = {
        let new_cursors = {
            let expected_rope = r!("1\r\n2\r\n3\r\n4".to_owned());

            Cursors::new(
                &expected_rope,
                vec1![cur! {l 3 o 0}, cur! {l 2 o 0}, cur! {l 1 o 0}],
            )
        };

        let range_edits = {
            vec1![
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(6),
                            AbsoluteCharOffset(7)
                        ),
                        chars: "\n".to_owned(),
                    }),
                    delete_range: None,
                },
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(4),
                            AbsoluteCharOffset(5)
                        ),
                        chars: "\n".to_owned(),
                    }),
                    delete_range: None,
                },
                RangeEdits {
                    insert_range: Some(RangeEdit {
                        range: AbsoluteCharOffsetRange::new(
                            AbsoluteCharOffset(2),
                            AbsoluteCharOffset(3)
                        ),
                        chars: "\n".to_owned(),
                    }),
                    delete_range: None,
                }
            ]
        };

        edit_from_pieces(
            range_edits,
            Change {
                new: new_cursors,
                old: (*cursors).clone(),
            },
        )
    };

    assert_eq!(edit, expected);
}

fn does_not_lose_characters_on<TestEdits: Borrow<[TestEdit]>>(
    initial_buffer: TextBuffer,
    edits: TestEdits,
) {
    let mut counts = get_counts(&initial_buffer);
    let mut buffer = deep_clone(&initial_buffer);

    for edit in edits.borrow().iter() {
        TestEdit::apply_with_counts(&mut buffer, &mut counts, edit);
    }

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

proptest! {
    #[test]
    fn does_not_lose_characters((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::All)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::Insert)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_non_control_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::RegexInsert("\\PC"))) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_non_cr_inserts((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::RegexInsert("[^\r]"))) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_set_cursor_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::SetCursorHeavy)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_tab_in_out_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::TabInOutHeavy)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_delete_and_tab_in_out_heavy((buffer, edits) in arb::text_buffer_and_test_edits(SOME_AMOUNT, TestEditSpec::DeleteAndTabInOutHeavy)) {
        does_not_lose_characters_on(buffer, edits);
    }

    #[test]
    fn does_not_lose_characters_on_delete_then_tab_out(buffer in arb::text_buffer_with_valid_cursors(), edits in arb::test_edit_delete_then_tab_out_vec()) {
        does_not_lose_characters_on(buffer, edits);
    }
}

#[test]
fn does_not_lose_characters_in_this_generated_case() {
    use TestEdit::*;
    does_not_lose_characters_on(
        t_b!(""),
        [InsertString("\u{b}".to_string()), DragCursors(pos!{l 0 o 0}), TabIn]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_generated_case() {
    use TestEdit::*;

    let mut buffer = t_b!("");
    let mut counts = get_counts(&buffer);


    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &InsertString("\n".to_string()));
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &DragCursors(pos!{l 0 o 0}));
    dbg!(&buffer);
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(get_counts(&buffer), &counts);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_this_tab_in_tab_in_tab_out_case() {
    use TestEdit::*;
    does_not_lose_characters_on(
        t_b!(""),
        [TabIn, TabIn, TabOut]
    );
}

#[test]
fn does_not_lose_characters_in_this_extend_selection_case() {
    use TestEdit::*;
    does_not_lose_characters_on(
        t_b!(""),
        [
            InsertString("\u{b}\t".to_owned()),
            ExtendSelectionForAllCursors(Move::ToBufferStart),
            TabIn
        ]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_extend_selection_case() {
    use TestEdit::*;

    let mut buffer = t_b!("");
    let mut counts = get_counts(&buffer);


    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &InsertString("\u{b}\t".to_owned()));
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &ExtendSelectionForAllCursors(Move::ToBufferStart));
    dbg!(&buffer);
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(get_counts(&buffer), &counts);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_this_further_reduced_extend_selection_case() {
    use TestEdit::*;

    let mut buffer = t_b!("");
    let mut counts = get_counts(&buffer);


    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &InsertString("\u{b}\t".to_owned()));
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &ExtendSelectionForAllCursors(Move::ToBufferStart));
    dbg!(&buffer);
    dbg!(get_counts(&buffer), &counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(get_counts(&buffer), &counts);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_this_delete_lines_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{2028}ૠ�🌀");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);
    does_not_lose_characters_on(
        buffer,
        [DeleteLines]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_delete_lines_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\na");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);
    does_not_lose_characters_on(
        buffer,
        [DeleteLines]
    );
}

#[test]
fn does_not_lose_characters_in_this_single_newline_delete_lines_case() {
    use TestEdit::*;

    does_not_lose_characters_on(
        t_b!("\n"),
        [DeleteLines]
    );
}

#[test]
fn does_not_lose_characters_in_this_more_complicated_delete_lines_case() {
    use TestEdit::*;

    does_not_lose_characters_on(
        t_b!("a"),
        [
            Insert('\n'),
            InsertString("\\\u{b}\u{b}\\".to_string()),
            SetCursor(pos!{l 3 o 2}, ReplaceOrAdd::Add),
            DeleteLines
        ]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_more_complicated_delete_lines_case() {
    use TestEdit::*;

    let mut buffer = t_b!("\n\\\u{b}\u{b}\\a", vec1![
        cur!{l 3 o 2},
        cur!{l 3 o 1}
    ]);
    let mut counts = get_counts(&buffer);

    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &DeleteLines);
    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_this_further_reduced_more_complicated_delete_lines_case() {
    use TestEdit::*;

    let mut buffer = t_b!("\n\\\n\n\\a", vec1![
        cur!{l 3 o 2},
        cur!{l 3 o 1}
    ]);
    let mut counts = get_counts(&buffer);

    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &DeleteLines);
    dbg!(get_counts(&buffer), &counts);
    dbg!(&buffer);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_minimal_tab_in_case() {
    use TestEdit::*;
    does_not_lose_characters_on(
        t_b!(""),
        [TabIn]
    );
}

#[test]
fn does_not_lose_characters_in_this_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    does_not_lose_characters_on(
        buffer,
        [TabIn]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    let mut counts = get_counts(&buffer);

    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}


#[test]
fn does_not_lose_characters_in_this_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let mut buffer = t_b!("\u{a0}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);
    does_not_lose_characters_on(
        buffer,
        [
            SetCursor(pos!{l 0 o 0}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            Delete,
            SetCursor(pos!{l 0 o 2}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            TabIn
        ]
    );
}

#[test]
fn does_not_lose_characters_in_this_two_space_then_zero_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let buffer = t_b!("  0");

    does_not_lose_characters_on(
        buffer,
        [
            SetCursor(pos!{l 0 o 0}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            Delete,
            SetCursor(pos!{l 0 o 2}, Add),
            ExtendSelectionForAllCursors(Up),
            ExtendSelectionForAllCursors(Left),
            TabIn
        ]
    );
}

#[test]
fn does_not_lose_characters_in_this_reduced_two_space_then_zero_delete_then_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    use Move::*;
    let initial_buffer = t_b!("  0");

    let mut counts = get_counts(&initial_buffer);
    let mut buffer = deep_clone(&initial_buffer);

    let edits = [
            SetCursor(pos!{l 0 o 0}, Add),
            SetCursor(pos!{l 0 o 2}, Add),
            ExtendSelectionForAllCursors(Left),
            TabIn
        ];

    for edit in edits.iter() {
        TestEdit::apply_with_counts(&mut buffer, &mut counts, edit);
    }

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn tab_in_does_what_is_expected_with_this_selection() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(" 0");
    buffer.set_cursor(cur!{l 0 o 1 h l 0 o 0}, Replace);

    let edit = dbg!(get_tab_in_edit(&buffer.rope));

    apply_edit(&mut buffer.rope, &edit, None);

    let mut expected_buffer = t_b!("     0");
    expected_buffer.set_cursor(cur!{l 0 o 5 h l 0 o 4}, Replace);

    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer);
}

// Large enough that the cursors don't get clamped.
fn get_large_rope() -> Rope {
    r!(
    r"1234567890abcdef
1234567890abcdef
1234567890abcdef
1234567890abcdef"
    )
}

fn get_expected_tab_in_edit() -> Edit {
    let large_rope = get_large_rope();

    edit_from_pieces(
        vec1![
            RangeEdits {
                insert_range: Some(
                    RangeEdit {
                        chars: "     ".to_owned(),
                        range: AbsoluteCharOffsetRange::new_usize(0, 5),
                    },
                ),
                delete_range: Some(
                    RangeEdit {
                        chars: " ".to_owned(),
                        range: AbsoluteCharOffsetRange::new_usize(0, 1),
                    },
                ),
            },
        ],
        Change {
            old: curs!{
                large_rope,
                cur!{l 0 o 1 h l 0 o 0},
            },
            new: curs!{
                large_rope,
                cur!{l 0 o 5 h l 0 o 4},
            },
        },
    )
}

#[test]
fn this_tab_in_edit_does_what_is_expected_with_this_selection() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(" 0");
    buffer.set_cursor(cur!{l 0 o 1 h l 0 o 0}, Replace);

    let edit = get_expected_tab_in_edit();

    apply_edit(&mut buffer.rope, &edit, None);

    let mut expected_buffer = t_b!("     0");
    expected_buffer.set_cursor(cur!{l 0 o 5 h l 0 o 4}, Replace);

    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer);
}

#[test]
fn get_tab_in_edit_produces_the_expected_edit_with_this_selection() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(" 0");
    buffer.set_cursor(cur!{l 0 o 1 h l 0 o 0}, Replace);

    assert_eq!(
        get_tab_in_edit(&buffer.rope),
        get_expected_tab_in_edit()
    );
}

#[test]
fn does_not_lose_characters_in_this_set_cursor_heavy_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("a ");
    buffer.set_cursor(cur!{l 0 o 0 h l 0 o 1}, Replace);
    does_not_lose_characters_on(
        buffer,
        [SetCursor(pos!{l 0 o 2}, Add), Delete]
    );
}

#[test]
fn does_not_lose_characters_in_this_tab_out_then_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);

    let mut counts = get_counts(&buffer);

    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabOut);
    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);
    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn get_cut_edit_returns_an_edit_with_the_right_selection_in_this_tab_out_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);

    TestEdit::apply(&mut buffer, TabOut);

    let cut_edit = get_cut_edit(&buffer.rope);

    assert_eq!(cut_edit.selected(), vec!["!"]);
}

#[test]
fn get_tab_out_edit_returns_an_edit_with_the_right_selection_in_this_case() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);
    buffer.set_cursor(cursor, Replace);

    let edit = get_tab_out_edit(&buffer.rope);

    assert_eq!(edit.selected(), vec!["!"]);

    apply_edit(&mut buffer.rope, dbg!(&edit), None);

    assert_eq!(buffer.borrow_rope(), r!("!\u{2000}"));
}

#[test]
fn does_not_lose_characters_in_this_two_wide_selection_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 2 h l 0 o 0};
    cursor.sticky_offset = d!();
    buffer.set_cursor(cursor, Replace);

    let mut counts = get_counts(&buffer);

    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);

    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn does_not_lose_characters_in_this_two_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("A");

    buffer.set_cursor(cur!{l 0 o 0 h l 0 o 1}, Replace);

    let mut counts = get_counts(&buffer);

    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &SetCursor(pos!{l 0 o 0}, Add));
    dbg!(&counts);
    TestEdit::apply_with_counts(&mut buffer, &mut counts, &TabIn);
    dbg!(&counts);
    counts.retain(|_, v| *v != 0);

    assert_eq!(get_counts(&buffer), counts);
}

#[test]
fn copy_selections_returns_what_is_expected_in_this_two_tab_in_case() {
    use TestEdit::*;
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("A");

    buffer.set_cursor(cur!{l 0 o 0 h l 0 o 1}, Replace);

    TestEdit::apply(&mut buffer, TabIn);
    TestEdit::apply(&mut buffer, SetCursor(pos!{l 0 o 0}, Add));
    TestEdit::apply(&mut buffer, TabIn);

    assert_eq!(buffer.copy_selections(), vec!["A"]);
}

proptest!{
    #[test]
    fn get_cut_edit_does_not_affect_a_lone_cursor_if_there_is_no_selection(buffer in arb::text_buffer_with_no_selection()) {
        let expected = buffer.borrow_cursors().clone();

        let edit = get_cut_edit(&buffer.rope);

        assert_eq!(&edit.cursors().new, &expected);
        assert_eq!(edit.cursors().new, edit.cursors().old);
    }
}

#[test]
fn get_cut_edit_does_not_affect_a_lone_cursor_if_there_is_no_selection_in_this_simple_case() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!(" ");

    buffer.set_cursor(cur!{l 0 o 1}, Replace);

    let expected = buffer.borrow_cursors().clone();

    let edit = get_cut_edit(&buffer.rope);

    assert_eq!(&edit.cursors().new, &expected);
    assert_eq!(edit.cursors().new, edit.cursors().old);
}

proptest!{
    #[test]
    fn tab_out_does_not_change_the_count_of_non_whitespace_chars(mut buffer in arb::text_buffer_with_valid_cursors()) {
        let old_counts = get_counts(&buffer);

        TestEdit::apply(&mut buffer, TestEdit::TabOut);

        let new_counts = get_counts(&buffer);

        for key in old_counts.keys() {
            if key.is_whitespace() {
                continue
            }

            let old = old_counts.get(key).unwrap_or(&0);
            let new = new_counts.get(key).unwrap_or(&0);

            assert_eq!(new, old, "key: '{}' ({})", key, key.escape_unicode());
        }
    }
}

#[test]
fn tab_out_does_not_change_the_count_of_non_whitespace_chars_in_this_case() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("!");

    let mut cursor = cur!{l 0 o 1};
    cursor.sticky_offset = CharOffset(0);
    buffer.set_cursor(cursor, Replace);

    let old_counts = get_counts(&buffer);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    let new_counts = get_counts(&buffer);

    for key in old_counts.keys() {
        if key.is_whitespace() {
            continue
        }

        let old = old_counts.get(key).unwrap_or(&0);
        let new = new_counts.get(key).unwrap_or(&0);

        assert_eq!(new, old, "key: '{}' ({})", key, key.escape_unicode());
    }
}

#[test]
fn tab_out_does_not_change_the_count_of_non_whitespace_chars_in_this_unicode_case() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{2028}�");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);

    let old_counts = get_counts(&buffer);
    dbg!(&old_counts);

    TestEdit::apply(&mut buffer, TestEdit::TabOut);

    let new_counts = get_counts(&buffer);
    dbg!(&new_counts);

    for key in old_counts.keys() {
        if key.is_whitespace() {
            continue
        }

        let old = old_counts.get(key).unwrap_or(&0);
        let new = new_counts.get(key).unwrap_or(&0);

        assert_eq!(new, old, "key: '{}' ({})", key, key.escape_unicode());
    }
}

#[test]
fn get_tab_out_edit_returns_the_right_chars_in_this_unicode_case() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\u{2028}�");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);

    let edit = edit::get_tab_out_edit(&buffer.rope);

    assert_eq!(edit.range_edits().len(), 1);

    let range_edit = edit.range_edits().first().clone();

    // tab out here should not cause any changes to the chars
    assert_eq!(
        range_edit.insert_range.unwrap().chars,
        range_edit.delete_range.unwrap().chars
    );
}

#[test]
fn get_tab_out_edit_returns_the_right_chars_in_this_ascii_case() {
    use ReplaceOrAdd::*;
    let mut buffer = t_b!("\nA");

    buffer.set_cursor(cur!{l 0 o 0 h l 1 o 0}, Replace);

    let edit = edit::get_tab_out_edit(&buffer.rope);

    assert_eq!(edit.range_edits().len(), 1);

    let range_edit = edit.range_edits().first().clone();

    // tab out here should not cause any changes to the chars
    assert_eq!(
        range_edit.insert_range.unwrap().chars,
        range_edit.delete_range.unwrap().chars
    );
}

fn delete_lines_deletes_the_expected_amount_of_lines_on(mut buffer: TextBuffer) {
    let initial_line_count = buffer.borrow_rope().len_lines();

    let mut line_indicies = HashSet::with_capacity(buffer.borrow_rope().len_lines().0);

    for c in buffer.borrow_cursors().iter() {
        let offsets = offset_pair(buffer.borrow_rope(), c);
        match offsets {
            (Some(o1), offset2) => {
                let o2 = offset2.unwrap_or(o1);
                let range = AbsoluteCharOffsetRange::new(o1, o2);
                for index in some_or!(line_indicies_touched_by(buffer.borrow_rope(), range), continue) {
                    line_indicies.insert(index.0);
                }
            }
            _ => {}
        }
    }

    let expected_line_count = std::cmp::max((initial_line_count - line_indicies.len()).0, 1);

    TestEdit::apply(&mut buffer, TestEdit::DeleteLines);

    assert_eq!(
        buffer.borrow_rope().len_lines().0,
        expected_line_count,
        "started with {} lines and expected lines {:?} ({} total) to be deleted, but got the wrong count. The rope ended up as {:?}",
        initial_line_count.0,
        line_indicies,
        line_indicies.len(),
        buffer.rope
    );
}

proptest! {
    #[test]
    fn delete_lines_deletes_the_expected_amount_of_lines(
        buffer in arb::text_buffer_with_many_cursors(),
    ) {
        delete_lines_deletes_the_expected_amount_of_lines_on(buffer);
    }
}

#[test]
fn delete_lines_deletes_the_expected_amount_of_lines_in_this_small_case() {
    delete_lines_deletes_the_expected_amount_of_lines_on(
        t_b!("\u{2028}", vec1![cur!{l 0 o 0 h l 1 o 0}]),
    );
}

#[test]
fn delete_lines_deletes_the_expected_amount_of_lines_in_this_less_small_case() {
    delete_lines_deletes_the_expected_amount_of_lines_on(
        t_b!("\u{2029}A", vec1![cur!{l 1 o 0}]),
    );
}

#[test]
fn delete_lines_deletes_the_expected_amount_of_lines_in_this_reduced_less_small_case() {
    delete_lines_deletes_the_expected_amount_of_lines_on(
        t_b!("\nA", vec1![cur!{l 1 o 0}]),
    );
}

mod included_files;
mod undo_redo;
