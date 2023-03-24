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

fn char_offset_to_pos_to_char_offset(rope: &Rope, offset: AbsoluteCharOffset) {
    if let Some(p) = char_offset_to_pos(&rope, offset) {
        assert_eq!(pos_to_char_offset(&rope, &p), Some(offset))
    }
}

proptest! {
    #[test]
    fn char_offset_to_pos_to_char_offset_works((rope, offset) in arb_rope_and_offset()) {
        char_offset_to_pos_to_char_offset(&rope, offset);
    }

    #[test]
    fn pos_to_to_char_offset_to_pos_works((rope, pos) in arb_rope_and_pos()) {
        if let Some(o) = pos_to_char_offset(&rope, &pos) {
            assert_eq!(char_offset_to_pos(&rope, o), Some(pos))
        }
    }
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
fn char_offset_to_pos_to_char_offset_works_on_final_offset() {
    let rope = r!("A");
    let o = AbsoluteCharOffset(1);
    char_offset_to_pos_to_char_offset(&rope, o);
}

#[test]
fn pos_to_to_char_offset_to_pos_works_on_final_offset() {
    let rope = r!("A");
    let p = pos! {l 0 o 1};
    pos_to_to_char_offset_to_pos(&rope, p);
}

#[test]
fn char_offset_to_pos_to_char_offset_on_final_offset_on_non_ascii() {
    let rope = r!("¡");
    let o = AbsoluteCharOffset(1);
    char_offset_to_pos_to_char_offset(&rope, o);
}

#[test]
fn pos_to_to_char_offset_to_pos_works_on_final_offset_on_non_ascii() {
    let rope = r!("¡");
    let p = pos! {l 0 o 1};
    pos_to_to_char_offset_to_pos(&rope, p);
}

#[test]
fn pos_to_to_char_offset_works_on_final_offset_on_non_ascii() {
    let rope = r!("¡");
    let p = pos! {l 0 o 1};
    assert_eq!(pos_to_char_offset(&rope, &p), Some(AbsoluteCharOffset(1)))
}

#[test]
fn clamp_position_works_on_this_example() {
    let rope = r!("\n1234567890");
    let p = pos! {l 0 o 10};
    assert_eq!(clamp_position(&rope, p), pos! {l 0 o 0});
}

prop_compose! {
    fn arb_offset_pair()(
        o1 in option::of(arb::absolute_char_offset(SOME_AMOUNT)),
        o2 in option::of(arb::absolute_char_offset(SOME_AMOUNT))
    ) -> OffsetPair {
        (o1, o2)
    }
}

proptest! {
    #[test]
    fn nearest_valid_position_on_same_line_is_identity_for_positions_in_bounds(
        rope in arb::rope(),
        position in arb::pos(SOME_AMOUNT, SOME_AMOUNT)
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

    buffer.insert('5', None);

    let expected = "5000\n1511\n2252\n3335\n5".to_owned();
    let actual: String = buffer.borrow_rope().into();

    assert_eq!(actual, expected);
}

fn buffer_inserts_all_the_requested_numbers_in_order(buffer: &TextBuffer) {
    let mut buffer = deep_clone(buffer);

    let cursors_len = buffer.borrow_cursors().len();
    assert!(
        cursors_len <= 9,
        "this test is only valid if there are less than 10 cursors."
    );

    let get_string = |i: usize| i.to_string();

    buffer.insert_at_each_cursor(get_string, None);

    let expected_strings = (0..cursors_len).map(get_string);

    let buffer_string: String = buffer.borrow_rope().into();

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

    buffer.set_cursors_from_vec1(vec1![
        Cursor::new_with_highlight(pos! {l 0 o 0}, pos! {l 1 o 0}),
        Cursor::new_with_highlight(pos! {l 1 o 0}, pos! {l 0 o 0})
    ]);

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
const OUTSIDE_ROPE_BOUNDS: u8 = 0b100;

fn cursor_vec1_maintains_invariants(rope: &Rope, cursors: &Vec1<Cursor>) -> u8 {
    use std::cmp::Ordering;
    let mut spans = cursors
        .mapped_ref(|c| {
            let p = c.get_position();
            let h = c.get_highlight_position_or_position();

            (std::cmp::min(p, h), std::cmp::max(p, h))
        })
        .into_vec();
    //
    // Ordering check
    //
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

    //
    // Bounds check
    //
    for (p1, p2) in spans.iter() {
        if in_cursor_bounds(rope, p1) && in_cursor_bounds(rope, p2) {
            continue;
        }

        output |= OUTSIDE_ROPE_BOUNDS;
        break;
    }

    //
    // Overlap check
    //

    // This prevents false overlap results given things are out of order
    spans.sort_by(|(min1, max1), (min2, max2)| min1.cmp(&min2).then_with(|| max1.cmp(&max2)));

    let mut previous = None;

    for (span_min, span_max) in spans.into_iter().rev() {
        // Note: we are going in order from starting furthest, to starting earliest
        if let Some((prev_min, _prev_max)) = previous {
            if span_max >= prev_min {
                output |= HAS_OVERLAPS;
            }
        }
        previous = Some((span_min, span_max));
    }

    output
}

// meta
#[test]
fn cursor_vec1_maintains_invariants_detects_invariant_violations() {
    // I just want some rope large enough for all the tests that precede the rope
    // requirement for creating cursors to be in bounds.
    let rope = r!(format!(
        "{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}{0}",
        "01234567890\n"
    ));

    assert_eq!(
        HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(
            &rope,
            &vec1![Cursor::new(pos! {l 1 o 2}), Cursor::new(pos! {l 1 o 2})]
        )
    );

    assert_eq!(
        OUT_OF_ORDER,
        cursor_vec1_maintains_invariants(
            &rope,
            &vec1![Cursor::new(pos! {l 1 o 2}), Cursor::new(pos! {l 9 o 0})]
        )
    );

    assert_eq!(
        OUT_OF_ORDER | HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(
            &rope,
            &vec1![
                Cursor::new_with_highlight(pos! {l 1 o 2}, pos! {l 9 o 1}),
                Cursor::new(pos! {l 9 o 0})
            ]
        )
    );

    assert_eq!(
        OUTSIDE_ROPE_BOUNDS,
        cursor_vec1_maintains_invariants(&rope, &vec1![Cursor::new(pos! {l 20 o 0})])
    );

    assert_eq!(
        OUTSIDE_ROPE_BOUNDS | HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(
            &rope,
            &vec1![
                Cursor::new(pos! {l 20 o 0}),
                Cursor::new(pos! {l 1 o 2}),
                Cursor::new(pos! {l 1 o 2})
            ]
        )
    );

    assert_eq!(
        OUTSIDE_ROPE_BOUNDS | OUT_OF_ORDER,
        cursor_vec1_maintains_invariants(
            &rope,
            &vec1![Cursor::new(pos! {l 1 o 2}), Cursor::new(pos! {l 20 o 0})]
        )
    );

    assert_eq!(
        OUTSIDE_ROPE_BOUNDS | OUT_OF_ORDER | HAS_OVERLAPS,
        cursor_vec1_maintains_invariants(
            &rope,
            &vec1![
                Cursor::new(pos! {l 20 o 0}),
                Cursor::new_with_highlight(pos! {l 1 o 2}, pos! {l 9 o 1}),
                Cursor::new(pos! {l 9 o 0})
            ]
        )
    );
}

fn cursors_maintains_invariants(rope: &Rope, cursors: &Cursors) -> u8 {
    cursor_vec1_maintains_invariants(rope, cursors.borrow_cursors())
}

macro_rules! flags_assert {
    ($flags: expr) => {
        let flags = $flags;
        assert_eq!(
            0,
            flags,
            "{} {} {}",
            if flags & OUTSIDE_ROPE_BOUNDS == OUTSIDE_ROPE_BOUNDS {
                "OUTSIDE_ROPE_BOUNDS"
            } else {
                ""
            },
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
    }
}

macro_rules! assert_cursor_invarints_maintained {
    ($buffer: expr) => {{
        let buffer = &$buffer;
        let (rope, cursors) = (buffer.borrow_rope(), buffer.borrow_cursors());

        assert_cursor_invarints_maintained!(rope, cursors)
    }};
    ($rope: expr, $cursors: expr) => {{
        flags_assert!(cursors_maintains_invariants(&$rope, &$cursors));
    }};
}

fn editing_this_buffer_preserves_the_cursors_invariants(
    mut buffer: TextBuffer,
    edits: Vec<TestEdit>,
) {
    assert_cursor_invarints_maintained!(buffer);

    for edit in edits {
        TestEdit::apply(&mut buffer, edit);
    }

    assert_cursor_invarints_maintained!(buffer);
}

proptest! {
    #[test]
    fn editing_the_buffer_preserves_the_cursors_invariants(
        buffer in arb::text_buffer_with_many_cursors(),
        edits in arb::test_edits(16, arb::TestEditSpec::All)
    ) {
        editing_this_buffer_preserves_the_cursors_invariants(buffer, edits);
    }
}

#[test]
fn editing_the_buffer_preserves_the_cursors_invariants_in_this_generated_case() {
    use arb::TestEdit::*;
    let mut buffer: TextBuffer = d!();

    assert_cursor_invarints_maintained!(buffer);

    TestEdit::apply(&mut buffer, Insert('\n'));
    assert_cursor_invarints_maintained!(buffer);

    TestEdit::apply(&mut buffer, MoveAllCursors(Move::ToBufferStart));
    assert_cursor_invarints_maintained!(buffer);

    TestEdit::apply(&mut buffer, Insert('\t'));
    assert_cursor_invarints_maintained!(buffer);

    TestEdit::apply(&mut buffer, TabOut);
    assert_cursor_invarints_maintained!(buffer);
}

proptest! {
    #[test]
    fn cursors_new_maintains_invariants(
        rope in arb::rope(),
        vec1_of_cursors in arb::vec1_of_cursors(SOME_AMOUNT)
    ) {
        assert_cursor_invarints_maintained!(rope, Cursors::new(&rope, vec1_of_cursors));
    }
}

#[test]
fn cursors_new_maintains_invariants_on_this_out_of_bounds_example() {
    let rope = r!("");
    assert_cursor_invarints_maintained!(
        rope,
        Cursors::new(
            &rope,
            vec1![Cursor::new_with_highlight(pos! {}, pos! {l 0 o 1})]
        )
    );
}

#[test]
fn cursors_new_maintains_invariants_on_this_newline_involving_out_of_bounds_example() {
    let rope = r!("\n");
    assert_cursor_invarints_maintained!(
        rope,
        Cursors::new(
            &rope,
            vec1![Cursor::new_with_highlight(pos! {l 0 o 1}, pos! {l 0 o 1})]
        )
    );
}

#[test]
fn cursors_new_maintains_invariants_on_this_new_out_of_order_example() {
    let rope = r!("a 0");
    assert_cursor_invarints_maintained!(
        rope,
        Cursors::new(
            &rope,
            vec1![
                Cursor::new_with_highlight(pos! {l 0 o 2}, pos! {}),
                Cursor::new_with_highlight(pos! {l 1 o 0}, pos! {})
            ]
        )
    );
}

#[test]
fn cursors_new_maintains_invariants_on_this_u2028_out_of_order_example() {
    let rope = r!("\u{2028}");
    assert_cursor_invarints_maintained!(
        rope,
        Cursors::new(
            &rope,
            vec1![
                cur!{l 0 o 5},
                cur!{l 1 o 1 h l 0 o 1},
            ]
        )
    );
}

#[test]
fn cursors_new_maintains_invariants_on_this_u2028_out_of_order_example_reduction() {
    let rope = r!("\u{2028}");
    let cursors = std::dbg!(Cursors::new(
        &rope,
        vec1![
            cur!{l 0 o 5},
            cur!{l 1 o 1 h l 0 o 1},
        ]
    ));

    let flags = {
        use std::cmp::Ordering;
        let spans = cursors.borrow_cursors()
            .mapped_ref(|c| {
                let p = c.get_position();
                let h = c.get_highlight_position_or_position();

                (std::cmp::min(p, h), std::cmp::max(p, h))
            })
            .into_vec();
        //
        // Ordering check
        //
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

        output
    };

    flags_assert!(flags);
}

#[test]
fn cursors_new_maintains_invariants_on_this_hat_trick_example() {
    // This was
    // OUTSIDE_ROPE_BOUNDS | OUT_OF_ORDER | HAS_OVERLAPS
    let rope = r!(" ꬑ�a A®®aAa�0");
    assert_cursor_invarints_maintained!(
        rope,
        Cursors::new(
            &rope,
            vec1![
                Cursor::new(pos! {l 0 o 5}),
                d!(),
                Cursor::new(pos! {l 1 o 0})
            ]
        )
    );
}

#[test]
fn cursors_new_merges_these_cursors() {
    let cursors = Cursors::new(&r!("hi"), vec1![d!(), d!(), d!()]);

    assert_eq!(cursors.len(), 1);
}

#[test]
fn cursors_new_merges_these_identical_cursors_correctly() {
    let cursors = Cursors::new(
        &r!("long\nenough"),
        vec1![
            Cursor::new(pos! {l 1 o 2}),
            Cursor::new(pos! {l 1 o 2}),
            d!()
        ],
    );

    assert_eq!(cursors.borrow_cursors(), &vec1![Cursor::new(pos! {l 1 o 2}), d!()]);
}

#[test]
fn adding_a_cursor_inside_a_highlight_does_not_change_the_selection() {
    let mut buffer = t_b!("12345678");

    macro_rules! selection_is_unchanged {
        (p right) => {
            let cs = buffer.borrow_cursors();

            assert_eq!(
                1,
                cs.len(),
                "A cursor didn't get merged: {:#?}",
                cs
            );
            let c = cs.first();

            assert_eq!(c.get_position(), pos! {l 0 o 7});
            assert_eq!(c.get_highlight_position_or_position(), pos! {l 0 o 1});
        };
        (p left) => {
            let cs = buffer.borrow_cursors();

            assert_eq!(
                1,
                cs.len(),
                "A cursor didn't get merged: {:#?}",
                cs
            );
            let c = cs.first();

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

#[test]
fn copy_selections_works_in_this_case_generated_elsewhere() {
    u!{ReplaceOrAdd, TestEdit}

    let mut buffer = t_b!("!\u{2000}");

    let mut cursor = cur!{l 0 o 1 h l 0 o 0};
    cursor.sticky_offset = CharOffset(0);

    buffer.set_cursor(cursor, Replace);

    assert_eq!(buffer.copy_selections(), vec!["!"]);
    TestEdit::apply(&mut buffer, TabOut);
    assert_eq!(buffer.copy_selections(), vec!["!"]);
    TestEdit::apply(&mut buffer, TabIn);
    assert_eq!(buffer.copy_selections(), vec!["!"]);
}

fn single_cursor(buffer: &TextBuffer) -> Cursor {
    let cursors = buffer.borrow_cursors();
    assert_eq!(cursors.len(), 1);

    cursors.first().clone()
}

// This test demonstrated that a bug was not (soley) in the text_buffer crate.
proptest!{
    #[test]
    fn inserting_after_a_find_between_two_other_chars_places_the_cursor_correctly(
        ch1 in non_line_break_char(),
        ch2 in non_line_break_char(),
        ch3 in non_line_break_char(),
        ch4 in non_line_break_char(),
    ) {
        // Arrange
        let mut buffer = t_b!("");

        buffer.insert(ch1, None);
        buffer.insert(ch2, None);
        buffer.insert(ch3, None);

        buffer.move_all_cursors(Move::Left);
        buffer.extend_selection_for_all_cursors(Move::Left);

        let cursor = single_cursor(&buffer);
        assert_eq!(cursor, cur!{l 0 o 1 h l 0 o 2});

        buffer.move_all_cursors(Move::Right);
        buffer.move_all_cursors(Move::Right);

        let cursor = single_cursor(&buffer);
        assert_eq!(cursor, cur!{l 0 o 3});

        // Act
        buffer.insert(ch4, None);

        // Assert
        let cursor = single_cursor(&buffer);
        assert_eq!(cursor, cur!{l 0 o 4});
    }
}

fn inserting_then_deleting_preserves_editedness_on(
    mut buffer: TextBuffer,
    ch: char,
) {
    u!{TestEdit}
    let old_editedness = buffer.editedness();

    TestEdit::apply(&mut buffer, Insert(ch));

    TestEdit::apply(&mut buffer, Delete);

    let new_editedness = buffer.editedness();

    assert_eq!(
        old_editedness,
        new_editedness
    );
}

fn all_selected(buffer: &TextBuffer) -> bool {
    if buffer.borrow_cursors().len() != 1 {
        return false;
    }

    if let Some(last_pos) = last_position(buffer.borrow_rope()) {
        buffer.borrow_cursors().first() == &cur!{pos! {}, last_pos}
    } else {
        // We expect this will only happen when there are no lines.
        // So all the characters would be selected, since there are none.
        true
    }
}

proptest!{
    #[test]
    fn inserting_then_deleting_preserves_editedness_unless_all_is_selected(
        mut buffer in arb::text_buffer_with_many_cursors(),
        ch in any_char(),
    ) {
        if all_selected(&buffer) {
            buffer.set_cursors(d!());
        }

        inserting_then_deleting_preserves_editedness_on(
            buffer,
            ch
        );
    }
}

#[test]
fn inserting_then_deleting_preserves_editedness_in_this_minimal_example() {
    inserting_then_deleting_preserves_editedness_on(
        d!(),
        'a'
    );
}

#[test]
fn inserting_then_deleting_preserves_editedness_on_this_found_example() {
    const EXPECTED_DEGUG_STR: &str = r#"TextBuffer { rope: CursoredRope { rope: ["\u{2028}"], cursors: Cursors { cursors: [cur!{l 0 o 0}] } }, history: History { edits: [], index: 0 }, unedited: ["\u{2028}"], scroll: slxy!(0, 0) }"#;

    let mut buffer: TextBuffer = d!();
    buffer.rope = c_r!("\u{2028}");
    buffer.set_unedited();
    if all_selected(&buffer) {
        buffer.set_cursors(d!());
    }

    assert_eq!(format!("{:?}", buffer), EXPECTED_DEGUG_STR);

    inserting_then_deleting_preserves_editedness_on(
        buffer,
        '\u{b}'
    );
}

#[test]
fn inserting_then_deleting_preserves_editedness_on_this_found_asciified_example() {
    let mut buffer: TextBuffer = d!();
    buffer.rope = c_r!("a");
    buffer.set_cursors(curs!(buffer.borrow_rope(), cur!{l 0 o 0 h l 1 o 0}));
    buffer.set_unedited();
    if all_selected(&buffer) {
        buffer.set_cursors(d!());
    }

    inserting_then_deleting_preserves_editedness_on(
        buffer,
        'b'
    );
}

#[test]
fn inserting_then_deleting_preserves_editedness_on_this_found_asciified_example_reduction() {
    u!{Editedness}
    u!{TestEdit}

    let mut buffer: TextBuffer = d!();
    buffer.rope = c_r!("a");
    buffer.set_cursors(curs!(buffer.borrow_rope(), cur!{l 0 o 0 h l 1 o 0}));
    buffer.set_unedited();
    if all_selected(&buffer) {
        buffer.set_cursors(d!());
    }

    let old_editedness = buffer.editedness();

    TestEdit::apply(&mut buffer, Insert('b'));

    assert_eq!(
        buffer.editedness(),
        Edited,
        "Was not Edited after insert"
    );

    TestEdit::apply(&mut buffer, Delete);

    let new_editedness = buffer.editedness();

    assert_eq!(
        old_editedness,
        new_editedness
    );
}

fn calling_set_unedited_acts_as_expected_after_a_second_insertion_on(
    mut buffer: TextBuffer,
    ch1: char,
    ch2: char,
) {
    u!{Editedness}

    buffer.insert(ch1, None);

    assert_eq!(buffer.editedness(), Edited, "precondition_failure");

    buffer.set_unedited();

    assert_eq!(
        buffer.editedness(),
        Unedited,
        "set_unedited did not set the buffer as unedited!",
    );

    buffer.insert(ch2, None);

    assert_eq!(
        buffer.editedness(),
        Edited,
        "the buffer was not reported as edited after an edit!",
    );
}

proptest!{
    #[test]
    fn calling_set_unedited_acts_as_expected_after_a_second_insertion(
        buffer in arb::text_buffer_with_many_cursors(),
        ch1 in any_char(),
        ch2 in any_char(),
    ) {
        calling_set_unedited_acts_as_expected_after_a_second_insertion_on(
            buffer,
            ch1,
            ch2
        );
    }
}

#[test]
fn calling_set_unedited_acts_as_expected_after_a_second_insertion_works_on_an_empty_buffer_with_these_chars() {
    calling_set_unedited_acts_as_expected_after_a_second_insertion_on(
        d!(),
        'a',
        'b'
    );
}

pub mod arb;
mod cursor_manipulation;
mod edit_tests;
mod inserting_then_deleting;
