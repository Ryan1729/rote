use super::*;
use platform_types::pos;
use proptest::prelude::*;
use proptest::{prop_compose, proptest};

macro_rules! r {
    ($s:expr) => {
        Rope::from_str(&$s)
    };
}

prop_compose! {
    fn arb_rope()(s in any::<String>()) -> Rope {
        r!(s)
    }
}

prop_compose! {
    fn arb_absolute_char_offset(max_len: usize)(offset in 0..max_len) -> AbsoluteCharOffset {
        AbsoluteCharOffset(offset)
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

#[test]
fn offset_at_end_of_line_works() {
    let rope = r!("\u{b}");

    assert_eq!(
        pos_to_char_offset(&rope, &pos! {l 1 o 0}),
        Some(AbsoluteCharOffset(1))
    );

    assert_eq!(
        char_offset_to_pos(&rope, &AbsoluteCharOffset(1)),
        Some(pos! {l 1 o 0})
    )
}

#[test]
fn offset_in_middle_of_single_line_with_non_ascii_works() {
    let rope = r!("0¡¡");

    assert_eq!(
        char_offset_to_pos(&rope, &AbsoluteCharOffset(2)),
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
        char_offset_to_pos(&rope, &AbsoluteCharOffset(1)),
        Some(pos! {l 0 o 1})
    )
}

fn pos_to_to_char_offset_to_pos(rope: &Rope, p: Position) {
    if let Some(o) = pos_to_char_offset(&rope, &p) {
        assert_eq!(char_offset_to_pos(&rope, &dbg!(o)), Some(p))
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
        char_offset_to_pos(&rope, &AbsoluteCharOffset(1)),
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
        if let Some(p) = char_offset_to_pos(&rope, &offset) {
            assert_eq!(pos_to_char_offset(&rope, &p), Some(offset))
        }
    }

    #[test]
    fn pos_to_to_char_offset_to_pos_works((rope, pos) in arb_rope_and_pos()) {
        if let Some(o) = pos_to_char_offset(&rope, &pos) {
            assert_eq!(char_offset_to_pos(&rope, &o), Some(pos))
        }
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
    {
        let c = buffer.cursors.first();
        assert_eq!(c.highlight_position, None);
        assert_eq!(c.position, pos! {l 0 o 4});
    }

    buffer.move_cursor(0, Move::Left);
    buffer.move_cursor(0, Move::Left);

    {
        let c = buffer.cursors.first();
        assert_eq!(c.highlight_position, None);
        assert_eq!(c.position, pos! {l 0 o 2});
    }

    buffer.extend_selection(0, Move::Right);

    {
        let c = buffer.cursors.first();
        assert_eq!(c.highlight_position, Some(pos! {l 0 o 2}));
        assert_eq!(c.position, pos! {l 0 o 3});
    }

    // Act
    buffer.insert('3');

    // Assert
    let s: String = buffer.rope.into();
    assert_eq!(s, "1234");

    let c = buffer.cursors.first();
    assert_eq!(c.highlight_position, None);
    assert_eq!(c.position, pos! {l 0 o 3});
}
