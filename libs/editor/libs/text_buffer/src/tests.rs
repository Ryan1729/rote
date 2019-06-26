use super::{cursor_assert, r, t_b, *};
use platform_types::pos;
use proptest::prelude::*;
use proptest::{prop_compose, proptest};

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

    buffer.insert(dbg!('\n'));

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

#[test]
fn forward_works_across_line_feeds() {
    let rope = r!("123\n567");

    assert_eq!(forward(&rope, pos! {l 0 o 3}), Some(pos! {l 1 o 0}));
}
#[test]
fn forward_works_across_carriage_return_line_feeds() {
    let rope = r!("123\r\n567");

    assert_eq!(forward(&rope, pos! {l 0 o 3}), Some(pos! {l 1 o 0}));
}

#[test]
fn backward_works_across_line_feeds() {
    let rope = r!("123\n567");

    assert_eq!(backward(&rope, pos! {l 1 o 0}), Some(pos! {l 0 o 3}));
}
#[test]
fn backward_works_across_carriage_return_line_feeds() {
    let rope = r!("123\r\n567");

    assert_eq!(backward(&rope, pos! {l 1 o 0}), Some(pos! {l 0 o 3}));
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
        dbg!(1);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: None
        }

        buffer.extend_selection(0, Move::ToBufferEnd);

        dbg!(2);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: pos! {l 0 o 0}
        }

        buffer.move_cursor(0, Move::ToBufferEnd);
        dbg!(3);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 3},
            h: None,
            s: d!()
        }

        buffer.extend_selection(0, Move::ToBufferStart);

        dbg!(4);
        cursor_assert! {
            buffer,
            p: pos! {l 0 o 0},
            h: pos! {l 1 o 3}
        }

        buffer.move_cursor(0, Move::ToLineEnd);

        dbg!(5);
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

        dbg!(5.5);
        buffer.move_cursor(0, Move::Down);

        dbg!(6);
        cursor_assert! {
            buffer,
            p: pos! {l 1 o 0},
            h: None,
            s: d!()
        }

        buffer.extend_selection(0, Move::Left);
        dbg!(7);
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
    ]
}

#[derive(Debug, Clone, Copy)]
enum Edit {
    Insert(char),
    Delete,
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
}

fn apply_edit(buffer: &mut TextBuffer, edit: Edit) {
    match edit {
        Edit::Insert(c) => buffer.insert(c),
        Edit::Delete => buffer.delete(),
        Edit::MoveAllCursors(r#move) => buffer.move_all_cursors(r#move),
        Edit::ExtendSelectionForAllCursors(r#move) => {
            buffer.extend_selection_for_all_cursors(r#move)
        }
    }
}

fn arb_edit() -> impl Strategy<Value = Edit> {
    prop_oneof![
        Just(Edit::Delete),
        any::<char>().prop_map(Edit::Insert),
        arb_move().prop_map(Edit::MoveAllCursors),
        arb_move().prop_map(Edit::ExtendSelectionForAllCursors)
    ]
}

prop_compose! {
    fn arb_edits_and_index(max_len: usize)
        (index in 0..max_len)
        (
            edits in proptest::collection::vec(arb_edit(), 0..max_len),
             i in Just(index)
         ) -> (Vec<Edit>, usize) {
        (edits, i)
    }
}

// `Rope`s share backing buffers when cloned, so we want to avoid that.
fn deep_clone(buffer: &TextBuffer) -> TextBuffer {
    let s: std::borrow::Cow<str> = (&buffer.rope).into();
    TextBuffer {
        rope: Rope::from_str(&s),
        ..buffer.clone()
    }
}

proptest! {
    #[test]
    fn undo_redo_works((edits, index) in arb_edits_and_index(16)) {
        //TODO generate initial buffer?
        let initial_buffer: TextBuffer = d!();
        let mut buffer: TextBuffer = deep_clone(&initial_buffer);

        let mut expected_buffer_at_index: Option<TextBuffer> = None;
        for (i, edit) in edits.iter().enumerate() {
            apply_edit(&mut buffer, *edit);

            if i == index {
                expected_buffer_at_index = Some(deep_clone(&buffer));
            }
        }

        let final_buffer = deep_clone(&buffer);
        let expected_buffer_at_index = expected_buffer_at_index.unwrap_or_default();

        let len = edits.len();

        for _ in 0..dbg!(dbg!(len) - index) {
            buffer.undo();
        }

        assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer_at_index);

        for _ in 0..len {
            buffer.redo();
        }

        assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

        // Redo with no redos left should be a no-op
        for _ in 0..3 {
            buffer.redo();
        }

        assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

        for _ in 0..len {
            buffer.undo();
        }

        assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

        // undo with no redos left should be a no-op
        for _ in 0..3 {
            buffer.undo();
        }

        assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
    }
}

#[test]
fn undo_undoes() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    buffer.insert('a');
    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn redo_redoes() {
    let mut buffer: TextBuffer = d!();
    buffer.insert('a');

    let final_buffer: TextBuffer = deep_clone(&buffer);

    buffer.undo();

    buffer.redo();

    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);
}
