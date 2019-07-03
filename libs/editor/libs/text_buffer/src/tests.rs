#![allow(dead_code)]
use super::{cursor_assert, r, t_b, *};
use platform_types::pos;
use proptest::prelude::*;
use proptest::{prop_compose, proptest, option, collection};
use std::fmt::Debug;

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

prop_compose! {
    fn arb_char_edit()(c in option::of(any::<char>()), offsets in arb_offset_pair()) -> CharEdit {
        CharEdit {
            c,
            offsets,
        }
    }
}

fn vec1<D: Debug>(strat: impl Strategy<Value = D>, max_len: usize) -> impl Strategy<Value = Vec1<D>> {
    collection::vec(strat, 1..std::cmp::max(2, max_len))
        .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
}

const ARB_OFFSET_PAIR_SIZE: usize = 16;
prop_compose! {
    fn arb_offset_pair()(
        o1 in option::of(arb_absolute_char_offset(ARB_OFFSET_PAIR_SIZE)),
        o2 in option::of(arb_absolute_char_offset(ARB_OFFSET_PAIR_SIZE))
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

prop_compose! {
    fn arb_no_history_text_buffer()
    (rope in arb_rope())
    (cursors in arb_cursors(rope.chars().count()), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

fn arb_edit() -> impl Strategy<Value = Edit> {
    const LEN: usize = 16;
    prop_oneof![
        vec1(arb_char_edit(), LEN).prop_map(Edit::Insert),
        vec1(arb_char_edit(), LEN).prop_map(Edit::Delete),
        arb_change!(arb_cursors(LEN)).prop_map(Edit::Move),
        arb_change!(arb_cursors(LEN)).prop_map(Edit::Select),
    ]
}

fn arb_edit_from_buffer(text_buffer: TextBuffer) -> impl Strategy<Value = Edit> {
    let cs = text_buffer.cursors.clone();
    arb_edit()
    //Okay this amount of cloning and `move` annotation seems excessive.
    .prop_map(move |mut edit| {
        match edit {
            Edit::Move(ref mut change)| Edit::Select(ref mut change) => {
                change.old = cs.clone();
            }
            _ => {}
        }
        edit
    })
}

prop_compose! {
    fn arb_no_history_text_buffer_and_edit()
    (text_buffer in arb_no_history_text_buffer())
    (edit in arb_edit_from_buffer(deep_clone(&text_buffer)), t_b in Just(text_buffer)) -> (TextBuffer, Edit) {
        (t_b, edit)
    }
}

// After some thought I am unable to establish a relationship between this property holding and
// the property we actually care about, undo/redo working. It seemed intuitive that either this
// property would imply undo/redo works or vice versa. But the closest I have come to
// demonstrating a link requires assumiong that there is only one edit that produces a given rope
// to rope transition, which is clearly false, (sometimes moving the cursor one spec doen the same
// thing as Home/End.) So, at this time it does not seem worth it to try to make this property
// hold. But it feels like it might make sense to do this later, and it also feels like without
// a reminder of this happneing before, it moght happen again so I will leave this commented out.
/*
proptest! {
    #[test]
    fn edits_double_negate_properly(edit in arb_edit()) {
        let initial = edit.clone();

        assert_eq!(!!edit, initial);
    }
}
*/

fn negated_edit_undo_redos_properly(initial_buffer: TextBuffer, edit: Edit) {
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    buffer.apply_edit(edit.clone(), ApplyKind::Record);

    let modified_buffer = deep_clone(&buffer);

    buffer.apply_edit(!(edit.clone()), ApplyKind::Playback);

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    buffer.apply_edit(edit, ApplyKind::Playback);

    assert_text_buffer_eq_ignoring_history!(buffer, modified_buffer);
}

// I am more confidnent that this weaker theorem follows directly from undo/redo working. It is
// essentially the statement that undo/redo works for a single action.
// However,it is complicated to generate valid edits for this, whereas the method used in
// `undo_redo_works_on_these_edits_and_index` (seemingly?) generates valid edits every time.
// So let's skip these for now.
proptest! {
    //#[test]
    fn negated_edits_undo_redo_properly(
        (initial_buffer, edit) in arb_no_history_text_buffer_and_edit()
    ) {
        negated_edit_undo_redos_properly(initial_buffer, edit)
    }
}

//#[test]
fn negated_edits_undo_redo_this_delete_edit() {
    negated_edit_undo_redos_properly(
        d!(),
        Edit::Delete(
            Vec1::new(CharEdit { c: Some('0'), offsets: (Some(AbsoluteCharOffset(0)), None) })
        )
    )
}


#[test]
fn negated_edits_undo_redo_this_edit_that_only_changes_the_sticky_offset() {
    let new_cursor = {
        let mut c: Cursor = d!();
        c.sticky_offset = CharOffset(1);
        c
    };

    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let edit = Edit::Move(Change {
        // If the first old change does not correspond to the initial buffer, then undoing to that
        // state can fail to match the initila buffer.
        old: buffer.cursors.clone(),
        new: Vec1::new(new_cursor.clone())
    });

    buffer.apply_edit(edit.clone(), ApplyKind::Record);

    let modified_buffer = deep_clone(&buffer);

    assert_eq!(modified_buffer.cursors.first(), &new_cursor);

    let undo_edit = !(edit.clone());

    match (&undo_edit, &edit) {
        (Edit::Move(u), Edit::Move(e)) => {
            assert_eq!(u.old, e.new);
            assert_eq!(u.new, e.old);
        }
        _ => {
            assert!(false);
        }
    }

    buffer.apply_edit(undo_edit, ApplyKind::Playback);

    assert_eq!(buffer.cursors.first(), initial_buffer.cursors.first());

    buffer.apply_edit(edit, ApplyKind::Playback);

    assert_eq!(buffer.cursors.first(), modified_buffer.cursors.first());
}


#[derive(Debug, Clone, Copy)]
enum TestEdit {
    Insert(char),
    Delete,
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
}

fn apply_edit(buffer: &mut TextBuffer, edit: TestEdit) {
    match edit {
        TestEdit::Insert(c) => buffer.insert(c),
        TestEdit::Delete => buffer.delete(),
        TestEdit::MoveAllCursors(r#move) => buffer.move_all_cursors(r#move),
        TestEdit::ExtendSelectionForAllCursors(r#move) => {
            buffer.extend_selection_for_all_cursors(r#move)
        }
    }
}

fn arb_test_edit() -> impl Strategy<Value = TestEdit> {
    prop_oneof![
        Just(TestEdit::Delete),
        any::<char>().prop_map(TestEdit::Insert),
        arb_move().prop_map(TestEdit::MoveAllCursors),
        arb_move().prop_map(TestEdit::ExtendSelectionForAllCursors)
    ]
}

fn arb_test_edit_insert() -> impl Strategy<Value = TestEdit> {
    any::<char>().prop_map(TestEdit::Insert)
}

type Regex = &'static str;

fn arb_test_edit_regex_insert(regex: Regex) -> impl Strategy<Value = TestEdit> {
    regex.prop_map(|s| TestEdit::Insert(s.chars().next().unwrap_or('a')))
}

enum ArbTestEditSpec {
    All,
    Insert,
    RegexInsert(Regex),
}

prop_compose! {
    fn arb_test_edits_and_index(max_len: usize, spec: ArbTestEditSpec)
        (edits in collection::vec(
            match spec {
                ArbTestEditSpec::All => arb_test_edit().boxed(),
                ArbTestEditSpec::Insert => arb_test_edit_insert().boxed(),
                ArbTestEditSpec::RegexInsert(regex) => arb_test_edit_regex_insert(regex).boxed(),
            },
            0..max_len
        ))
        (
            i in if edits.len() == 0 { 0..1 } else { 0..edits.len() },
            es in Just(edits)
         ) -> (Vec<TestEdit>, usize) {
        (es, i)
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

fn undo_redo_works_on_these_edits_and_index(edits: Vec<TestEdit>, index: usize) {
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

    if len != 0 {
        for _ in 0..dbg!(dbg!(len - 1) - index) {
            dbg!();
            buffer.undo();
        }
    }
    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, expected_buffer_at_index);

    for _ in 0..len {
        buffer.redo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    // Redo with no redos left should be a no-op
    for _ in 0..3 {
        dbg!();
        buffer.redo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, final_buffer);

    for _ in 0..len {
        dbg!();
        dbg!(&mut buffer).undo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);

    // undo with no undos left should be a no-op
    for _ in 0..3 {
        dbg!();
        buffer.undo();
    }

    dbg!();
    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

proptest! {
    #[test]
    fn undo_redo_works((edits, index) in arb_test_edits_and_index(16, ArbTestEditSpec::All)) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }

    #[test]
    fn undo_redo_works_on_inserts((edits, index) in arb_test_edits_and_index(16, ArbTestEditSpec::Insert)) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }

    #[test]
    fn undo_redo_works_on_non_control_inserts((edits, index) in arb_test_edits_and_index(16, ArbTestEditSpec::RegexInsert("\\PC"))) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }

    #[test]
    fn undo_redo_works_on_non_cr_inserts((edits, index) in arb_test_edits_and_index(16, ArbTestEditSpec::RegexInsert("[^\r]"))) {
        undo_redo_works_on_these_edits_and_index(edits, index);
    }
}

#[test]
fn undo_redo_works_on_this_set_of_edits() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('\u{b}'),
            TestEdit::Insert('a'),
            TestEdit::Insert('\n'),
        ],
        0,
    );
}

#[test]
fn undo_redo_works_in_this_reduced_scenario() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, TestEdit::Insert('\u{b}'));

    let expected_final_buffer = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('a'));

    let expected_mid_buffer = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('\n'));

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, expected_mid_buffer);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, expected_final_buffer);
}

#[test]
fn undo_redo_works_on_this_single_movement_edit() {
    undo_redo_works_on_these_edits_and_index(vec![TestEdit::MoveAllCursors(Move::Up)], 0);
}

#[test]
fn undo_redo_works_on_a_wall_hitting_movement_after_an_insert() {
    undo_redo_works_on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::MoveAllCursors(Move::Up)],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_manually_invented_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('a'),
            TestEdit::MoveAllCursors(Move::Left),
            TestEdit::Insert('b'),
            TestEdit::MoveAllCursors(Move::Right),
        ],
        2,
    );
}

#[test]
fn undo_redo_works_on_this_previously_panicking_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('a'),
            TestEdit::ExtendSelectionForAllCursors(Move::Left),
            TestEdit::Delete,
        ],
        2,
    );
}

#[test]
fn undo_redo_works_on_this_cr_lf_case() { // sigh
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('\r'), TestEdit::Insert('\n')
        ],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_line_end_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::MoveAllCursors(Move::ToLineEnd)],
        0,
    );
}

#[test]
fn undo_redo_works_in_this_familiar_scenario() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    apply_edit(&mut buffer, TestEdit::Insert('1'));

    let buffer_with_1 = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('2'));

    let buffer_with_2 = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Insert('3'));

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_2);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_1);

    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn undo_redo_works_on_this_simple_insert_delete_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![TestEdit::Insert('a'), TestEdit::Delete],
        0,
    );
}

#[test]
fn undo_redo_works_on_this_reduced_simple_insert_delete_case() {
    let initial_buffer: TextBuffer = d!();
    let mut buffer: TextBuffer = deep_clone(&initial_buffer);

    let inserted_char = 'a';

    apply_edit(&mut buffer, TestEdit::Insert(inserted_char));

    let buffer_with_a = deep_clone(&buffer);

    apply_edit(&mut buffer, TestEdit::Delete);

    dbg!();
    let delete_edit = buffer.history.get(buffer.history_index.checked_sub(1).unwrap()).unwrap();
    match delete_edit {
        Edit::Delete(char_edits) => assert_eq!(char_edits.first().c, Some(inserted_char)),
        _ => assert!(false),
    }

    dbg!();
    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, buffer_with_a);
    dbg!();
    buffer.undo();

    assert_text_buffer_eq_ignoring_history!(buffer, initial_buffer);
}

#[test]
fn undo_redo_works_on_this_move_to_line_start_case() {
    undo_redo_works_on_these_edits_and_index(
        vec![
            TestEdit::Insert('¡'),
            TestEdit::ExtendSelectionForAllCursors(Move::ToLineStart),
            TestEdit::Delete
        ],
        0,
    );
}
