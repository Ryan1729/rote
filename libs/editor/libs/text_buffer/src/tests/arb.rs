// This module is inside `tests`
use super::*;

// TODO move all `arb` fns in here

prop_compose! {
    pub fn rope()(s in any::<String>()) -> Rope {
        r!(s)
    }
}

prop_compose! {
    pub fn all_space_rope()(s in "[ \n]*") -> Rope {
        r!(s)
    }
}

prop_compose! {
    pub fn non_0_to_9_char_rope()(s in "[^0-9]*") -> Rope {
        r!(s)
    }
}

pub fn vec1<D: Debug>(
    strat: impl Strategy<Value = D>,
    max_len: usize,
) -> impl Strategy<Value = Vec1<D>> {
    collection::vec(strat, 1..std::cmp::max(2, max_len))
        .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
}

prop_compose! {
    pub fn cursor(LineIndex(max_line): LineIndex, CharOffset(max_offset): CharOffset)(
        position in arb_pos(max_line, max_offset),
        highlight_position in arb_pos(max_line, max_offset),
        sticky_offset in arb_char_offset(max_offset),
        state in arb_cursor_state()
    ) -> Cursor {
        let mut c = Cursor::new(position);
        c.set_highlight_position(highlight_position);
        c.sticky_offset = sticky_offset;
        c.state = state;
        c
    }
}

pub fn vec1_of_cursors(max_len: usize) -> impl Strategy<Value = Vec1<Cursor>> {
    vec1(
        // passing `max_len` in for both of these ensures that we get all possibly valid cursors
        // but itmeans we will get many invalid ones.
        cursor(LineIndex(max_len), CharOffset(max_len)),
        // It doesn't semm particularly useful to have more cursors than text positions.
        max_len,
    )
}

pub fn cursors(max_len: usize) -> impl Strategy<Value = Cursors> {
    vec1_of_cursors(max_len).prop_map(Cursors::new)
}

pub fn many_cursors(max_len: usize) -> impl Strategy<Value = Cursors> {
    collection::vec(
        cursor(LineIndex(max_len), CharOffset(max_len)),
        std::cmp::max(max_len / 2, 1)..std::cmp::min(std::cmp::max(2, max_len), max_len),
    )
    .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
    .prop_map(Cursors::new)
}

pub fn valid_cursors_for_rope(rope: &Rope, max_len: usize) -> impl Strategy<Value = Cursors> {
    let (max_line, max_offset) = rope
        .lines()
        .enumerate()
        .fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
            (i, std::cmp::min(line.len_chars(), acc_offset))
        });

    vec1(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::min(max_len, std::cmp::max(max_line * max_offset.0, 1)),
    )
    .prop_map(Cursors::new)
}

pub fn many_valid_cursors_for_rope(rope: &Rope, max_len: usize) -> impl Strategy<Value = Cursors> {
    let (max_line, max_offset) = rope
        .lines()
        .enumerate()
        .fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
            (i, std::cmp::min(line.len_chars(), acc_offset))
        });
    let len = max_line + 1;

    collection::vec(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::max(len / 2, 1)..std::cmp::min(std::cmp::max(2, len), max_len),
    )
    .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
    .prop_map(Cursors::new)
}

prop_compose! {
    pub fn text_buffer_with_many_cursors()
    (rope in rope())
    (cursors in many_cursors(MORE_THAN_SOME_AMOUNT), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn all_space_text_buffer_with_many_cursors()
    (rope in all_space_rope())
    (cursors in many_cursors(MORE_THAN_SOME_AMOUNT), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn no_history_text_buffer()
    (rope in rope())
    (cursors in cursors(rope.chars().count()), r in Just(rope)) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn text_buffer_with_valid_cursors()
    (rope in rope())
    (
        cursors in {
            let (max_line, max_offset) = rope.lines().enumerate().fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
                (i, std::cmp::min(line.len_chars(), acc_offset))
            });

            vec1(
                cursor(LineIndex(max_line), max_offset),
                std::cmp::max(max_line * max_offset.0, 1)
            ).prop_map(Cursors::new)
        },
        r in Just(rope)
    ) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn text_buffer_with_valid_cursors_and_no_0_to_9_chars(max_len: usize)
    (rope in non_0_to_9_char_rope())
    (
        cursors in valid_cursors_for_rope(&rope, max_len),
        r in Just(rope)
    ) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

prop_compose! {
    pub fn text_buffer_with_many_valid_cursors_and_no_0_to_9_chars(max_len: usize)
    (rope in non_0_to_9_char_rope())
    (
        cursors in many_valid_cursors_for_rope(&rope, max_len),
        r in Just(rope)
    ) -> TextBuffer {
        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;
        text_buffer.cursors = cursors;
        text_buffer
    }
}

#[derive(Debug, Clone)]
pub enum TestEdit {
    Insert(char),
    InsertString(String),
    Delete,
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    MoveCursors(usize, Move),
    ExtendSelection(usize, Move),
    SetCursor(Position, ReplaceOrAdd),
    DragCursors(Position),
    SelectCharTypeGrouping(Position, ReplaceOrAdd),
    Cut,
    InsertNumbersAtCursors,
    TabIn,
    TabOut,
}

impl TestEdit {
    pub fn apply(buffer: &mut TextBuffer, edit: TestEdit) {
        use TestEdit::*;
        match edit {
            Insert(c) => buffer.insert(c),
            InsertString(s) => buffer.insert_string(s),
            Delete => buffer.delete(),
            MoveAllCursors(r#move) => buffer.move_all_cursors(r#move),
            ExtendSelectionForAllCursors(r#move) => buffer.extend_selection_for_all_cursors(r#move),
            MoveCursors(index, r#move) => buffer.move_cursor(index, r#move),
            ExtendSelection(index, r#move) => buffer.extend_selection(index, r#move),
            SetCursor(position, replace_or_add) => buffer.set_cursor(position, replace_or_add),
            DragCursors(position) => buffer.drag_cursors(position),
            SelectCharTypeGrouping(position, replace_or_add) => {
                buffer.select_char_type_grouping(position, replace_or_add)
            }
            Cut => {
                buffer.cut_selections();
            }
            InsertNumbersAtCursors => buffer.insert_at_each_cursor(|i| i.to_string()),
            TabIn => buffer.tab_in(),
            TabOut => buffer.tab_out(),
        }
    }
}

pub fn test_edit() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        Just(Delete),
        any::<char>().prop_map(Insert),
        ".*".prop_map(InsertString),
        arb_move().prop_map(MoveAllCursors),
        arb_move().prop_map(ExtendSelectionForAllCursors),
        // Moving a cursor that isn't there should just be a no-op
        (0..MORE_THAN_SOME_AMOUNT, arb_move()).prop_map(|(i, m)| MoveCursors(i, m)),
        (0..MORE_THAN_SOME_AMOUNT, arb_move()).prop_map(|(i, m)| ExtendSelection(i, m)),
        // The user can attempt to move the cursor to invalid positions,
        // and their cursor may get snapped to a valid position producing an actual movement.
        (
            arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT),
            replace_or_add()
        )
            .prop_map(|(p, r)| SetCursor(p, r)),
        arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT).prop_map(DragCursors),
        (
            arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT),
            replace_or_add()
        )
            .prop_map(|(p, r)| SelectCharTypeGrouping(p, r)),
        Just(Cut),
        Just(InsertNumbersAtCursors),
        Just(TabIn),
        Just(TabOut),
    ]
}

pub fn replace_or_add() -> impl Strategy<Value = ReplaceOrAdd> {
    prop_oneof![Just(ReplaceOrAdd::Replace), Just(ReplaceOrAdd::Add),]
}

pub fn test_edit_insert() -> impl Strategy<Value = TestEdit> {
    any::<char>().prop_map(TestEdit::Insert)
}

type Regex = &'static str;

pub fn test_edit_regex_insert(regex: Regex) -> impl Strategy<Value = TestEdit> {
    regex.prop_map(|s| TestEdit::Insert(s.chars().next().unwrap_or('a')))
}

pub fn test_edit_set_cursor_heavy() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        9 => arb_pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT).prop_map(|p| SetCursor(p, ReplaceOrAdd::Add)),
        1 => test_edit()
    ]
}

pub enum TestEditSpec {
    All,
    Insert,
    RegexInsert(Regex),
    SetCursorHeavy,
}

pub fn test_edits(max_len: usize, spec: TestEditSpec) -> impl Strategy<Value = Vec<TestEdit>> {
    collection::vec(
        match spec {
            TestEditSpec::All => test_edit().boxed(),
            TestEditSpec::Insert => test_edit_insert().boxed(),
            TestEditSpec::RegexInsert(regex) => test_edit_regex_insert(regex).boxed(),
            TestEditSpec::SetCursorHeavy => test_edit_set_cursor_heavy().boxed(),
        },
        0..max_len,
    )
}

prop_compose! {
    pub fn test_edits_and_index(max_len: usize, spec: TestEditSpec)
        (edits in test_edits(max_len, spec))
        (
            i in if edits.len() == 0 { 0..1 } else { 0..edits.len() },
            es in Just(edits)
         ) -> (Vec<TestEdit>, usize) {
        (es, i)
    }
}
