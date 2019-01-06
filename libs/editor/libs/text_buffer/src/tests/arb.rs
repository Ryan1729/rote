// This module is inside `tests`
use super::*;

use std::collections::HashMap;

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

prop_compose! {
    pub fn needle_and_haystack()
    (needle in "\\PC+", needleless_haystack in "\\PC*")
    (insert_point in 0..=needleless_haystack.chars().count(), needle in Just(needle), needleless_haystack in Just(needleless_haystack)) -> (String, String) {
        let mut haystack = String::with_capacity(needleless_haystack.len() + needle.len());

        let mut inserted_count = 0;
        for (i, c) in needleless_haystack.chars().enumerate() {
            if i == insert_point {
                inserted_count += 1;
                haystack.push_str(&needle);
            }
            haystack.push(c);
        }
        if needleless_haystack.chars().count() == insert_point {
            inserted_count += 1;
            haystack.push_str(&needle);
        }
        self::assert_eq!(inserted_count, 1,
            "insert_point {}, needle {:?}, needleless_haystack: {:?}, needleless_haystack.chars().count() {}",
            insert_point,
            needle,
            needleless_haystack,
            needleless_haystack.chars().count()
        );

        (needle, haystack)
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

prop_compose! {
    pub fn non_highlight_cursor(LineIndex(max_line): LineIndex, CharOffset(max_offset): CharOffset)(
        position in arb_pos(max_line, max_offset),
        sticky_offset in arb_char_offset(max_offset),
        state in arb_cursor_state()
    ) -> Cursor {
        let mut c = Cursor::new(position);
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

fn get_max_line_and_max_offset(rope: &Rope) -> (usize, CharOffset) {
    rope.lines()
        .enumerate()
        .fold((0, CharOffset(0)), |(_, acc_offset), (i, line)| {
            (i, std::cmp::min(line.len_chars(), acc_offset))
        })
}

pub fn valid_cursors_for_rope<'rope, R: 'rope + Borrow<Rope>>(
    rope: R,
    max_len: usize,
) -> impl Strategy<Value = Cursors> + 'rope {
    let rope = rope.borrow();
    let (max_line, max_offset) = get_max_line_and_max_offset(rope);

    let rope2 = rope.clone();

    vec1(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::min(max_len, std::cmp::max(max_line * max_offset.0, 1)),
    )
    .prop_map(move |c| Cursors::new(&rope2, c))
}

pub fn many_valid_cursors_for_rope<'rope, R: 'rope + Borrow<Rope>>(
    rope: R,
    max_len: usize,
) -> impl Strategy<Value = Cursors> + 'rope {
    let rope = rope.borrow();

    let (max_line, max_offset) = get_max_line_and_max_offset(rope);
    let len = max_line + 1;

    let rope2 = rope.clone();

    collection::vec(
        cursor(LineIndex(max_line), max_offset),
        std::cmp::max(len / 2, 1)..std::cmp::min(std::cmp::max(2, len), max_len),
    )
    .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
    .prop_map(move |c| Cursors::new(&rope2, c))
}

pub fn many_valid_non_highlight_cursors_for_rope<'rope, R: 'rope + Borrow<Rope>>(
    rope: R,
    max_len: usize,
) -> impl Strategy<Value = Cursors> + 'rope {
    let rope = rope.borrow();

    let (max_line, max_offset) = get_max_line_and_max_offset(rope);
    let len = max_line + 1;

    let rope2 = rope.clone();

    collection::vec(
        non_highlight_cursor(LineIndex(max_line), max_offset),
        std::cmp::max(len / 2, 1)..std::cmp::min(std::cmp::max(2, len), max_len),
    )
    .prop_map(|v| Vec1::try_from_vec(v).expect("we said at least one!"))
    .prop_map(move |c| Cursors::new(&rope2, c))
}

fn all_but_end_cursors_for_rope(rope: &Rope) -> Vec<Cursor> {
    let len = rope.len_chars().0;
    let mut output = Vec::with_capacity(len);
    if len > 1 {
        for i in 1..(len - 1) {
            let offset = AbsoluteCharOffset(i);
            if let Some(position) = char_offset_to_pos(rope, offset) {
                output.push(Cursor::new(position));
            } else {
                break;
            }
        }
    }

    output
}

pub fn text_buffer_with_many_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rop| {
        (
            many_valid_cursors_for_rope(rop.clone(), MORE_THAN_SOME_AMOUNT),
            Just(rop),
        )
            .prop_map(|(cursors, r)| {
                let mut text_buffer: TextBuffer = d!();
                text_buffer.rope = r;
                text_buffer.set_cursors(cursors);
                text_buffer
            })
    })
}

pub fn text_buffer_with_many_non_highlight_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rop| {
        (
            many_valid_non_highlight_cursors_for_rope(rop.clone(), MORE_THAN_SOME_AMOUNT),
            Just(rop),
        )
            .prop_map(|(cursors, r)| {
                let mut text_buffer: TextBuffer = d!();
                text_buffer.rope = r;
                text_buffer.set_cursors(cursors);
                text_buffer
            })
    })
}

pub fn text_buffer_with_all_but_end_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_map(|r| {
        let cursors = all_but_end_cursors_for_rope(&r);

        let mut text_buffer: TextBuffer = d!();
        text_buffer.rope = r;

        if let Ok(cs) = Vec1::try_from_vec(cursors) {
            text_buffer.set_cursors(Cursors::new(&text_buffer.rope, cs));
        }
        text_buffer
    })
}

pub fn all_space_text_buffer_with_many_cursors() -> impl Strategy<Value = TextBuffer> {
    all_space_rope().prop_flat_map(|rop| {
        (
            many_valid_cursors_for_rope(rop.clone(), MORE_THAN_SOME_AMOUNT),
            Just(rop),
        )
            .prop_map(|(cursors, r)| {
                let mut text_buffer: TextBuffer = d!();
                text_buffer.rope = r.clone();
                text_buffer.set_cursors(cursors);
                text_buffer
            })
    })
}

pub fn no_history_text_buffer() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rop| {
        (
            many_valid_cursors_for_rope(rop.clone(), rop.chars().count()),
            Just(rop),
        )
            .prop_map(|(cursors, r)| {
                let mut text_buffer: TextBuffer = d!();
                text_buffer.rope = r;
                text_buffer.set_cursors(cursors);
                text_buffer
            })
    })
}

pub fn text_buffer_with_valid_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rope| {
        (
            {
                let rope = rope.clone();
                let (max_line, max_offset) = if rope.len_lines() > 0 {
                    rope
                    .lines()
                    .enumerate()
                    .fold((0, CharOffset(0xFFFF_FFFF)), |(_, acc_offset), (i, line)| {
                        (i, std::cmp::min(line.len_chars(), acc_offset))
                    })
                } else {
                    (0, CharOffset(0))
                };

                vec1(
                    cursor(LineIndex(max_line), max_offset),
                    std::cmp::max(max_line * max_offset.0, 1),
                )
                .prop_map(move |c| Cursors::new(&rope, c))
            },
            Just(rope),
        )
            .prop_map(|(cursors, r)| {
                let mut text_buffer: TextBuffer = d!();
                text_buffer.rope = r;
                text_buffer.set_cursors(cursors);
                text_buffer
            })
    })
}

pub fn text_buffer_with_valid_cursors_and_no_0_to_9_chars(
    max_len: usize,
) -> impl Strategy<Value = TextBuffer> {
    non_0_to_9_char_rope().prop_flat_map(move |rop| {
        (valid_cursors_for_rope(rop.clone(), max_len), Just(rop)).prop_map(|(cursors, r)| {
            let mut text_buffer: TextBuffer = d!();
            text_buffer.rope = r;
            text_buffer.set_cursors(cursors);
            text_buffer
        })
    })
}

pub fn text_buffer_with_many_valid_cursors_and_no_0_to_9_chars(
    max_len: usize,
) -> impl Strategy<Value = TextBuffer> {
    non_0_to_9_char_rope().prop_flat_map(move |rop| {
        (many_valid_cursors_for_rope(rop.clone(), max_len), Just(rop)).prop_map(|(cursors, r)| {
            let mut text_buffer: TextBuffer = d!();
            text_buffer.rope = r;
            text_buffer.set_cursors(cursors);
            text_buffer
        })
    })
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

pub type CountNumber = usize;

pub type Counts = HashMap<char, CountNumber>;

pub fn get_counts(buffer: &TextBuffer) -> Counts {
    let mut output = HashMap::with_capacity(buffer.len());

    for c in buffer.chars() {
        let count = output.entry(c).or_insert(0);
        *count += 1;
    }

    output
}

pub fn increment_char_by(counts: &mut Counts, c: char, amount: CountNumber) {
    let count = counts.entry(c).or_insert(0);
    *count += amount;
}

pub fn decrement_char_by(counts: &mut Counts, c: char, amount: CountNumber) {
    let count = counts.entry(c).or_insert(0);
    *count = (*count).saturating_sub(amount);
}

pub fn increment_char(counts: &mut Counts, c: char) {
    increment_char_by(counts, c, 1);
}

pub fn decrement_char(counts: &mut Counts, c: char) {
    decrement_char_by(counts, c, 1);
}

pub fn increment_string(counts: &mut Counts, s: &str) {
    for c in s.chars() {
        increment_char(counts, c);
    }
}

pub fn decrement_string(counts: &mut Counts, s: &str) {
    for c in s.chars() {
        decrement_char(counts, c);
    }
}

pub fn increment_strings(counts: &mut Counts, strs: &Vec<String>) {
    for s in strs.iter() {
        increment_string(counts, s);
    }
}

pub fn decrement_strings(counts: &mut Counts, strs: &Vec<String>) {
    for s in strs.iter() {
        decrement_string(counts, s);
    }
}

impl TestEdit {
    pub fn apply(buffer: &mut TextBuffer, edit: TestEdit) {
        Self::apply_ref(buffer, &edit);
    }
    pub fn apply_ref(buffer: &mut TextBuffer, edit: &TestEdit) {
        use TestEdit::*;
        match edit {
            Insert(c) => buffer.insert(*c),
            InsertString(s) => buffer.insert_string(s.to_owned()),
            Delete => buffer.delete(),
            MoveAllCursors(r#move) => buffer.move_all_cursors(*r#move),
            ExtendSelectionForAllCursors(r#move) => buffer.extend_selection_for_all_cursors(*r#move),
            MoveCursors(index, r#move) => buffer.move_cursor(*index, *r#move),
            ExtendSelection(index, r#move) => buffer.extend_selection(*index, *r#move),
            SetCursor(position, replace_or_add) => buffer.set_cursor(position, *replace_or_add),
            DragCursors(position) => buffer.drag_cursors(position),
            SelectCharTypeGrouping(position, replace_or_add) => {
                buffer.select_char_type_grouping(position, *replace_or_add)
            }
            Cut => {
                buffer.cut_selections();
            }
            InsertNumbersAtCursors => buffer.insert_at_each_cursor(|i| i.to_string()),
            TabIn => buffer.tab_in(),
            TabOut => buffer.tab_out(),
        }
    }

    pub fn apply_with_counts(buffer: &mut TextBuffer, counts: &mut Counts, edit: &TestEdit) {
        use TestEdit::*;
        match edit {
            Insert(c) => {
                decrement_strings(counts, &buffer.copy_selections());
                for _ in 0..buffer.borrow_cursors_vec().len() {
                    increment_char(counts, *c);
                }
            },
            InsertString(s) => {
                decrement_strings(counts, &buffer.copy_selections());
                for _ in 0..buffer.borrow_cursors_vec().len() {
                    increment_string(counts, s);
                }
            },
            Delete => {
                let selections = buffer.copy_selections();
                if selections.len() == 0 {
                    for cur in buffer.borrow_cursors_vec().clone() {
                        let offsets = offset_pair(&buffer.rope, &cur);
                        match offsets {
                            (Some(o), None) if o > 0 => {
                                let delete_offset_range = AbsoluteCharOffsetRange::new(o - 1, o);
                                let s = edit::copy_string(&buffer.rope, delete_offset_range);
                                decrement_string(counts, &s);
      
                            }
                            _ => {},
                        }        
                    }
                } else {
                    decrement_strings(counts, &selections);
                }
            },
            Cut => {
                decrement_strings(counts, &buffer.copy_selections());
            },
            MoveAllCursors(_) | ExtendSelectionForAllCursors(_) | MoveCursors(_, _)
            | ExtendSelection(_, _) | SetCursor(_, _) | DragCursors(_)
            | SelectCharTypeGrouping(_, _) => {},
            InsertNumbersAtCursors => {
                decrement_strings(counts, &buffer.copy_selections());
                for i in 0..buffer.borrow_cursors_vec().len() {
                    increment_string(counts, &i.to_string());
                }
            },
            TabIn => {
                let mut selections = buffer.copy_selections();
                if selections.len() == 0 {
                    for _ in 0..buffer.borrow_cursors_vec().len() {
                        selections.push(d!());
                    }
                }

                for selection in selections {
                    let line_count: CountNumber = r!(selection).len_lines().0;
    
                    increment_char_by(
                        counts, 
                        edit::TAB_STR_CHAR,
                        dbg!(line_count * edit::TAB_STR_CHAR_COUNT)
                    );
                }
            },
            TabOut => {
                //TODO do something better than this tautology
                let mut clone = deep_clone(&buffer);
                clone.tab_out();
                let clone_counts = get_counts(&clone);

                // We don't want any non-whitespace characters to be changed
                let key_set: std::collections::HashSet<&char> = 
                    clone_counts.keys()
                    .chain(counts.keys())
                    .collect();
                for key in key_set {
                    assert!(
                        key.is_whitespace()
                        || match (clone_counts.get(key), counts.get(key)) {
                            (Some(0), None)|(None, Some(0)) => true,
                            (clone_count, count) => clone_count == count
                        },
                        "key.is_whitespace(): {:?}\n clone_counts.get(key): {:?}\n counts.get(key): {:?}",
                        key.is_whitespace(),
                        clone_counts.get(key),
                        counts.get(key)
                    );
                }

                *counts = clone_counts;
            },
        }
        Self::apply_ref(buffer, edit);
    }

    pub fn is_recordable(&self) -> bool {
        use TestEdit::*;
        match *self {
            SelectCharTypeGrouping(_, _) | MoveAllCursors(_) 
            | ExtendSelectionForAllCursors(_) | MoveCursors(_, _) 
            | ExtendSelection(_, _) | SetCursor(_, _) | DragCursors(_) => {
                false
            }
            Insert(_) | InsertString(_) | Delete | Cut 
            | InsertNumbersAtCursors | TabIn | TabOut => {
                true
            }
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

pub fn test_edit_tab_in_out_heavy() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        9 => Just(TabIn),
        9 => Just(TabOut),
        5 => test_edit_selection_changes(), // Make sure there can be selections
        1 => test_edit()
    ]
}

pub fn test_edit_delete_and_tab_in_out_heavy() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        9 => Just(TabIn),
        9 => Just(TabOut),
        9 => Just(Delete),
        5 => test_edit_selection_changes(), // Make sure there can be selections
        1 => test_edit()
    ]
}

// Generates only cursor movement and selection edits. Intended for use as a part of larger
pub fn test_edit_selection_changes() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
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
    ]
}

pub enum TestEditSpec {
    All,
    Insert,
    RegexInsert(Regex),
    SetCursorHeavy,
    TabInOutHeavy,
    DeleteAndTabInOutHeavy,
    SelectionChanges,
}

pub fn test_edits(max_len: usize, spec: TestEditSpec) -> impl Strategy<Value = Vec<TestEdit>> {
    use TestEditSpec::*;
    collection::vec(
        match spec {
            All => test_edit().boxed(),
            Insert => test_edit_insert().boxed(),
            RegexInsert(regex) => test_edit_regex_insert(regex).boxed(),
            SetCursorHeavy => test_edit_set_cursor_heavy().boxed(),
            TabInOutHeavy => test_edit_tab_in_out_heavy().boxed(),
            DeleteAndTabInOutHeavy => test_edit_delete_and_tab_in_out_heavy().boxed(),
            SelectionChanges => test_edit_selection_changes().boxed(),
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

prop_compose! {
    pub fn text_buffer_and_test_edits(max_len: usize, spec: TestEditSpec)
        (buffer in text_buffer_with_valid_cursors(), edits in test_edits(max_len, spec))         -> (TextBuffer, Vec<TestEdit>) {
        (buffer, edits)
    }
}

pub fn test_edit_select_something_vec() -> impl Strategy<Value = Vec<TestEdit>> {
    use TestEdit::*;
    (any::<u8>(), any::<u8>()).prop_map(|(kind, amount)| {
        macro_rules! header {
            ($($tokens:tt)*) => {
                vec![
                    SetCursor(pos!{l (amount as usize >> 4), o (amount as usize & 0b1111)}, ReplaceOrAdd::Add),
                    $($tokens)*
                ]
            }
        }
        match kind & 0b11 {
            0 => {
                header![
                    ExtendSelectionForAllCursors(Move::Up),
                    ExtendSelectionForAllCursors(Move::Left),
                ]
            }
            1 => {
                header![
                    ExtendSelectionForAllCursors(Move::Up),
                    ExtendSelectionForAllCursors(Move::Right),
                ]
            }
            2 => {
                header![
                    ExtendSelectionForAllCursors(Move::Down),
                    ExtendSelectionForAllCursors(Move::Left),
                ]
            }
            _ => {
                header![
                    ExtendSelectionForAllCursors(Move::Down),
                    ExtendSelectionForAllCursors(Move::Right),
                ]
            }
        }
    })
}

pub fn test_edit_delete_then_tab_out_vec() -> impl Strategy<Value = Vec<TestEdit>> {
    use TestEdit::*;
    (test_edit_select_something_vec(), test_edit_select_something_vec()).prop_map(|(del_select, tab_select)| {
        let mut output = Vec::with_capacity(8);

        for e in del_select {
            output.push(e);
        }
        output.push(Delete);
        for e in tab_select {
            output.push(e);
        }
        output.push(TabIn);

        output
    })
}

prop_compose! {
    pub fn test_edit_delete_then_tab_out_vec_and_index()
        (edits in test_edit_delete_then_tab_out_vec())
        (
            i in if edits.len() == 0 { 0..1 } else { 0..edits.len() },
            es in Just(edits)
         ) -> (Vec<TestEdit>, usize) {
        (es, i)
    }
}


