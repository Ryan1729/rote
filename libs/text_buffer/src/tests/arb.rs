// This module is inside `tests`
use super::*;

pub use text_buffer_testing::arb::*;

use panic_safe_rope::{LineIndex};

use rope_pos::is_linebreak_char;

pub use pub_arb_cursors::{
    all_but_end_cursors_for_rope,
    cursor,
    many_valid_cursors_for_rope,
    many_valid_non_highlight_cursors_for_rope,
    valid_cursors_for_rope,
    vec1_of_cursors,
};
pub use pub_arb_edit::absolute_char_offset;
pub use pub_arb_text_pos::{pos};
pub use pub_arb_vec1::{vec1};

fn to_text_buffer((cursors, rope): (Cursors, Rope)) -> TextBuffer {
    let mut text_buffer: TextBuffer = d!();
    text_buffer.rope = CursoredRope::new(rope, cursors);
    text_buffer
}

pub fn text_buffer_with_many_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rop| {
        (
            many_valid_cursors_for_rope(rop.clone(), MORE_THAN_SOME_AMOUNT),
            Just(rop),
        ).prop_map(to_text_buffer)
    })
}

pub fn text_buffer_with_many_non_highlight_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rop| {
        (
            many_valid_non_highlight_cursors_for_rope(rop.clone(), MORE_THAN_SOME_AMOUNT),
            Just(rop),
        ).prop_map(to_text_buffer)
    })
}

pub fn text_buffer_with_all_but_end_cursors() -> impl Strategy<Value = TextBuffer> {
    rope().prop_map(|r| {
        let cursors = all_but_end_cursors_for_rope(&r);

        to_text_buffer((
            Cursors::new(&r, Vec1::try_from_vec(cursors).unwrap_or_default()),
            r
        ))
    })
}

pub fn all_space_text_buffer_with_many_cursors() -> impl Strategy<Value = TextBuffer> {
    all_space_rope().prop_flat_map(|rop| {
        (
            many_valid_cursors_for_rope(rop.clone(), MORE_THAN_SOME_AMOUNT),
            Just(rop),
        ).prop_map(to_text_buffer)
    })
}

pub fn no_history_text_buffer() -> impl Strategy<Value = TextBuffer> {
    rope().prop_flat_map(|rop| {
        (
            many_valid_cursors_for_rope(rop.clone(), rop.chars().count()),
            Just(rop),
        ).prop_map(to_text_buffer)
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
        ).prop_map(to_text_buffer)
    })
}

pub fn text_buffer_with_no_selection() -> impl Strategy<Value = TextBuffer> {
    text_buffer_with_valid_cursors().prop_map(|mut buffer| {
        use ReplaceOrAdd::*;

        let mut is_first = true;

        for cur in buffer.borrow_cursors().clone().iter() {
            buffer.set_cursor(cur.get_position(), if is_first {
                is_first = false;
                Replace
            } else {
                Add
            });
        }

        buffer
    })
}

pub fn text_buffer_with_valid_cursors_and_no_0_to_9_chars(
    max_len: usize,
) -> impl Strategy<Value = TextBuffer> {
    non_0_to_9_char_rope().prop_flat_map(move |rop| {
        (valid_cursors_for_rope(rop.clone(), max_len), Just(rop))
        .prop_map(to_text_buffer)
    })
}

pub fn text_buffer_with_many_valid_cursors_and_no_0_to_9_chars(
    max_len: usize,
) -> impl Strategy<Value = TextBuffer> {
    non_0_to_9_char_rope().prop_flat_map(move |rop| {
        (many_valid_cursors_for_rope(rop.clone(), max_len), Just(rop))
        .prop_map(to_text_buffer)
    })
}

impl TestEditApply for &mut TextBuffer {
    fn apply_ref(buffer: Self, edit: &TestEdit) {
        use TestEdit::*;
        match edit {
            Insert(c) => { buffer.insert(*c, None); },
            InsertString(s) => { buffer.insert_string(s.to_owned(), None); },
            Delete => { buffer.delete(None); },
            DeleteLines => { buffer.delete_lines(None); },
            MoveAllCursors(r#move) => buffer.move_all_cursors(*r#move),
            ExtendSelectionForAllCursors(r#move) => buffer.extend_selection_for_all_cursors(*r#move),
            MoveCursors(index, r#move) => buffer.move_cursor(*index, *r#move),
            ExtendSelection(index, r#move) => buffer.extend_selection(*index, *r#move),
            SetCursor(position, replace_or_add) => buffer.set_cursor(position, *replace_or_add),
            DragCursors(position) => buffer.drag_cursors(position),
            SelectCharTypeGrouping(position, replace_or_add) => {
                buffer.select_char_type_grouping(*position, *replace_or_add)
            }
            SelectAll => buffer.select_all(),
            Cut => {
                buffer.cut_selections(None);
            }
            InsertNumbersAtCursors => { buffer.insert_at_each_cursor(|i| i.to_string(), None); },
            TabIn => { buffer.tab_in(None); },
            TabOut => { buffer.tab_out(None); },
            StripTrailingWhitespace => { buffer.strip_trailing_whitespace(None); },
            ToggleCase => { buffer.toggle_case(None); },
            DuplicateLines => { buffer.duplicate_lines(None); },
            AutoIndentSelection => { buffer.auto_indent_selection(None); },
        };
    }

    // TODO: can we use the ppel!() type, that was created for other reasons, to
    // do this instead? We'd need to make ppel!() into a trait I suppose.
    fn apply_with_counts(buffer: Self, counts: &mut Counts, edit: &TestEdit) {
        fn apply_delete_edit(counts: &mut Counts, buffer: &TextBuffer, cursors_vec: Vec1<Cursor>) {
            let cursors = Cursors::new(buffer.borrow_rope(), cursors_vec);
            for cur in cursors.iter() {
                let offsets = offset_pair(buffer.borrow_rope(), &cur);
                match offsets {
                    (Some(o), None) if o > 0 => {
                        let delete_offset_range = AbsoluteCharOffsetRange::new(o - 1, o);
                        let s = edit::copy_string(buffer.borrow_rope(), delete_offset_range);
                        decrement_string(counts, &s);
                    }
                    (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                        let delete_offset_range = AbsoluteCharOffsetRange::new(o1, o2);
                        let s = edit::copy_string(buffer.borrow_rope(), delete_offset_range);
                        decrement_string(counts, &s);
                    }
                    _ => {},
                }
            }
        }

        use TestEdit::*;
        match edit {
            Insert(c) => {
                decrement_strings(counts, &buffer.copy_selections());
                for _ in 0..buffer.borrow_cursors().len() {
                    increment_char(counts, *c);
                }
            },
            InsertString(s) => {
                decrement_strings(counts, &buffer.copy_selections());
                for _ in 0..buffer.borrow_cursors().len() {
                    increment_string(counts, s);
                }
            },
            Delete => {
                apply_delete_edit(counts, buffer, buffer.borrow_cursors().get_cloned_cursors());
            },
            DeleteLines => {
                let mut cursor_vec = buffer.borrow_cursors().get_cloned_cursors();
                for c in cursor_vec.iter_mut() {
                    edit::extend_cursor_to_cover_line(c, buffer.borrow_rope());
                }
                apply_delete_edit(counts, buffer, cursor_vec);
            },
            Cut => {
                decrement_strings(counts, &buffer.copy_selections());
            },
            MoveAllCursors(_) | ExtendSelectionForAllCursors(_) | MoveCursors(_, _)
            | ExtendSelection(_, _) | SetCursor(_, _) | DragCursors(_)
            | SelectCharTypeGrouping(_, _) | SelectAll => {},
            InsertNumbersAtCursors => {
                decrement_strings(counts, &buffer.copy_selections());
                for i in 0..buffer.borrow_cursors().len() {
                    increment_string(counts, &i.to_string());
                }
            },
            TabIn => {
                let selections: Vec<_> = buffer.borrow_cursors()
                    .iter()
                    .map(|cur|
                        match offset_pair(buffer.borrow_rope(), &cur) {
                            (Some(o1), Some(o2)) => {
                                let range = AbsoluteCharOffsetRange::new(o1, o2);
                                buffer.borrow_rope().slice(range.min()..range.max())
                            }
                            _ => None,
                        }.unwrap_or_else(|| buffer.borrow_rope().empty_slice())
                    ).collect();

                for selection in selections {
                    let line_count: CountNumber = selection.len_lines().0;

                    increment_char_by(
                        counts,
                        edit::TAB_STR_CHAR,
                        line_count * edit::TAB_STR_CHAR_COUNT
                    );

                    for line in selection.lines() {
                        for c in line.chars() {
                            if c != edit::TAB_STR_CHAR && !is_linebreak_char(c) && c.is_whitespace() {
                                increment_char(
                                    counts,
                                    edit::TAB_STR_CHAR
                                );
                                decrement_char(
                                    counts,
                                    c
                                )
                            } else {
                                break
                            }
                        }
                    }
                }
            },
            TabOut => {
                //TODO do something better than this tautology
                let mut clone = deep_clone(&buffer);
                clone.tab_out(None);
                let clone_counts = get_counts(&clone);
                std::dbg!(&buffer, &clone_counts);

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
                        "key: {key}\n key.is_whitespace(): {:?}\n clone_counts.get(key): {:?}\n counts.get(key): {:?}\n",
                        key.is_whitespace(),
                        clone_counts.get(key),
                        counts.get(key)
                    );
                }

                *counts = clone_counts;
            },
            StripTrailingWhitespace => {
                // It doesn't seem worth it at this time to track the exact amount
                // of whitespace to remove.
                counts.laxity |= IGNORE_WHITESPACE;
            }
            ToggleCase => {
                // We do not care thgat much about what happens to non-ASCII 
                // characters regarding case.
                counts.laxity |= IGNORE_NON_ASCII;

                let mut selections = buffer.copy_selections();
                std::dbg!(&selections);
                for selection in &mut selections {
                    selection.retain(|c| c.is_ascii_alphabetic());
                }
                decrement_strings(counts, &selections);

                for selection in &mut selections {
                    if let Some(last) = selection.chars().last() {
                        if last.is_ascii_uppercase() {
                            selection.make_ascii_lowercase();
                        } else {
                            selection.make_ascii_uppercase();
                        }
                    }
                }
                increment_strings(counts, &selections);
            },
            DuplicateLines => {
                if buffer.is_empty() {
                    // No change to counts
                } else {
                    fn extend_cursor_to_same_lines(c: &mut Cursor) {
                        use core::cmp::{min, max};
                        let position = c.get_position();
                        let highlight_position = c.get_highlight_position_or_position();

                        let min_line = min(position.line, highlight_position.line);
                        let max_line = max(position.line, highlight_position.line) + 1;

                        *c = cur!{pos!{l min_line, o 0}, pos!{l max_line, o 0}};
                    }

                    let mut cursor_vec = buffer.borrow_cursors().get_cloned_cursors();
                    for c in cursor_vec.iter_mut() {
                        extend_cursor_to_same_lines(c);
                    }

                    let cursors = Cursors::new(buffer.borrow_rope(), cursor_vec);
                    for cur in cursors.iter() {
                        let offsets = offset_pair(buffer.borrow_rope(), &cur);
                        std::dbg!(&offsets);
                        match offsets {
                            (Some(_o), None) => {
                                // No change to counts
                            }
                            (Some(o1), Some(o2)) => {
                                let s = edit::copy_string(
                                    buffer.borrow_rope(),
                                    AbsoluteCharOffsetRange::new(o1, o2)
                                );
                                let ends_with_newline_or_is_empty
                                    = s.chars()
                                    .last()
                                    .map(is_linebreak_char)
                                    .unwrap_or(true);
                                if !ends_with_newline_or_is_empty {
                                    increment_char(
                                        counts,
                                        '\n'
                                    );
                                }
                                increment_string(counts, &s);
                            }
                            _ => {},
                        }
                    }
                }
            },
            AutoIndentSelection => {
                // It doesn't seem worth it at this time to track the exact amount
                // of whitespace add/remove.
                counts.laxity |= IGNORE_WHITESPACE;
            }
        }
        Self::apply_ref(buffer, edit);
    }
}

pub fn test_edit() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        Just(Delete),
        Just(DeleteLines),
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
            pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT),
            replace_or_add()
        )
            .prop_map(|(p, r)| SetCursor(p, r)),
        pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT).prop_map(DragCursors),
        (
            pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT),
            replace_or_add()
        )
            .prop_map(|(p, r)| SelectCharTypeGrouping(p, r)),
        Just(SelectAll),
        Just(Cut),
        Just(InsertNumbersAtCursors),
        Just(TabIn),
        Just(TabOut),
        Just(StripTrailingWhitespace),
        Just(ToggleCase),
        Just(DuplicateLines),
        Just(AutoIndentSelection),
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
        9 => pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT).prop_map(|p| SetCursor(p, ReplaceOrAdd::Add)),
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
        9 => Just(DeleteLines),
        5 => test_edit_selection_changes(), // Make sure there can be selections
        1 => test_edit()
    ]
}

pub fn test_edit_delete_lines_heavy() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        9 => Just(DeleteLines),
        5 => test_edit_selection_changes(), // Make sure there can be selections
        1 => test_edit()
    ]
}

pub fn test_edit_toggle_case_heavy() -> impl Strategy<Value = TestEdit> {
    use TestEdit::*;
    prop_oneof![
        9 => Just(ToggleCase),
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
            pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT),
            replace_or_add()
        )
            .prop_map(|(p, r)| SetCursor(p, r)),
        pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT).prop_map(DragCursors),
        (
            pos(MORE_THAN_SOME_AMOUNT, MORE_THAN_SOME_AMOUNT),
            replace_or_add()
        )
            .prop_map(|(p, r)| SelectCharTypeGrouping(p, r)),
        // TODO would adding this line adversely affect probabiliites of interesting cursor placements?
        // Just(SelectAll)
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
    DeleteLinesHeavy,
    ToggleCaseHeavy,
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
            DeleteLinesHeavy => test_edit_delete_lines_heavy().boxed(),
            ToggleCaseHeavy => test_edit_toggle_case_heavy().boxed(),
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
        (buffer in text_buffer_with_valid_cursors(), edits in test_edits(max_len, spec))
         -> (TextBuffer, Vec<TestEdit>) {
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


