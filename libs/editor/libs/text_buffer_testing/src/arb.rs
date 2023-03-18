use proptest::{
    arbitrary::any,
    collection,
    option,
    prop_compose,
    prop_oneof,
    proptest,
    prelude::{Just, Strategy}
};

use panic_safe_rope::{BorrowRope, ByteIndex, LineIndex, Rope, RopeSlice};

use rope_pos::is_linebreak_char;

use std::collections::HashMap;
use platform_types::{*, screen_positioning::*};

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

/// This is expected to be used where the amount does not really matter, except that it must be
/// enough that the behaviour we want to test has enough space.
/// Code that assumes this is > 1 is known to exist as of this writing.
pub const SOME_AMOUNT: usize = 16;
/// This is expected to be used where the amount does not really matter, except that it must be
/// greater than `SOME_AMOUNT` so that out-of-bounds checks, etc. get tested.
pub const MORE_THAN_SOME_AMOUNT: usize = 24;

#[macro_export]
macro_rules! r {
    ($s:expr) => {
        Rope::from_str(&$s)
    };
}

// TODO? move all `arb` fns in here?
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

pub fn arb_move() -> impl Strategy<Value = Move> {
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

#[allow(unused)]
fn arb_cursor_state() -> impl Strategy<Value = CursorState> {
    prop_oneof![
        Just(CursorState::None),
        arb_move().prop_map(CursorState::PressedAgainstWall),
    ]
}

#[derive(Debug, Clone)]
pub enum TestEdit {
    Insert(char),
    InsertString(String),
    Delete,
    DeleteLines,
    MoveAllCursors(Move),
    ExtendSelectionForAllCursors(Move),
    MoveCursors(usize, Move),
    ExtendSelection(usize, Move),
    SetCursor(Position, ReplaceOrAdd),
    DragCursors(Position),
    SelectCharTypeGrouping(Position, ReplaceOrAdd),
    SelectAll,
    Cut,
    InsertNumbersAtCursors,
    TabIn,
    TabOut,
    StripTrailingWhitespace,
    ToggleCase,
    DuplicateLines,
    AutoIndentSelection,
}

pub type CountNumber = usize;
pub type CountMap = HashMap<char, CountNumber>;
pub type Laxity = u32;

pub const IGNORE_WHITESPACE: Laxity = 0b1;
pub const IGNORE_NON_ASCII: Laxity  = 0b10;

#[derive(Debug, Default)]
pub struct Counts {
    pub map: CountMap,
    pub laxity: Laxity,
}

impl Counts {
    pub fn expectations_met_by(&self, actual: &CountMap) -> bool {
        if self.laxity == 0 {
            &self.map == actual
        } else {
            let mut expected = self.map.clone();
            let mut actual = actual.clone();

            if self.laxity & IGNORE_WHITESPACE != 0 {
                expected.retain(|c, _| !c.is_whitespace());
                actual.retain(|c, _| !c.is_whitespace());
            } else if self.laxity & IGNORE_NON_ASCII != 0 {
                expected.retain(|c, _| !c.is_ascii());
                actual.retain(|c, _| !c.is_ascii());
            } else {
                panic!("unhandled laxity flag. {:b}", self.laxity);
            }

            expected == actual
        }
    }
}

impl From<CountMap> for Counts {
    fn from(map: CountMap) -> Self {
        Self {
            map,
            ..<_>::default()
        }
    }
}

impl Counts {
    pub fn get(&self, c: &char) -> Option<&CountNumber> {
        self.map.get(c)
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, char, CountNumber> {
        self.map.keys()
    }

    pub fn entry(&mut self, c: char) -> std::collections::hash_map::Entry<'_, char, CountNumber> {
        self.map.entry(c)
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&char, &mut CountNumber) -> bool
    {
        self.map.retain(f)
    }

    pub fn map(&self) -> &CountMap {
        &self.map
    }
}

#[macro_export]
macro_rules! counts_assert {
    ($buffer: expr, $expected: expr) => {
        let actual_counts: Counts = {
            // We inline get_counts to lessen the imports needed for this macro
            let rope = $buffer.borrow_rope();

            let mut output = std::collections::HashMap::with_capacity(rope.len_bytes().0);
        
            for c in rope.chars() {
                let count = output.entry(c).or_insert(0);
                *count += 1;
            }
        
            output.into()
        };
        let expected_counts = $expected;
        assert!(
            expected_counts.expectations_met_by(&actual_counts.map()),
            "{expected_counts:?} not met by {actual_counts:?}"
        );
    }
}

pub fn get_counts(buffer: impl BorrowRope) -> Counts {
    let rope = buffer.borrow_rope();

    let mut output = HashMap::with_capacity(rope.len_bytes().0);

    for c in rope.chars() {
        let count = output.entry(c).or_insert(0);
        *count += 1;
    }

    output.into()
}

pub fn get_normalized_newline_counts(buffer: impl BorrowRope) -> Counts {
    let rope = buffer.borrow_rope();

    let mut output = HashMap::with_capacity(rope.len_bytes().0);

    for mut c in rope.chars() {
        if is_linebreak_char(c) {
            c = '\n';
        }
        let count = output.entry(c).or_insert(0);
        *count += 1;
    }

    output.into()
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

pub trait TestEditApply
where Self: Sized {
    fn apply(buffer: Self, edit: TestEdit) {
        Self::apply_ref(buffer, &edit);
    }
    fn apply_ref(buffer: Self, edit: &TestEdit);

    fn apply_with_counts(buffer: Self, counts: &mut Counts, edit: &TestEdit);
}

impl TestEdit {
    // Kind of silly adapters so I don't need to update the old tests {
    pub fn apply(buffer: impl TestEditApply, edit: TestEdit) {
        TestEditApply::apply(buffer, edit);
    }
    pub fn apply_ref(buffer: impl TestEditApply, edit: &TestEdit) {
        TestEditApply::apply_ref(buffer, edit);
    }

    pub fn apply_with_counts(buffer: impl TestEditApply, counts: &mut Counts, edit: &TestEdit) {
        TestEditApply::apply_with_counts(buffer, counts, edit);
    }
    // }

    pub fn is_recordable(&self) -> bool {
        use TestEdit::*;
        match *self {
            SelectCharTypeGrouping(_, _) | MoveAllCursors(_)
            | ExtendSelectionForAllCursors(_) | MoveCursors(_, _)
            | ExtendSelection(_, _) | SetCursor(_, _) | DragCursors(_)
            | SelectAll => {
                false
            }
            Insert(_) | InsertString(_) | Delete | DeleteLines | Cut
            | InsertNumbersAtCursors | TabIn | TabOut | StripTrailingWhitespace
            | ToggleCase | DuplicateLines | AutoIndentSelection => {
                true
            }
        }
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
