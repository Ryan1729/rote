#![deny(unused)]
use cursors::Cursors;
use editor_types::{Cursor, SetPositionAction, cur};
use macros::{CheckedSub, d, some_or, dbg};
use panic_safe_rope::{is_linebreak_char, LineIndex, Rope, RopeSliceTrait, RopeLine};
use platform_types::*;
use rope_pos::{AbsoluteCharOffsetRange, char_offset_to_pos, final_non_newline_offset_for_rope_line, get_first_non_white_space_offset_in_range, get_last_non_white_space_offset_in_range, offset_pair, pos_to_char_offset};

use std::cmp::{min, max};

pub fn apply(rope: &mut CursoredRope, edit: &Edit) {
    // we assume that the edits are in the proper order so we won't mess up our indexes with our
    // own inserts and removals. I'm not positive that there being a single order that works
    // is possible for all possible edits, but in practice I think the edits we will actually
    // produce will work out. The tests should tell us if we're wrong!
    for range_edit in edit.range_edits.iter() {
        rope.apply(range_edit);
    }

    rope.set_cursors(edit.cursors.new.clone());
}

#[derive(Debug)]
enum SpecialHandling {
    None,
    HighlightOnLeftShiftedLeftBy(usize),
    HighlightOnRightPositionShiftedLeftBy(usize),
}
d!(for SpecialHandling: SpecialHandling::None);

fn get_special_handling(
    original_rope: &Rope,
    cursor: &Cursor,
    highlight_length: usize,
) -> SpecialHandling {
    dbg!(&original_rope);
    let p = cursor.get_position();
    dbg!(&p);
    if let Some(h) = cursor.get_highlight_position() {
        dbg!(&h);
        if let (Some(h_abs), Some(p_abs)) = dbg!(
            pos_to_char_offset(original_rope, &h),
            pos_to_char_offset(original_rope, &p)
        ) {
            dbg!();
            return if h_abs > p_abs {
                SpecialHandling::HighlightOnRightPositionShiftedLeftBy(highlight_length)
            } else {
                SpecialHandling::HighlightOnLeftShiftedLeftBy(highlight_length)
            };
        }
    }
    d!()
}

#[derive(Debug)]
enum PostDeltaShift {
    None,
    Left,
}
d!(for PostDeltaShift: PostDeltaShift::None);

#[derive(Debug, Default)]
struct CursorPlacementSpec {
    offset: AbsoluteCharOffset,
    delta: isize,
    special_handling: SpecialHandling,
    post_delta_shift: PostDeltaShift,
}

type EditSpec = (RangeEdits, CursorPlacementSpec);

/// Calls the `FnMut` once with a copy of each cursor and a reference to the same clone of the
/// `Rope`. Then the (potentially) modified cursors and another copy of the `original_cursors`
/// are wrapped up along with the returned `RangeEdit`s into the Edit.
#[perf_viz::record]
fn get_edit<F>(original_rope: &Rope, original_cursors: &Cursors, mut mapper: F) -> Edit
where
    F: FnMut(&Cursor, &mut Rope, usize) -> EditSpec,
{
    perf_viz::start_record!("init cloning");
    let mut cloned_rope = original_rope.clone();
    perf_viz::end_record!("init cloning");

    // the index needs to account for the order being from the high positions
    // to the low positions.
    perf_viz::start_record!("range_edits");
    let mut index = original_cursors.len();
    let mut specs = Vec::with_capacity(index);
    let range_edits = original_cursors.mapped_ref(|c| {
        index -= 1;
        let (edits, spec) = mapper(c, &mut cloned_rope, index);

        specs.push(spec);
        edits
    });
    perf_viz::end_record!("range_edits");

    construct_edit(
        cloned_rope,
        original_cursors,
        specs,
        range_edits
    )
}

fn construct_edit(
    updated_rope: Rope,
    original_cursors: &Cursors,
    specs: Vec<CursorPlacementSpec>,
    range_edits: Vec1<RangeEdits>
) -> Edit {
    perf_viz::record_guard!("construct result");

    // We need the cursors to be sorted in reverse order, so our `range_edits` 
    // are in the right order, so we can go backwards when we apply them, so 
    // our indexes don't get messed up by our own inserts and deletes. 
    // The Cursors type is expected to maintain this ordering.
    let mut cloned_cursors = original_cursors.get_cloned_cursors();

    let mut total_delta: isize = 0;
    for (
        i,
        CursorPlacementSpec {
            offset,
            delta,
            special_handling,
            post_delta_shift,
        },
    ) in specs.into_iter().enumerate().rev()
    {
        total_delta = total_delta.saturating_add(delta);

        // This is only correct if the `offset` doesn't have the sign bit set.
        // Plus overflow and so forth, but these are probably 64 bits so who cares?
        let mut signed_offset = offset.0 as isize + total_delta;
        match post_delta_shift {
            PostDeltaShift::None => {}
            PostDeltaShift::Left => {
                signed_offset -= 1;
            }
        }
        let o = AbsoluteCharOffset(if signed_offset > 0 {
            signed_offset as usize
        } else {
            0
        });

        let cursor = &mut cloned_cursors[i];

        let action = SetPositionAction::ClearHighlight;
        match special_handling {
            SpecialHandling::None => {
                move_cursor::to_absolute_offset(&updated_rope, cursor, o, action);
            }
            SpecialHandling::HighlightOnLeftShiftedLeftBy(len) => {
                move_cursor::to_absolute_offset(&updated_rope, cursor, o, action);
                if let Some(h) = o
                    .checked_sub(len)
                    .and_then(|o| char_offset_to_pos(&updated_rope, o))
                {
                    cursor.set_highlight_position(h);
                }
            }
            SpecialHandling::HighlightOnRightPositionShiftedLeftBy(len) => {
                let p = o.checked_sub(len).unwrap_or_default();
                move_cursor::to_absolute_offset(&updated_rope, cursor, p, action);
                let h =
                    char_offset_to_pos(&updated_rope, o).unwrap_or_else(|| cursor.get_position());
                cursor.set_highlight_position(h);
            }
        }
    }

    Edit {
        range_edits,
        cursors: Change {
            new: Cursors::new(&updated_rope, cloned_cursors),
            old: original_cursors.clone(),
        },
    }
}

fn get_standard_insert_range_edits(
    rope: &mut Rope,
    cursor: &Cursor,
    offset: AbsoluteCharOffset,
    chars: String,
    char_count: usize, //we take this as a param to support it being a const.
) -> (RangeEdits, CursorPlacementSpec) {
    let insert_option = rope.insert(offset, &chars);
    if cfg!(feature = "invariant-checking") {
        insert_option.expect(&format!("offset {offset} was invalid!"));
    }

    let range = AbsoluteCharOffsetRange::new(offset, offset + char_count);

    let mut post_delta_shift = d!();
    if chars.starts_with('\n') {
        if let Some(char_before_insert) = offset.checked_sub_one().and_then(|o| rope.char(o)) {
            if char_before_insert == '\r' {
                post_delta_shift = PostDeltaShift::Left;
            }
        }
    }

    (
        RangeEdits {
            insert_range: Some(RangeEdit { chars, range }),
            ..d!()
        },
        CursorPlacementSpec {
            offset: pos_to_char_offset(&rope, &cursor.get_position()).unwrap_or_default(),
            delta: char_count as isize,
            post_delta_shift,
            ..d!()
        },
    )
}

/// Returns an edit that, if applied, after deleting the highlighted region at each cursor if
/// there is one, inserts the given string at each of the cursors.
#[perf_viz::record]
pub fn get_insert_edit<F>(
    rc: impl RC,
    get_string: F
) -> Edit
where
    F: Fn(usize) -> String,
{
    let (original_rope, original_cursors) = rc.rc();

    get_edit(
        original_rope,
        original_cursors,
        |cursor, rope, index| match offset_pair(original_rope, cursor) {
            (Some(o), highlight) if highlight.is_none() || Some(o) == highlight => {
                let s = get_string(index);
                let char_count = s.chars().count();
                get_standard_insert_range_edits(rope, cursor, o, s, char_count)
            }
            (Some(o1), Some(o2)) => {
                let s = get_string(index);

                let (range_edit, delete_offset, delete_delta) = delete_within_range(
                    rope,
                    AbsoluteCharOffsetRange::new(o1, o2)
                );

                let min = range_edit.range.min();
                let char_count = s.chars().count();

                let mut edits = get_standard_insert_range_edits(rope, cursor, min, s, char_count);

                edits.0.delete_range = Some(range_edit);
                edits.1.offset = delete_offset;
                edits.1.delta += delete_delta;

                edits
            }
            _ => d!(),
        },
    )
}

/// Returns an edit that, if applied, deletes the highlighted region at each cursor if there is one.
/// Otherwise the applying the edit will delete a single character at each cursor.
pub fn get_delete_edit(rc: impl RC) -> Edit {
    let (original_rope, original_cursors) = rc.rc();

    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);
        match offsets {
            (Some(o), None) if o > 0 => {
                let delete_offset_range = AbsoluteCharOffsetRange::new(o - 1, o);
                let chars = copy_string(&rope, delete_offset_range);
                let remove_option = rope.remove(delete_offset_range.range());
                if cfg!(feature = "invariant-checking") {
                    remove_option.expect(&format!("delete offset {delete_offset_range:?} was invalid!"));
                }

                let min = delete_offset_range.min();
                let max = delete_offset_range.max();
                (
                    RangeEdits {
                        delete_range: Some(RangeEdit {
                            chars,
                            range: delete_offset_range,
                        }),
                        ..d!()
                    },
                    CursorPlacementSpec {
                        offset: max,
                        delta: min.0 as isize - max.0 as isize,
                        ..d!()
                    },
                )
            }
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                let (range_edit, delete_offset, delete_delta) = delete_within_range(
                    rope,
                    AbsoluteCharOffsetRange::new(o1, o2)
                );

                (
                    RangeEdits {
                        delete_range: Some(range_edit),
                        ..d!()
                    },
                    CursorPlacementSpec {
                        offset: delete_offset,
                        delta: delete_delta,
                        ..d!()
                    },
                )
            }
            _ => d!(),
        }
    })
}

pub fn extend_cursor_to_cover_line(c: &mut Cursor, rope: &Rope) {
    let position = c.get_position();
    let highlight_position = c.get_highlight_position_or_position();

    let min_line = min(position.line, highlight_position.line);
    let max_line = max(position.line, highlight_position.line) + 1;

    let has_no_next_line = rope.len_lines().0 <= max_line;
    
    if has_no_next_line {
        if let Some((previous_line, last_non_newline_offset)) = min_line
            .checked_sub(1)
            .and_then(|i| 
                rope.line(LineIndex(i))
                    .map(final_non_newline_offset_for_rope_line)
                    .map(|o| (i, o.0))
            ) {
            *c = dbg!(cur!{pos!{l previous_line, o last_non_newline_offset}, pos!{l max_line, o 0}});
            return
        }
    }

    *c = dbg!(cur!{pos!{l min_line, o 0}, pos!{l max_line, o 0}});
}

/// Returns an edit that, if applied, deletes the line(s) each cursor intersects with.
pub fn get_delete_lines_edit(rc: impl RC) -> Edit {
    let (original_rope, original_cursors) = rc.rc();

    let mut extended_cursors = original_cursors.get_cloned_cursors();
    for c in extended_cursors.iter_mut() {
        extend_cursor_to_cover_line(c, original_rope);
    }

    let mut edit = get_delete_edit((original_rope, &Cursors::new(original_rope, extended_cursors)));

    edit.cursors.old = original_cursors.clone();

    edit
}

/// returns an edit that if applied will delete the highlighted region at each cursor if there is
/// one, and which does nothing otherwise.
pub fn get_cut_edit(rc: impl RC) -> Edit {
    let (original_rope, original_cursors) = rc.rc();

    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => {
                let (range_edit, delete_offset, delete_delta) = delete_within_range(
                    rope,
                    AbsoluteCharOffsetRange::new(o1, o2)
                );

                (
                    RangeEdits {
                        delete_range: Some(range_edit),
                        ..d!()
                    },
                    CursorPlacementSpec {
                        offset: delete_offset,
                        delta: delete_delta,
                        ..d!()
                    },
                )
            }
            (Some(o), None) => {
                (
                    d!(),
                    CursorPlacementSpec {
                        offset: o,
                        ..d!()
                    },
                )
            }
            _ => d!(),
        }
    })
}

pub const TAB_STR: &str = "    "; //four spaces
pub const TAB_STR_CHAR: char = ' ';
pub const TAB_STR_CHAR_COUNT: usize = 4; // this isn't const (yet?) TAB_STR.chars().count();

#[perf_viz::record]
pub fn get_tab_in_edit(rc: impl RC) -> Edit {
    let (original_rope, original_cursors) = rc.rc();

    get_edit(
        original_rope,
        original_cursors,
        |cursor, rope, _| match offset_pair(original_rope, cursor) {
            (Some(o), highlight) if highlight.is_none() || Some(o) == highlight => {
                get_standard_insert_range_edits(
                    rope,
                    cursor,
                    o,
                    TAB_STR.to_owned(),
                    TAB_STR_CHAR_COUNT,
                )
            }
            (Some(o1), Some(o2)) => {
                let range = AbsoluteCharOffsetRange::new(o1, o2);

                let mut chars = String::with_capacity(range.max().0 - range.min().0);

                let line_indicies = some_or!(line_indicies_touched_by(rope, range), return d!());

                let mut tab_insert_count = 0;
                let last_line_index = line_indicies.len() - 1;
                for (i, index) in line_indicies.into_iter().enumerate() {
                    let line = some_or!(rope.line(index), continue);

                    let line_start = some_or!(rope.line_to_char(index), continue);
                    let relative_line_end = final_non_newline_offset_for_rope_line(line);
                    let line_end = line_start + relative_line_end;

                    let highlight_start_for_line = max(range.min(), line_start) - line_start;
                    let highlight_end_for_line = if line_end < range.max() {
                        relative_line_end
                    } else {
                        range.max() - line_start
                    };

                    let highlight_range = dbg!(highlight_start_for_line..=highlight_end_for_line);

                    let first_highlighted_non_white_space_offset: Option<CharOffset> =
                        get_first_non_white_space_offset_in_range(
                            line,
                            highlight_range.clone(),
                        );

                    let start = highlight_start_for_line.0;
                    let previous_offset = min(
                        first_highlighted_non_white_space_offset
                            .unwrap_or(CharOffset(usize::max_value())),
                        highlight_end_for_line,
                    );

                    let end = previous_offset.0 + TAB_STR_CHAR_COUNT;
                    dbg!(start, end);
                    for _ in start..end {
                        chars.push(TAB_STR_CHAR);
                    }
                    tab_insert_count += 1;

                    let o = first_highlighted_non_white_space_offset.unwrap_or(relative_line_end);

                    let should_include_newlline =
                        highlight_end_for_line != relative_line_end || i != last_line_index;

                    let slice_end =
                        if highlight_end_for_line >= relative_line_end && should_include_newlline {
                            line.len_chars()
                        } else {
                            highlight_end_for_line
                        };
                    let highlighted_line_after_whitespace =
                        some_or!(line.slice(o..slice_end), continue);

                    if let Some(s) = highlighted_line_after_whitespace.as_str_if_no_allocation_needed() {
                        chars.push_str(s);
                    } else { 
                        for c in highlighted_line_after_whitespace.chars() {
                            chars.push(c);
                        }
                    }
                }

                let (range_edit, delete_offset, delete_delta) = delete_within_range(rope, range);

                let range = range_edit
                    .range
                    .add_to_max(TAB_STR_CHAR_COUNT * tab_insert_count);

                let char_count = chars.chars().count();
                dbg!(char_count);

                let special_handling = get_special_handling(
                    &original_rope,
                    cursor,
                    char_count.saturating_sub(TAB_STR_CHAR_COUNT),
                );

                let min = range.min();

                let mut edits = get_standard_insert_range_edits(
                    rope,
                    cursor,
                    min,
                    chars,
                    char_count,
                );

                edits.0.delete_range = Some(range_edit);
                edits.1.offset = delete_offset;
                edits.1.delta += delete_delta;
                edits.1.special_handling = special_handling;

                dbg!(edits)
            }
            _ => d!(),
        },
    )
}

pub fn get_tab_out_edit(rc: impl RC) -> Edit {
    let (original_rope, original_cursors) = rc.rc();

    get_line_slicing_edit(
        original_rope,
        original_cursors,
        tab_out_step,
    )
}

fn tab_out_step(
    line: RopeLine,
    RelativeSelected{ line_end, slice_end }: RelativeSelected,
    chars: &mut String,
) {
    let first_non_white_space_offset: Option<CharOffset> =
        get_first_non_white_space_offset_in_range(line, d!()..=line_end);

    let delete_count = min(
        first_non_white_space_offset.unwrap_or(line_end),
        CharOffset(TAB_STR_CHAR_COUNT),
    );

    if let Some(sliced_line) = line.slice(delete_count..slice_end) {
        if let Some(s) = sliced_line.as_str_if_no_allocation_needed() {
            chars.push_str(s);
        } else { 
            for c in sliced_line.chars() {
                chars.push(c);
            }
        }
    }
}

/// returns an edit that if applied will delete the non-line-ending whitespace at the 
/// end of each line in the buffer, if there is any.
pub fn get_strip_trailing_whitespace_edit(
    rc: impl RC
) -> Edit {
    let (original_rope, original_cursors) = rc.rc();

    get_line_slicing_edit(
        original_rope,
        original_cursors,
        strip_trailing_whitespace_step,
    )
}

fn strip_trailing_whitespace_step(
    line: RopeLine,
    RelativeSelected{ line_end, slice_end }: RelativeSelected,
    chars: &mut String,
) {
    if slice_end == CharOffset(0) {
        return
    }

    dbg!(line, line_end, slice_end);
    let last_non_white_space_offset: Option<CharOffset> =
        get_last_non_white_space_offset_in_range(line, d!()..=line_end)
        // We add one so we keep the final non-whitespace
        .map(|CharOffset(o)| CharOffset(o.saturating_add(1)));

    let strip_after = min(
        last_non_white_space_offset.unwrap_or(CharOffset(0)),
        slice_end,
    );

    dbg!(last_non_white_space_offset, last_non_white_space_offset.unwrap_or(line_end), strip_after);
    if let Some(sliced_line) = line.slice(CharOffset(0)..strip_after) {
        if let Some(s) = sliced_line.as_str_if_no_allocation_needed() {
            chars.push_str(s);
        } else { 
            for c in sliced_line.chars() {
                chars.push(c);
            }
        }
    }

    // TODO It strikes me that this condition can probably be less complicated
    if chars.ends_with(is_linebreak_char) && strip_after != CharOffset(0) {
        return;
    }

    if let Some(last_char) = dbg!(line.chars_at_end().prev()) {
        if is_linebreak_char(last_char) {
            chars.push('\n');
        }
    }
}

struct RelativeSelected {
    line_end: CharOffset, // Before newline if any
    slice_end: CharOffset, // Including newline if any
}

/// We expect the step to push the characters that are NOT deleted onto the string
/// Yes this is a slightly awkward API, but I think it is the least awkward of the
/// considered possibilities.
type LineSlicingEditStep = fn(RopeLine, RelativeSelected, &mut String);

fn get_line_slicing_edit(
    original_rope: &Rope,
    original_cursors: &Cursors,
    step: LineSlicingEditStep,
) -> Edit {
    get_edit(
        original_rope,
        original_cursors,
        |cursor, rope, _| match offset_pair(original_rope, cursor) {
            (Some(o1), offset2) => {
                let o2 = offset2.unwrap_or(o1);
                let selected_range = AbsoluteCharOffsetRange::new(o1, o2);
                let line_indicies = dbg!(some_or!(
                    line_indicies_touched_by(rope, selected_range),
                    return d!()
                ));
                let selected_max = selected_range.max();
                
                // a range extending the selected range to the leading edges of the relevant lines.
                let leading_line_edge_range = {
                    let first_line_index = line_indicies[0];

                    AbsoluteCharOffsetRange::new(
                        some_or!(rope.line_to_char(first_line_index), return d!()),
                        selected_max,
                    )
                };

                let mut chars = String::with_capacity(leading_line_edge_range.max().0 - leading_line_edge_range.min().0);

                let last_line_indicies_index = line_indicies.len() - 1;

                for (i, index) in line_indicies.into_iter().enumerate() {
                    let line = some_or!(rope.line(index), continue);

                    let should_include_entire_end_of_line = dbg!(i != last_line_indicies_index);

                    let (line_end, slice_end) = if should_include_entire_end_of_line {
                        (final_non_newline_offset_for_rope_line(line), line.len_chars())
                    } else {
                        let first_char_of_line: AbsoluteCharOffset = some_or!(
                            rope.line_to_char(index),
                            continue
                        );
                        let end_of_selection_on_line: CharOffset = selected_max - first_char_of_line;
                        dbg!(end_of_selection_on_line, end_of_selection_on_line)
                    };

                    dbg!(
                        &line,
                        line_end,
                        slice_end,
                        &mut chars
                    );

                    step(
                        line,
                        RelativeSelected {
                            line_end,
                            slice_end,
                        },
                        &mut chars
                    );
                }

                replace_in_range(
                    cursor,
                    rope,
                    leading_line_edge_range,
                    chars
                )
            }
            _ => d!(),
        },
    )
}

fn replace_in_range(
    cursor: &Cursor,
    rope: &mut Rope,
    range: AbsoluteCharOffsetRange,
    chars: String
) -> EditSpec {
    let char_count = chars.chars().count();

    let special_handling = get_special_handling(&rope, cursor, char_count);

    let (delete_edit, delete_offset, delete_delta) = dbg!(delete_within_range(rope, range));

    // AKA `-delete_delta - char_count`.
    // Doing it like this avoids some overflow cases
    let char_delete_count = (-(delete_delta + char_count as isize)) as usize;

    let insert_edit_range = some_or!(
        range.checked_sub_from_max(char_delete_count),
        return d!()
    );

    let insert_range = delete_edit.range.min();
    let insert_option = rope.insert(insert_range, &chars);
    if cfg!(feature = "invariant-checking") {
        insert_option.expect(
            &format!("offset {insert_range} was invalid!")
        );
    }

    dbg!(
        RangeEdits {
            insert_range: Some(RangeEdit { chars, range: insert_edit_range }),
            delete_range: Some(delete_edit),
        },
        CursorPlacementSpec {
            offset: delete_offset,
            delta: char_count as isize + delete_delta,
            special_handling,
            ..d!()
        },
    )
}


/// The length of `range_edits` must be greater than or equal to the length of the
/// two `Vec1`s in `cursors`. This is because we assume this is the case in `read_at`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Edit {
    range_edits: Vec1<RangeEdits>,
    cursors: Change<Cursors>,
}

impl Edit {
    pub fn range_edits(&self) -> &Vec1<RangeEdits> {
        &self.range_edits
    }

    pub fn cursors(&self) -> &Change<Cursors> {
        &self.cursors
    }

    pub fn selected(&self) -> Vec<String> {
        let mut strings = Vec::with_capacity(self.range_edits.len());

        for range_edit in self.range_edits.iter() {
            if let Some(RangeEdit { chars, .. }) = &range_edit.delete_range {
                strings.push(chars.to_owned());
            }
        }

        strings
    }

    pub fn read_at(&self, i: usize) -> Option<(Change<Option<&Cursor>>, &RangeEdits)> {
        let old_cursors = self.cursors.old.borrow_cursors();
        let new_cursors = self.cursors.new.borrow_cursors();

        let len = self.range_edits.len();
        
        if i >= len {
            return None;
        }
        
        let old = old_cursors.get(i);
        let new = new_cursors.get(i);
        
        Some(
            (Change {old, new}, &self.range_edits[i])
        )
    }

    // TODO write a test that fails when we add a new field that isn't counted here.
    // A compile-time assert would be preferable, of course.
    #[perf_viz::record]
    pub fn size_in_bytes(&self) -> usize {
        use core::mem;
        
        let mut output = 0;

        for edit in self.range_edits.iter() {
            output += edit.size_in_bytes();
        }

        output += (
            self.range_edits.capacity() -
            // Don't double count the struct bytes from the `edit.size_in_bytes()`
            // calls above.
            self.range_edits.len()
        ) * mem::size_of::<RangeEdits>();

        output += self.cursors.old.size_in_bytes();
        output += self.cursors.new.size_in_bytes();

        output
    }
}

impl From<Change<Cursors>> for Edit {
    fn from(cursors: Change<Cursors>) -> Edit {
        Edit {
            range_edits: cursors.new.mapped_ref(|_| d!()),
            cursors,
        }
    }
}

impl std::ops::Not for Edit {
    type Output = Edit;

    fn not(self) -> Self::Output {
        let reversed = self
            .range_edits
            .mapped(|r_e| !r_e)
            .into_vec()
            .into_iter()
            .rev()
            .collect::<Vec<_>>();
        Edit {
            // This unwrap is fine because `self.range_edits` was a `Vec1`.
            range_edits: Vec1::try_from_vec(reversed).unwrap(),
            cursors: !self.cursors,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RangeEdit {
    pub chars: String,
    pub range: AbsoluteCharOffsetRange,
}

impl RangeEdit {
    // TODO write a test that fails when we add a new field that isn't counted here.
    // A compile-time assert would be preferable, of course.
    pub fn size_in_bytes(&self) -> usize {
        use core::mem;

        mem::size_of::<String>() +
        self.chars.len() +
        mem::size_of_val(&self.range)
    }
}

/// Some seemingly redundant information is stored here, for example the insert's range's maximum
/// is never read. But that information is needed if the edits are ever negated, which swaps the
/// insert and delete ranges.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct RangeEdits {
    /// The characters to insert and a range that only the minimum of which is used as the
    /// insertion point. Applied second.
    pub insert_range: Option<RangeEdit>,
    /// The characters to delete and where to delete them from. Applied first.
    pub delete_range: Option<RangeEdit>,
}

impl RangeEdits {
    // TODO write a test that fails when we add a new field that isn't counted here.
    // A compile-time assert would be preferable, of course.
    pub fn size_in_bytes(&self) -> usize {
        use core::mem;

        let mut output = 0;

        output += mem::size_of_val(&self.insert_range);
        if let Some(ref r) = self.insert_range {
            // Don't double count the stack size of a RangeEdit
            output -= mem::size_of::<RangeEdit>();
            output += r.size_in_bytes();
        }

        output += mem::size_of_val(&self.delete_range);
        if let Some(ref r) = self.delete_range {
            // Don't double count the stack size of a RangeEdit
            output -= mem::size_of::<RangeEdit>();
            output += r.size_in_bytes();
        }

        output
    }
}

impl std::ops::Not for RangeEdits {
    type Output = RangeEdits;

    fn not(self) -> Self::Output {
        RangeEdits {
            insert_range: self.delete_range,
            delete_range: self.insert_range,
        }
    }
}

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Change<T> {
    pub old: T,
    pub new: T,
}

impl<T> std::ops::Not for Change<T> {
    type Output = Change<T>;

    fn not(self) -> Self::Output {
        let Change { old, new } = self;

        Change { old: new, new: old }
    }
}

#[macro_export]
macro_rules! change {
    //
    // Pattern matching
    //
    ($old: ident, $new: ident) => {
        Change { old: $old, new: $new }
    };
    (_, $new: ident) => {
        Change { old: _, new: $new }
    };
    ($old: ident, _) => {
        Change { old: $old, new: _ }
    };
    //
    // Initialization
    //
    ($old: expr, $new: expr) => {
        Change { old: $old, new: $new }
    };
}

/// returns a `RangeEdit` representing the deletion. and the relevant cursor 
/// positioning info
fn delete_within_range(
    rope: &mut Rope,
    range: AbsoluteCharOffsetRange,
) -> (RangeEdit, AbsoluteCharOffset, isize) {
    let chars = copy_string(rope, range);

    let remove_option = rope.remove(range.range());
    if cfg!(feature = "invariant-checking") {
        let range = range.range();
        remove_option.expect(&format!("range {range:?} was invalid!"));
    }

    let min = range.min();
    let max = range.max();
    (
        RangeEdit { chars, range },
        max,
        min.0 as isize - max.0 as isize,
    )
}

pub fn line_indicies_touched_by(
    rope: &Rope,
    range: AbsoluteCharOffsetRange,
) -> Option<Vec1<LineIndex>> {
    let min_index = rope.char_to_line(range.min())?;
    let max_index = rope.char_to_line(range.max())?;

    let mut output = Vec1::new(min_index);

    if min_index == max_index {
        return Some(output);
    }

    output.extend((min_index.0 + 1..=max_index.0).map(LineIndex));
    Some(output)
}

pub fn copy_string(rope: &Rope, range: AbsoluteCharOffsetRange) -> String {
    rope.slice(range.range())
        .map(|slice| {
            let s: String = slice.into();
            s
        })
        .unwrap_or_default()
}

/// Short for Rope and Cursors.
pub trait RC {
    fn rc(&self) -> (&Rope, &Cursors);
}

impl RC for (&Rope, &Cursors) {
    fn rc(&self) -> (&Rope, &Cursors) {
        (self.0, self.1)
    }
}

mod cursored_rope {
    use cursors::{Cursors, set_cursors};
    use panic_safe_rope::Rope;
    use crate::{RangeEdit, RangeEdits, RC};
    use editor_types::Cursor;
    use platform_types::Vec1;

    /// We keep the fields private so we can ensure that the cursors are always 
    /// within the rope's bounds.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct CursoredRope {
        rope: Rope,
        cursors: Cursors,
    }

    impl <R: Into<Rope>> From<R> for CursoredRope {
        fn from(into_rope: R) -> Self {
            let rope = into_rope.into();
            Self {
                rope,
                cursors: <_>::default(),
            }
        }
    }
    
    impl CursoredRope {
        pub fn new(
            rope: Rope,
            cursors: Cursors,
        ) -> Self {
            Self { rope, cursors }
        }

        #[cfg(any(test, feature = "pub_arb"))]
        pub fn split(self) -> (Rope, Cursors) {
            (self.rope, self.cursors)
        }

        pub fn set_cursors(&mut self, cursors: Cursors) {
            set_cursors(&self.rope, &mut self.cursors, cursors)
        }

        pub fn borrow_rope(&self) -> &Rope {
            &self.rope
        }

        pub fn borrow_cursors(&self) -> &Cursors {
            &self.cursors
        }

        pub fn borrow_tuple(&self) -> (&Rope, &Cursors) {
            (&self.rope, &self.cursors)
        }

        pub fn reset_cursor_states(&mut self) {
            self.cursors.reset_states()
        }

        #[cfg(any(test, feature = "pub_arb"))]
        pub fn set_cursors_from_vec1(&mut self, cursors: Vec1<Cursor>) {
            self.cursors = Cursors::new(&self.rope, cursors);
        }

        pub fn apply(&mut self, edits: &RangeEdits) {
            if let Some(RangeEdit { range, .. }) = edits.delete_range {
                let remove_option = self.rope.remove(range.range());
                if cfg!(feature = "invariant-checking") {
                    let range = range.range();
                    remove_option.expect(&format!("range {range:?} was invalid!"));
                }
            }
    
            if let Some(RangeEdit {
                ref chars, range, ..
            }) = edits.insert_range
            {
                let offset = range.min();
                let insert_option = self.rope.insert(offset, chars);
                if cfg!(feature = "invariant-checking") {
                    insert_option.expect(&format!("offset {offset} was invalid!"));
                }
            }
        }
    }

    impl RC for &CursoredRope {
        fn rc(&self) -> (&Rope, &Cursors) {
            self.borrow_tuple()
        }
    }
}
pub use cursored_rope::CursoredRope;

#[cfg(any(test, feature = "pub_arb"))]
pub mod tests {
    #[cfg(any(test, feature = "pub_arb"))]
    pub mod arb;

    #[cfg(test)]
    mod strip_trailing_whitespace_step_returns_the_expected_result {
        use crate::*;

        // Short for assert. We can be this brief becasue this is specific to this 
        // module
        macro_rules! a {
            ($from: literal $to: literal) => {
                let rope = Rope::from($from);

                let line: RopeLine = rope.line(LineIndex::default()).unwrap();
                let rel_sel = RelativeSelected {
                    line_end: final_non_newline_offset_for_rope_line(line),
                    slice_end: line.len_chars(),
                };
                let mut chars = String::new();
    
                strip_trailing_whitespace_step(
                    line,
                    rel_sel,
                    &mut chars,
                );
                
                assert_eq!(chars, $to);
            }
        }

        #[test]
        fn on_the_empty_line() {
            a!("" "");
        }

        #[test]
        fn on_the_empty_line_with_a_newline() {
            a!("\n" "\n");
        }

        #[test]
        fn on_a_line_with_a_trailing_space() {
            a!(" \n" "\n");
        }

        #[test]
        fn on_a_line_with_4_trailing_spaces() {
            a!("    \n" "\n");
        }

        #[test]
        fn on_a_line_with_a_non_white_space_then_4_trailing_spaces() {
            a!("a    \n" "a\n");
        }

        #[test]
        fn on_a_line_with_two_non_white_space_then_4_trailing_spaces() {
            a!("ab    \n" "ab\n");
        }

        #[test]
        fn on_a_line_with_two_non_white_space_then_4_trailing_spaces_no_nl() {
            a!("ab    " "ab");
        }
    }
}
