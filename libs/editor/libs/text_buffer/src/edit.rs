use super::*;
use crate::move_cursor::{backward_n, forward_n};
use macros::{CheckedSub, SaturatingSub};

pub fn apply(rope: &mut Rope, cursors: &mut Cursors, edit: &Edit) {
    // we assume that the edits are in the proper order so we won't mess up our indexes with our
    // own inserts and removals. I'm not positive that there being a single order that works
    // is possible for all possible edits, but in practice I think the edits we will actually
    // produce will work out. The tests should tell us if we're wrong!
    for range_edit in edit.range_edits.iter() {
        range_edit.apply(rope);
    }

    *cursors = edit.cursors.new.clone();
}

/// Calls the `FnMut` once with a copy of each cursor and a reference to the same clone of the
/// `Rope`. Then the (potentially) modified cursors and another copy of the `original_cursors`
/// are wrapped up along with the returned `RangeEdit`s into the Edit.
#[perf_viz::record]
fn get_edit<F>(original_rope: &Rope, original_cursors: &Cursors, mut mapper: F) -> Edit
where
    F: FnMut(&mut Cursor, &mut Rope, usize) -> RangeEdits,
{
    perf_viz::start_record!("init cloning + sort");
    // We need to sort cursors, so our `range_edits` are in the right order, so we can go
    // backwards, when we apply them so our indexes don't get messed up but our own inserts
    // and deletes.
    // Should we just always maintin that the cursors in sorted order?
    let mut cloned_cursors = sort_cursors(original_cursors.get_cloned_cursors());
    let mut cloned_rope = original_rope.clone();
    perf_viz::end_record!("init cloning + sort");

    // the index needs to account for the order being from the high positions to the low positions
    perf_viz::start_record!("range_edits");
    let mut index = cloned_cursors.len();
    let range_edits = cloned_cursors.mapped_mut(|c| {
        index -= 1;
        mapper(c, &mut cloned_rope, index)
    });
    perf_viz::end_record!("range_edits");

    perf_viz::record_guard!("construct result");
    Edit {
        range_edits,
        cursors: Change {
            new: Cursors::new(dbg!(cloned_cursors)),
            old: original_cursors.clone(),
        },
    }
}

fn get_standard_insert_range_edits(
    rope: &mut Rope,
    cursor: &mut Cursor,
    offset: AbsoluteCharOffset,
    chars: String,
    char_count: usize, //we take this a s a paam to support it being a const.
) -> RangeEdits {
    rope.insert(offset, &chars);
    move_cursor::right_n_times(&rope, cursor, char_count);

    let range = AbsoluteCharOffsetRange::new(offset, offset + char_count);

    RangeEdits {
        insert_range: Some(RangeEdit { chars, range }),
        ..d!()
    }
}

/// Returns an edit that, if applied, after deleting the highlighted region at each cursor if
/// there is one, inserts the given string at each of the cursors.
#[perf_viz::record]
pub fn get_insert_edit<F>(original_rope: &Rope, original_cursors: &Cursors, get_string: F) -> Edit
where
    F: Fn(usize) -> String,
{
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

                let range_edit = delete_highlighted(rope, cursor, o1, o2);

                rope.insert(range_edit.range.min(), &s);
                let char_count = s.chars().count();
                move_cursor::right_n_times(&rope, cursor, char_count);

                let range = {
                    let min = range_edit.range.min();
                    AbsoluteCharOffsetRange::new(min, min + char_count)
                };

                RangeEdits {
                    insert_range: Some(RangeEdit { chars: s, range }),
                    delete_range: Some(range_edit),
                }
            }
            _ => d!(),
        },
    )
}

/// Returns an edit that, if applied, deletes the highlighted region at each cursor if there is one.
/// Otherwise the applying the edit will delete a single character at each cursor.
pub fn get_delete_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o), None) if o > 0 => {
                // Deleting the LF ('\n') of a CRLF ("\r\n") pair is a special case
                // where the cursor should not be moved backwards. Thsi is because
                // CR ('\r') and CRLF ("\r\n") both count as a single newline.
                // TODO would it better to just delete both at once? That seems like
                // it would require a moe comlicated special case elsewhere.
                let not_deleting_lf_of_cr_lf = {
                    o.checked_sub(CharOffset(2))
                        .and_then(|two_back| {
                            let mut chars = rope.slice(two_back..o)?.chars();

                            Some((chars.next()?, chars.next()?) != ('\r', '\n'))
                        })
                        .unwrap_or(true)
                };

                let delete_offset_range = AbsoluteCharOffsetRange::new(o - 1, o);
                let chars = copy_string(&rope, delete_offset_range);
                rope.remove(delete_offset_range.range());

                if not_deleting_lf_of_cr_lf {
                    move_cursor::directly(&rope, cursor, Move::Left);
                }

                RangeEdits {
                    delete_range: Some(RangeEdit {
                        chars,
                        range: delete_offset_range,
                    }),
                    ..d!()
                }
            }
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => RangeEdits {
                delete_range: Some(delete_highlighted(rope, cursor, o1, o2)),
                ..d!()
            },
            _ => d!(),
        }
    })
}

/// returns an edit that if applied will delete the highlighted region at each cursor if there is
/// one, and which does nothing otherwise.
pub fn get_cut_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        let offsets = offset_pair(original_rope, cursor);

        match offsets {
            (Some(o1), Some(o2)) if o1 > 0 || o2 > 0 => RangeEdits {
                delete_range: Some(delete_highlighted(rope, cursor, o1, o2)),
                ..d!()
            },
            _ => d!(),
        }
    })
}

const TAB_STR: &'static str = "    "; //four spaces
const TAB_STR_CHAR: char = ' ';
const TAB_STR_CHAR_COUNT: usize = 4; // this isn't const (yet?) TAB_STR.chars().count();

#[perf_viz::record]
pub fn get_tab_in_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        match offset_pair(original_rope, cursor) {
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

                    let first_highlighted_non_white_space_offset: Option<CharOffset> =
                        get_first_non_white_space_offset_in_range(
                            line,
                            highlight_start_for_line..=highlight_end_for_line,
                        );

                    let start = highlight_start_for_line.0;
                    let previous_offset = min(
                        first_highlighted_non_white_space_offset
                            .unwrap_or(CharOffset(usize::max_value())),
                        highlight_end_for_line,
                    );
                    let end = previous_offset.0 + TAB_STR_CHAR_COUNT;
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
                    let highlighted_line_after_whitespace: &str =
                        some_or!(line.slice(o..slice_end).and_then(|l| l.as_str()), continue);
                    dbg!(highlighted_line_after_whitespace);
                    chars.push_str(highlighted_line_after_whitespace);
                }

                let range_edit = delete_within_range(rope, range);

                let range = range_edit
                    .range
                    .add_to_max(TAB_STR_CHAR_COUNT * tab_insert_count);

                rope.insert(range.min(), &chars);

                // We want the cursor to have the same orientation it started with.
                forward_n(rope, cursor.get_position(), TAB_STR_CHAR_COUNT).map(|p| {
                    cursor.set_position_custom(
                        p,
                        SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
                    );
                    cursor.sticky_offset += TAB_STR_CHAR_COUNT
                });
                cursor
                    .get_highlight_position()
                    .and_then(|p| forward_n(rope, p, TAB_STR_CHAR_COUNT))
                    .map(|p| cursor.set_highlight_position(p));

                RangeEdits {
                    insert_range: Some(RangeEdit { chars, range }),
                    delete_range: Some(range_edit),
                }
            }
            _ => d!(),
        }
    })
}

pub fn get_tab_out_edit(original_rope: &Rope, original_cursors: &Cursors) -> Edit {
    dbg!(original_cursors);
    get_edit(original_rope, original_cursors, |cursor, rope, _| {
        match offset_pair(original_rope, cursor) {
            (Some(o1), offset2) => {
                let o2 = offset2.unwrap_or(o1);
                let line_indicies = some_or!(
                    line_indicies_touched_by(rope, AbsoluteCharOffsetRange::new(o1, o2)),
                    return d!()
                );

                let last_line_indicies_index = line_indicies.len() - 1;
                let range = {
                    dbg!(&line_indicies);
                    let first_line_index = line_indicies[0];
                    let last_line_index = line_indicies[last_line_indicies_index];
                    dbg!(last_line_index);

                    AbsoluteCharOffsetRange::new(
                        some_or!(rope.line_to_char(first_line_index), return d!()),
                        some_or!(
                            rope.line(last_line_index).and_then(|line|
                                dbg!(rope
                                .line_to_char(last_line_index))
                                .map(|o|
                                    o
                                    + final_non_newline_offset_for_rope_line(line).0
                                )
                            ),
                            return d!()
                        ),
                    )
                };
                dbg!(AbsoluteCharOffsetRange::new(o1, o2), range);

                let mut chars = String::with_capacity(range.max().0 - range.min().0);
                let mut char_delete_count = 0;

                for (i, index) in line_indicies.into_iter().enumerate() {
                    let line = some_or!(rope.line(index), continue);

                    let line_start = some_or!(rope.line_to_char(index), continue);
                    let relative_line_end = final_non_newline_offset_for_rope_line(line);

                    let first_non_white_space_offset: Option<CharOffset> =
                        get_first_non_white_space_offset_in_range(line, d!()..=relative_line_end);

                    dbg!(
                        index,
                        i,
                        line,
                        line_start,
                        relative_line_end,
                        first_non_white_space_offset
                    );

                    let delete_count = min(
                        first_non_white_space_offset.unwrap_or(relative_line_end),
                        CharOffset(TAB_STR_CHAR_COUNT),
                    );
                    char_delete_count += delete_count.0;

                    let should_include_newlline = i != last_line_indicies_index;

                    let slice_end = if should_include_newlline {
                        line.len_chars()
                    } else {
                        relative_line_end
                    };

                    dbg!(delete_count, should_include_newlline, slice_end,);
                    chars.push_str(some_or!(
                        line.slice(delete_count..slice_end).and_then(|l| l.as_str()),
                        continue
                    ));
                }

                dbg!(&range);
                let delete_edit = delete_within_range(rope, range);
                dbg!(&delete_edit);

                let range = some_or!(
                    delete_edit.range.checked_sub_from_max(char_delete_count),
                    return d!()
                );

                rope.insert(range.min(), &chars);

                // We want the cursor to have the same orientation it started with.
                backward_n(rope, cursor.get_position(), TAB_STR_CHAR_COUNT).map(|p| {
                    cursor.set_position_custom(
                        p,
                        SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
                    );
                    cursor.sticky_offset = cursor.sticky_offset.saturating_sub(TAB_STR_CHAR_COUNT);
                });
                cursor
                    .get_highlight_position()
                    .and_then(|p| backward_n(rope, p, TAB_STR_CHAR_COUNT))
                    .map(|p| cursor.set_highlight_position(p));

                dbg!(RangeEdits {
                    insert_range: Some(RangeEdit { chars, range }),
                    delete_range: Some(delete_edit),
                })
            }
            (o1, o2) => {dbg!(o1, o2); d!()},
        }
    })
}

/// `range_edits` and the two `Vec1`s in `cursors` must all be the same length.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Edit {
    range_edits: Vec1<RangeEdits>,
    cursors: Change<Cursors>,
}

impl Edit {
    pub fn selected(&self) -> Vec<String> {
        let mut strings = Vec::with_capacity(self.range_edits.len());

        for range_edit in self.range_edits.iter() {
            if let Some(RangeEdit { chars, .. }) = &range_edit.delete_range {
                strings.push(chars.to_owned());
            }
        }

        strings
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

#[derive(Clone, Debug, PartialEq, Eq)]
struct RangeEdit {
    chars: String,
    range: AbsoluteCharOffsetRange,
}

/// Some seemingly redundant information is stored here, for example the insert's range's maximum
/// is never read. But that information is needed if the edits are ever negated, which swaps the
/// insert and delete ranges.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct RangeEdits {
    /// The characters to insert and a range that only the minimum of which is used as the
    /// insertion point. Applied second.
    insert_range: Option<RangeEdit>,
    /// The characters to delete and where to delete them from. Applied first.
    delete_range: Option<RangeEdit>,
}

impl RangeEdits {
    fn apply(&self, rope: &mut Rope) {
        if let Some(RangeEdit { range, .. }) = self.delete_range {
            rope.remove(range.range());
        }

        if let Some(RangeEdit {
            ref chars, range, ..
        }) = self.insert_range
        {
            rope.insert(range.min(), chars);
        }
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

#[derive(Clone, Default, Debug, PartialEq, Eq)]
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

mod absolute_char_offset_range {
    use super::*;

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub struct AbsoluteCharOffsetRange {
        min: AbsoluteCharOffset,
        max: AbsoluteCharOffset,
    }

    impl AbsoluteCharOffsetRange {
        pub fn new(o1: AbsoluteCharOffset, o2: AbsoluteCharOffset) -> Self {
            let min = std::cmp::min(o1, o2);
            let max = std::cmp::max(o1, o2);

            AbsoluteCharOffsetRange { min, max }
        }

        pub fn range(&self) -> std::ops::Range<AbsoluteCharOffset> {
            self.min..self.max
        }

        pub fn min(&self) -> AbsoluteCharOffset {
            self.min
        }

        pub fn max(&self) -> AbsoluteCharOffset {
            self.max
        }

        #[allow(dead_code)]
        pub fn add_to_min<A>(&self, min: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Add<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min + min, self.max)
        }

        pub fn add_to_max<A>(&self, max: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Add<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min, self.max + max)
        }

        #[allow(dead_code)]
        pub fn sub_from_min<A>(&self, min: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Sub<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min - min, self.max)
        }

        #[allow(dead_code)]
        pub fn sub_from_max<A>(&self, max: A) -> Self
        where
            AbsoluteCharOffset: std::ops::Sub<A, Output = AbsoluteCharOffset>,
        {
            Self::new(self.min, self.max - max)
        }

        #[allow(dead_code)]
        pub fn checked_sub_from_min<A>(&self, min: A) -> Option<Self>
        where
            AbsoluteCharOffset: macros::CheckedSub<A, Output = AbsoluteCharOffset>,
        {
            self.min
                .checked_sub(min)
                .map(|min| Self::new(min, self.max))
        }

        pub fn checked_sub_from_max<A>(&self, max: A) -> Option<Self>
        where
            AbsoluteCharOffset: macros::CheckedSub<A, Output = AbsoluteCharOffset>,
        {
            self.max
                .checked_sub(max)
                .map(|max| Self::new(self.min, max))
        }
    }
}
use absolute_char_offset_range::AbsoluteCharOffsetRange;

#[perf_viz::record]
fn sort_cursors(cursors: Vec1<Cursor>) -> Vec1<Cursor> {
    let mut cursors = cursors.to_vec();

    cursors.sort();
    cursors.reverse();

    // This unwrap is fine because we knew it was a Vec1 at the start.
    Vec1::try_from_vec(cursors).unwrap()
}

/// returns a `RangeEdit` representing the deletion.
fn delete_highlighted(
    rope: &mut Rope,
    cursor: &mut Cursor,
    o1: AbsoluteCharOffset,
    o2: AbsoluteCharOffset,
) -> RangeEdit {
    let range = AbsoluteCharOffsetRange::new(o1, o2);

    let output = delete_within_range(rope, range);

    cursor.set_position(char_offset_to_pos(&rope, range.min()).unwrap_or_default());

    output
}

/// returns the same thing as `delete_highlighted`
fn delete_within_range(rope: &mut Rope, range: AbsoluteCharOffsetRange) -> RangeEdit {
    let chars = copy_string(rope, range);

    rope.remove(range.range());

    RangeEdit { chars, range }
}

fn line_indicies_touched_by(
    rope: &Rope,
    range: AbsoluteCharOffsetRange,
) -> Option<Vec1<LineIndex>> {
    dbg!(rope, range);
    let min_index = rope.char_to_line(range.min())?;
    let max_index = rope.char_to_line(range.max())?;
    dbg!();

    let mut output = Vec1::new(min_index);

    if min_index == max_index {
        return Some(output);
    }

    output.extend((min_index.0 + 1..=max_index.0).map(LineIndex));
    Some(output)
}

fn get_first_non_white_space_offset_in_range<R: std::ops::RangeBounds<CharOffset>>(
    line: RopeLine,
    range: R,
) -> Option<CharOffset> {
    use std::ops::Bound::*;

    let skip = match range.start_bound() {
        Included(CharOffset(o)) => *o,
        Excluded(CharOffset(o)) => (*o).saturating_add(1),
        Unbounded => 0,
    };

    let take = match range.end_bound() {
        Included(CharOffset(o)) => *o,
        Excluded(CharOffset(o)) => (*o).checked_sub(1)?,
        Unbounded => usize::max_value(),
    } - skip;
    let final_offset = final_non_newline_offset_for_rope_line(line);

    for (i, c) in line
        .chars()
        .enumerate()
        .map(|(i, c)| (CharOffset(i), c))
        .skip(skip)
        .take(std::cmp::min(take, final_offset.0))
    {
        if !c.is_whitespace() {
            return Some(i);
        }

        if i + 1 > final_offset {
            break;
        }
    }

    None
}

fn copy_string(rope: &Rope, range: AbsoluteCharOffsetRange) -> String {
    rope.slice(range.range())
        .map(|slice| {
            let s: String = slice.into();
            s
        })
        .unwrap_or_default()
}

#[cfg(test)]
mod edit_tests;
