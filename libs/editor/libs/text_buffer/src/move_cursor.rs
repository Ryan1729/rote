// this module is inside `text_buffer`
use super::{
    char_offset_to_pos, final_non_newline_offset_for_rope_line, in_cursor_bounds, max, min,
    nearest_valid_position_on_same_line,
};
use editor_types::{Cursor, SetPositionAction};
use macros::d;
use panic_safe_rope::{LineIndex, Rope, RopeLine, RopeSliceTrait};
use platform_types::*;
use std::borrow::{Borrow, Cow};

use lazy_static::lazy_static;
use regex::Regex;

pub fn or_clear_highlights(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
    if let Some(p) = cursor.get_highlight_position() {
        use Move::*;
        match r#move {
            Up | Left | ToPreviousLikelyEditLocation => {
                //we might need to clear the highlight_position and set the cursor state
                cursor.set_position(min(p, cursor.get_position()));
            }
            Down | Right | ToNextLikelyEditLocation => {
                // see above comment
                cursor.set_position(max(p, cursor.get_position()));
            }
            ToLineStart | ToBufferStart | ToLineEnd | ToBufferEnd => {
                directly(rope, cursor, r#move);
                cursor.state = d!();
            }
        };
    } else {
        directly(rope, cursor, r#move);
    }
}

pub fn and_extend_selection(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
    directly_custom(
        rope,
        cursor,
        r#move,
        SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition,
    );
}

pub fn directly(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
    directly_custom(rope, cursor, r#move, SetPositionAction::ClearHighlight);
}
pub fn directly_custom(rope: &Rope, cursor: &mut Cursor, r#move: Move, action: SetPositionAction) {
    use Move::*;
    let moved = match r#move {
        Up => move_up(rope, cursor, action),
        Down => move_down(rope, cursor, action),
        Left => move_left(rope, cursor, action),
        Right => move_right(rope, cursor, action),
        ToLineStart => move_to_line_start(rope, cursor, action),
        ToLineEnd => move_to_line_end(rope, cursor, action),
        ToBufferStart => move_to_rope_start(rope, cursor, action),
        ToBufferEnd => move_to_rope_end(rope, cursor, action),
        ToPreviousLikelyEditLocation => move_to_previous_likely_edit_location(rope, cursor, action),
        ToNextLikelyEditLocation => move_to_next_likely_edit_location(rope, cursor, action),
    };

    cursor.state = state_from_moved(moved);
}

fn state_from_moved(moved: Moved) -> CursorState {
    match moved {
        Moved::No => CursorState::PressedAgainstWall,
        Moved::Yes => CursorState::None,
    }
}

enum Moved {
    No,
    Yes,
}

#[perf_viz::record]
fn move_to<OptionPos: Into<Option<Position>>>(
    rope: &Rope,
    cursor: &mut Cursor,
    position: OptionPos,
    action: SetPositionAction,
) -> Moved {
    if let Some(position) = position.into() {
        if cursor.get_position() == position {
            // We might need to clear the highlight cursor, depending on the action, even though
            // the position matches.
            cursor.set_position_custom(position, action);
        } else if in_cursor_bounds(rope, &position) {
            cursor.set_position_custom(position, action);

            // Remember this offset so that we can try
            // to maintain it when moving across lines.
            cursor.sticky_offset = position.offset;

            return Moved::Yes;
        }
    }

    Moved::No
}

/// Try moving to the same offset on the line below, falling back to its EOL.
#[perf_viz::record]
fn move_to_with_fallback(
    rope: &Rope,
    cursor: &mut Cursor,
    new_position: Position,
    action: SetPositionAction,
) -> Moved {
    let mut output = move_to(rope, cursor, new_position, action);
    if let Moved::No = output {
        if let Some(fallback_position) = nearest_valid_position_on_same_line(rope, new_position) {
            output = move_to(rope, cursor, fallback_position, action);

            // Moving the position successfully updates the sticky offset, but we
            // haven't actually moved to where we really wanted to go (offset-wise).
            // Restore the original desired offset; it might be available on the next try.
            cursor.sticky_offset = new_position.offset;
        }
    }
    output
}

#[perf_viz::record]
fn move_up(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    let pos = cursor.get_position();
    // Don't bother if we are already at the top.
    if pos.line == 0 {
        return Moved::No;
    }

    let target_line = pos.line - 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };
    move_to_with_fallback(rope, cursor, new_position, action)
}

#[perf_viz::record]
fn move_down(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    let target_line = cursor.get_position().line + 1;
    let new_position = Position {
        line: target_line,
        offset: cursor.sticky_offset,
    };

    move_to_with_fallback(rope, cursor, new_position, action)
}
#[perf_viz::record]
fn move_left(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    move_to(rope, cursor, backward(rope, cursor.get_position()), action)
}
#[perf_viz::record]
fn move_right(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    move_to(rope, cursor, forward(rope, cursor.get_position()), action)
}
#[perf_viz::record]
fn move_to_line_start(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    move_to(
        rope,
        cursor,
        Position {
            offset: d!(),
            ..cursor.get_position()
        },
        action,
    )
}
#[perf_viz::record]
fn move_to_line_end(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    let line = cursor.get_position().line;

    let option_pos = nth_line_count(rope, line).map(|offset| {
        let mut new_position = Position { line, offset };
        if !in_cursor_bounds(rope, new_position) {
            new_position = backward(rope, new_position).unwrap_or_default();
        }
        new_position
    });

    move_to(rope, cursor, option_pos, action)
}
#[perf_viz::record]
fn move_to_rope_start(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    // The default is the first position, and the first position is always there.
    let position: Position = d!();
    move_to(rope, cursor, position, action)
}
#[perf_viz::record]
fn move_to_rope_end(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    move_to(rope, cursor, last_position(rope), action)
}

pub fn get_previous_selection_point(rope: &Rope, position: Position) -> Option<Position> {
    get_previous_position(rope, position, OffsetKind::SelectionPoint)
}

fn get_previous_position(
    rope: &Rope,
    Position { line, offset }: Position,
    kind: OffsetKind,
) -> Option<Position> {
    let line_index_and_section = {
        let line_index = LineIndex(line);

        rope.line(line_index).and_then(|rope_line| {
            if offset == 0 {
                // We want to be able to move to the previous line if possible
                line_index
                    .checked_sub_one()
                    .and_then(|i| rope.line(i).map(|l| (i, l)))
            } else {
                rope_line.slice(..offset).map(|l| (line_index, l))
            }
        })
    };

    line_index_and_section.and_then(|(line_index, section)| {
        get_offsets(section, kind, IncludeStringLength::No)
            .last()
            .map(|offset| Position {
                line: line_index.0,
                offset,
            })
    })
}

#[perf_viz::record]
fn move_to_previous_likely_edit_location(
    rope: &Rope,
    cursor: &mut Cursor,
    action: SetPositionAction,
) -> Moved {
    move_to(
        rope,
        cursor,
        get_previous_position(rope, cursor.get_position(), OffsetKind::LikelyEditLocation),
        action,
    )
}

pub fn get_next_selection_point(rope: &Rope, position: Position) -> Option<Position> {
    get_next_position(rope, position, OffsetKind::SelectionPoint)
}

#[derive(Clone, Copy, Debug)]
enum OffsetKind {
    LikelyEditLocation,
    SelectionPoint,
}

fn get_next_position(
    rope: &Rope,
    Position { line, offset }: Position,
    kind: OffsetKind,
) -> Option<Position> {
    let line_index_and_section = {
        let line_index = LineIndex(line);
        rope.line(line_index).and_then(|rope_line| {
            Some(final_non_newline_offset_for_rope_line(rope_line))
                // try to move to the next line if there is nothing left on this one
                .filter(|&final_offset| offset < final_offset)
                .and_then(|final_offset| {
                    rope_line
                        .slice(offset..)
                        .map(|l| (line_index, offset, l, final_offset))
                })
                .or_else(|| {
                    line_index.checked_add_one().and_then(
                        // We rely on `d!()` being 0 here.
                        |i| {
                            rope.line(i).map(|l: RopeLine| {
                                (i, d!(), l, final_non_newline_offset_for_rope_line(l))
                            })
                        },
                    )
                })
        })
    };

    line_index_and_section.and_then(|(line_index, offset, section, final_offset)| {
        // The variable is needed to cause the `likely_edit_offsets` iterator to be dropped
        // at the right time.
        let output = get_offsets(section, kind, IncludeStringLength::No)
            // So we actually move if we started on a word boundary. This also causes us to
            // skip the first location on each line going forwards, but many other text
            // editors do it, and it seems plauible that the second edit location is used
            // more often than the first edit location. Making the other edit locations
            // "closer" doesn't seem like a bad thing. I guess we'll keep it.
            .skip_while(|&o| o == 0)
            .next()
            .map(|o| Position {
                line: line_index.0,
                offset: std::cmp::min(offset + o, final_offset),
            })
            .or_else(|| {
                Some(Position {
                    line: line_index.0,
                    offset: final_offset,
                })
            });

        output
    })
}

#[perf_viz::record]
fn move_to_next_likely_edit_location(
    rope: &Rope,
    cursor: &mut Cursor,
    action: SetPositionAction,
) -> Moved {
    move_to(
        rope,
        cursor,
        get_next_position(rope, cursor.get_position(), OffsetKind::LikelyEditLocation),
        action,
    )
}

// Regex Cheatsheet
//  * `\\s`        - whitespace
//  * `\\w`        - word character
//  * `[^\\w\\s]`  - not whitespace or word, aka "punctuation"

// We need mulitple regexes here because these matches might overlap, and we want all the matches
lazy_static! {
    static ref LIKELY_EDIT_REGEXES: Vec<Regex> = vec![
        Regex::new("\\w[^\\w\\s]").unwrap(),
        Regex::new("[^\\w\\s]\\w").unwrap(),
        Regex::new("\\s\\w").unwrap(),
        Regex::new("\\s[^\\w\\s]").unwrap(),
    ];
    static ref SELECTION_POINT_REGEXES: Vec<Regex> = vec![
        Regex::new("\\w[^\\w]").unwrap(),
        Regex::new("[^\\w\\s][\\w\\s]").unwrap(),
        Regex::new("\\s[^\\s]").unwrap(),
    ];
}

enum IncludeStringLength {
    No,
    Yes,
}
d!(for IncludeStringLength: IncludeStringLength::Yes);

/// The general idea here is that we split all characters into one of three groups:
/// * Word characters, as defined by the `regex` crate
/// * Whitspace characters, again as defined by the `regex` crate
/// * everything else, which we will call "Punctuation"
///
/// We condsider any time a string transitions from one of these categories to another to be a
/// selection point. Hopefully it is obviuos that you would usually want to select
/// groups of charaters all of the same group.
///
/// We consider a smaller set of transtion kinds to be likely edit locations. Hopefully it makes
/// inuitive sense that most edits happen between things of different groups of charaters rather
/// than inside of them.
///
/// This functions returns an iterator of the likely edit offsets in the given string.
/// Depending upon the value of the `IncludeStringLength` parameter, the iterator will either
/// include or omit what would be the last one which would always be the length of the string in
/// chars if included.
fn get_offsets<'line>(
    rope_line: RopeLine<'line>,
    kind: OffsetKind,
    include: IncludeStringLength,
) -> Box<dyn Iterator<Item = CharOffset> + 'line> {
    use std::iter::once;

    let output = once(CharOffset(0));

    let len = rope_line.len_chars();
    if len == 0 {
        return Box::new(output);
    }

    let s: Cow<str> = rope_line.into();

    let regexes = match kind {
        OffsetKind::LikelyEditLocation => LIKELY_EDIT_REGEXES.iter(),
        OffsetKind::SelectionPoint => SELECTION_POINT_REGEXES.iter(),
    };

    // There's a bunch of complicated ways we could try to make this algorithmically faster, like
    // making sure the regexes are maximal while being non-overlapping and taking advantage of the
    // fact that the individual streams are already sorted. But, that currently smells like
    // premature optimization to me.
    let mut matched_offsets: Vec<_> = regexes
        .flat_map(|re| {
            re.find_iter(&s).map(|m|
            // We expect only length 2 matches here, and the index we want is in the middle
            // We could divide, but in practice addition of 1 gives the right answer.
            CharOffset(m.start() + 1))
        })
        .collect();

    matched_offsets.sort();
    matched_offsets.dedup();

    match include {
        IncludeStringLength::No => Box::new(output.chain(matched_offsets.into_iter())),
        IncludeStringLength::Yes => {
            Box::new(output.chain(matched_offsets.into_iter()).chain(once(len)))
        }
    }
}

#[perf_viz::record]
pub fn to_absolute_offset(
    rope: &Rope,
    cursor: &mut Cursor,
    offset: AbsoluteCharOffset,
    action: SetPositionAction,
) {
    move_to(rope, cursor, char_offset_to_pos(rope, offset), action);
    // Moving to an absolute offset should never cause a cursor to be pressed against a wall,
    // and if it was previously pushed, it should not be anymore.
    cursor.state = d!();
}

// utils

fn nth_line_count(rope: &Rope, n: usize) -> Option<CharOffset> {
    rope.lines().nth(n).map(|l| l.len_chars().into())
}

pub fn last_position(rope: &Rope) -> Option<Position> {
    rope.lines()
        .map(|l| l.len_chars().into())
        .enumerate()
        .last()
        .map(|(line, offset)| Position { line, offset })
}

#[perf_viz::record]
pub fn backward<P>(rope: &Rope, position: P) -> Option<Position>
where
    P: Borrow<Position>,
{
    let mut position = *position.borrow();

    while {
        position = if position.offset == 0 {
            if position.line == 0 {
                return None;
            }
            let line = position.line.saturating_sub(1);
            Position {
                line,
                offset: nth_line_count(rope, line).unwrap_or_default(),
            }
        } else {
            Position {
                offset: position.offset - 1,
                ..position
            }
        };

        !in_cursor_bounds(rope, position)
    } {}

    Some(position)
}

#[perf_viz::record]
pub fn forward<P>(rope: &Rope, position: P) -> Option<Position>
where
    P: Borrow<Position>,
{
    let position = position.borrow();

    let mut new = Position {
        offset: position.offset + 1,
        ..*position
    };

    if !in_cursor_bounds(rope, &new) {
        new.line += 1;
        new.offset = d!();
    }

    if in_cursor_bounds(rope, &new) {
        Some(new)
    } else {
        None
    }
}

#[allow(dead_code)]
#[perf_viz::record]
pub fn backward_n<P>(rope: &Rope, position: P, n: usize) -> Option<Position>
where
    P: Borrow<Position>,
{
    let mut position = Some(position.borrow().clone());
    for _ in 0..n {
        position = backward(rope, position?);
    }

    return position;
}

#[allow(dead_code)]
#[perf_viz::record]
pub fn forward_n<P>(rope: &Rope, position: P, n: usize) -> Option<Position>
where
    P: Borrow<Position>,
{
    let mut position = Some(position.borrow().clone());
    for _ in 0..n {
        position = forward(rope, position?);
    }

    return position;
}

#[cfg(test)]
use super::*;

#[cfg(test)]
mod tests {
    use super::*;
    use platform_types::pos;

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

    #[test]
    fn getting_likely_edit_offsets_works_on_this_code_example() {
        let rope = r!("{[(012), (345)]}");
        let line = rope.line(LineIndex(0)).unwrap();
        let offsets: Vec<_> = get_offsets(
            line,
            OffsetKind::LikelyEditLocation,
            IncludeStringLength::Yes,
        )
        .collect();

        assert_eq!(
            offsets,
            vec![
                CharOffset(0),
                CharOffset(3),
                CharOffset(6),
                CharOffset(9),
                CharOffset(10),
                CharOffset(13),
                CharOffset(16),
            ]
        );
    }

    #[test]
    fn getting_selection_point_offsets_works_on_this_code_example() {
        let rope = r!("{[(012), (345)]}");
        let line = rope.line(LineIndex(0)).unwrap();
        let offsets: Vec<_> =
            get_offsets(line, OffsetKind::SelectionPoint, IncludeStringLength::Yes).collect();

        assert_eq!(
            offsets,
            vec![
                CharOffset(0),
                CharOffset(3),
                CharOffset(6),
                CharOffset(8),
                CharOffset(9),
                CharOffset(10),
                CharOffset(13),
                CharOffset(16),
            ]
        );
    }

    #[test]
    fn get_offsets_works_on_this_simple_example() {
        let rope = r!("(123)");
        let line = rope.line(LineIndex(0)).unwrap();

        let expected = vec![CharOffset(0), CharOffset(1), CharOffset(4), CharOffset(5)];

        let likely_edit_locations: Vec<_> = get_offsets(
            line,
            OffsetKind::LikelyEditLocation,
            IncludeStringLength::Yes,
        )
        .collect();

        assert_eq!(likely_edit_locations, expected);

        let selection_points: Vec<_> =
            get_offsets(line, OffsetKind::SelectionPoint, IncludeStringLength::Yes).collect();

        assert_eq!(selection_points, expected);
    }
}
