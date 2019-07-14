// this module is inside `text_buffer`
use super::*;

pub fn or_clear_highlights(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
    if let Some(p) = cursor.get_highlight_position() {
        use std::cmp::{max, min};
        use Move::*;
        match r#move {
            Up | Left | ToPreviousWordBoundary => {
                //we might need to clear the highlight_position and set the cursor state
                cursor.set_position(min(p, cursor.get_position()));
            }
            Down | Right | ToNextWordBoundary => {
                // see above comment
                cursor.set_position(max(p, cursor.get_position()));
            }
            ToLineStart | ToBufferStart | ToLineEnd | ToBufferEnd => {
                move_cursor::directly(rope, cursor, r#move);
                cursor.state = d!();
            }
        };
    } else {
        move_cursor::directly(rope, cursor, r#move);
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
    let new_state = match r#move {
        Up => move_up(rope, cursor, action),
        Down => move_down(rope, cursor, action),
        Left => move_left(rope, cursor, action),
        Right => move_right(rope, cursor, action),
        ToLineStart => move_to_line_start(rope, cursor, action),
        ToLineEnd => move_to_line_end(rope, cursor, action),
        ToBufferStart => move_to_rope_start(rope, cursor, action),
        ToBufferEnd => move_to_rope_end(rope, cursor, action),
        ToPreviousWordBoundary => move_to_previous_word_boundary(rope, cursor, action),
        ToNextWordBoundary => move_to_next_word_boundary(rope, cursor, action),
    };

    cursor.state = match new_state {
        Moved::No => CursorState::PressedAgainstWall,
        Moved::Yes => CursorState::None,
    };
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
            // the postion matches.
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
    let target_line = new_position.line;
    let mut output = move_to(rope, cursor, new_position, action);
    if let Moved::No = output {
        let mut target_offset = d!();
        if let Some(count) = nth_line_count(rope, target_line) {
            target_offset = count;
        }
        output = move_to(
            rope,
            cursor,
            Position {
                line: target_line,
                offset: target_offset,
            },
            action,
        );

        // Moving the position successfully updates the sticky offset, but we
        // haven't actually moved to where we really wanted to go (offset-wise).
        // Restore the original desired offset; it might be available on the next try.
        cursor.sticky_offset = new_position.offset;
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
    move_to(rope, cursor, Some(d!()), action)
}
#[perf_viz::record]
fn move_to_rope_end(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    move_to(rope, cursor, last_position(rope), action)
}

// lazy_static! {
//     static ref WORD_BOUNDARY: Regex = Regex::new("\\b").unwrap();
// }

#[perf_viz::record]
fn move_to_previous_word_boundary(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    unimplemented!()
}
#[perf_viz::record]
fn move_to_next_word_boundary(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    unimplemented!()
}

// utils

fn nth_line_count(rope: &Rope, n: usize) -> Option<CharOffset> {
    rope.lines().nth(n).map(|l| CharOffset(l.len_chars()))
}

fn last_position(rope: &Rope) -> Option<Position> {
    rope.lines()
        .map(|l| CharOffset(l.len_chars()))
        .enumerate()
        .last()
        .map(|(line, offset)| Position { line, offset })
}

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
}
