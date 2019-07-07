use super::*;

pub fn or_clear_highlights(rope: &Rope, cursor: &mut Cursor, r#move: Move) {
    if let Some(p) = cursor.get_highlight_position() {
        use std::cmp::{max, min};
        match r#move {
            Move::Up | Move::Left => {
                //we might need to clear the highlight_position and set the cursor state
                cursor.set_position(min(p, cursor.get_position()));
            }
            Move::Down | Move::Right => {
                // see above comment
                cursor.set_position(max(p, cursor.get_position()));
            }
            Move::ToLineStart
            | Move::ToBufferStart
            | Move::ToLineEnd
            | Move::ToBufferEnd => {
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
    dbg!(("directly_custom", rope, cursor, r#move));
}
pub fn directly_custom(
    rope: &Rope,
    cursor: &mut Cursor,
    r#move: Move,
    action: SetPositionAction,
) {
    let new_state = match r#move {
        Move::Up => move_up(rope, cursor, action),
        Move::Down => move_down(rope, cursor, action),
        Move::Left => move_left(rope, cursor, action),
        Move::Right => move_right(rope, cursor, action),
        Move::ToLineStart => move_to_line_start(rope, cursor, action),
        Move::ToLineEnd => move_to_line_end(rope, cursor, action),
        Move::ToBufferStart => move_to_rope_start(rope, cursor, action),
        Move::ToBufferEnd => move_to_rope_end(rope, cursor, action),
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
fn move_to(
    rope: &Rope,
    cursor: &mut Cursor,
    position: Position,
    action: SetPositionAction,
) -> Moved {
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
    if let Some(new_pos) = backward(rope, cursor.get_position()) {
        move_to(rope, cursor, new_pos, action)
    } else {
        Moved::No
    }
}
#[perf_viz::record]
fn move_right(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    if let Some(new_pos) = forward(rope, cursor.get_position()) {
        move_to(rope, cursor, new_pos, action)
    } else {
        Moved::No
    }
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
    if let Some(offset) = nth_line_count(rope, line) {
        let mut new_position = Position { line, offset };
        if !in_cursor_bounds(rope, new_position) {
            new_position = backward(rope, new_position).unwrap_or_default();
        }
        move_to(rope, cursor, new_position, action)
    } else {
        Moved::No
    }
}
#[perf_viz::record]
fn move_to_rope_start(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    // The default is the first position, and the first position is always there.
    move_to(rope, cursor, d!(), action)
}
#[perf_viz::record]
fn move_to_rope_end(rope: &Rope, cursor: &mut Cursor, action: SetPositionAction) -> Moved {
    dbg!("move_to_rope_end");
    if let Some((line, offset)) = last_line_index_and_count(rope) {
        let new_position = Position { line, offset };
        move_to(rope, cursor, new_position, action)
    } else {
        Moved::No
    }
}
