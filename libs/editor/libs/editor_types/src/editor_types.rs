use macros::{d, fmt_debug, fmt_display, format_if, ord};
pub use platform_types::{AbsoluteCharOffset, CharOffset, CursorState, Position};
use std::borrow::Borrow;

#[derive(Clone, Copy, Debug)]
pub enum SetPositionAction {
    ClearHighlight,
    ClearHighlightOnlyIfItMatchesNewPosition,
    OldPositionBecomesHighlightIfItIsNone,
}
d!(for SetPositionAction: SetPositionAction::ClearHighlight);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Cursor {
    // These are private so we can make sure whether to clear highlight or not 
    // is considered on each mutation of `position. And we can use a state we 
    // don't otherwise want,
    // `highlight_position == Some(position)` to represent a state we do:
    // `highlight_position == None`.
    position: Position,
    highlight_position: Position,
    pub sticky_offset: CharOffset,
    pub state: CursorState,
}

ord!(for Cursor: c, other in {
    // We don't really have a preferred ordering for ranges with the same start and
    // end. So we treat two cursors where one's position is the other's 
    // highlight_position and vice-versa as equal.
    let min = std::cmp::min(c.position, c.highlight_position);
    let max = std::cmp::max(c.position, c.highlight_position);

    let other_min = std::cmp::min(other.position, other.highlight_position);
    let other_max = std::cmp::max(other.position, other.highlight_position);

    min.cmp(&other_min)
        .then_with(|| max.cmp(&other_max))
        .then_with(|| c.sticky_offset.cmp(&other.sticky_offset))
        .then_with(|| c.state.cmp(&other.state))
});

impl Cursor {
    #[must_use]
    pub fn new(position: Position) -> Self {
        Self::new_with_highlight(position, position)
    }
    #[must_use]
    pub fn new_with_highlight(position: Position, highlight_position: Position) -> Self {
        Cursor {
            position,
            sticky_offset: position.offset,
            highlight_position,
            state: d!(),
        }
    }

    #[must_use]
    pub fn get_position(&self) -> Position {
        self.position
    }

    pub fn set_position(&mut self, position: Position) {
        self.set_position_custom(position, d!());
    }

    #[perf_viz::record]
    pub fn set_position_custom(&mut self, position: Position, action: SetPositionAction) {
        match action {
            SetPositionAction::ClearHighlight => {
                self.highlight_position = position;
            }
            SetPositionAction::ClearHighlightOnlyIfItMatchesNewPosition => {}
            SetPositionAction::OldPositionBecomesHighlightIfItIsNone => {
                if self.get_highlight_position().is_none() {
                    self.highlight_position = self.position;
                }
            }
        }
        self.position = position;
    }

    #[must_use]
    pub fn get_highlight_position(&self) -> Option<Position> {
        Some(self.highlight_position).filter(|&p| p != self.position)
    }

    /// Equivalent to `c.get_highlight_position().unwrap_or(c.get_position())`
    #[must_use]
    pub fn get_highlight_position_or_position(&self) -> Position {
        self.highlight_position
    }

    pub fn set_highlight_position<P: Into<Option<Position>>>(&mut self, position: P) {
        self.highlight_position = position.into().unwrap_or(self.position);
    }
}

// TODO generate all match arms that fit the pattern in a proc macro or something.
// For now we can just add ones as neede.
#[macro_export]
macro_rules! cur {
    ($pos: expr) => {
        Cursor::new($pos)
    };
    ($pos: expr, $h_pos: expr) => {
        Cursor::new_with_highlight($pos, $h_pos)
    };
    (l $line:literal o $offset:literal) => {
        Cursor::new(pos! {l $line o $offset})
    };
    (l $line:literal o $offset:literal s_o $sticky_offset:literal) => {{
        let mut c = Cursor::new(pos! {l $line o $offset});
        c.sticky_offset = CharOffset($sticky_offset);
        c
    }};
    (l $line:literal o $offset:literal ->|($move: expr)) => {
        cur!(->|($move) cursor: Cursor::new(pos! {l $line o $offset}))
    };
    (l $line:literal o $offset:literal h l $h_line:literal o $h_offset:literal) => {
        Cursor::new_with_highlight(pos! {l $line o $offset}, pos! {l $h_line o $h_offset})
    };
    (l $line:literal o $offset:literal h l $h_line:literal o $h_offset:literal ->|($move: expr)) => {
        cur!(->|($move) cursor: Cursor::new_with_highlight(pos! {l $line o $offset}, pos! {l $h_line o $h_offset}))
    };
    (l $line:literal o $offset:literal h $h_pos: expr) => {
        Cursor::new_with_highlight(pos! {l $line o $offset}, $h_pos)
    };
    (l $line:literal o $offset:literal h $h_pos: expr, ->|($move: expr)) => {
        cur!(->|($move) cursor: Cursor::new_with_highlight(pos! {l $line o $offset}, $h_pos))
    };
    (h l $h_line:literal o $h_offset:literal) => {
        Cursor::new_with_highlight(pos! {}, pos! {l $h_line o $h_offset})
    };
    (h l $h_line:literal o $h_offset:literal ->|($move: expr)) => {
        cur!(->|($move) cursor: Cursor::new_with_highlight(pos! {}, pos! {l $h_line o $h_offset}))
    };
    (l $line:literal o $offset:literal h) => {
        Cursor::new_with_highlight(pos! {l $line o $offset}, pos! {})
    };
    (l $line:literal o $offset:literal h ->|($move: expr)) => {
        cur!(->|($move) cursor: Cursor::new_with_highlight(pos! {l $line o $offset}, pos! {}))
    };
    (->|($move: expr) cursor: $cursor: expr) => {{
        let mut c = $cursor;
        c.state = CursorState::PressedAgainstWall($move);
        c
    }};
    (->|($move: expr)) => {
        cur!(->|($move) cursor: Cursor::new(pos! {}))
    };
    () => {
        Cursor::new(pos! {})
    };
}

fmt_display! {
    for Cursor : Cursor {
            position,
            sticky_offset,
            highlight_position,
            ..
        } in "{}{}{}",
        position,
        format_if!(*sticky_offset != position.offset, "({})", sticky_offset),
        format_if!(highlight_position != position, "h:{}", highlight_position)
}
fmt_debug!(for Cursor : Cursor {
            position,
            sticky_offset,
            highlight_position,
            state
        } in "cur!{{l {} o {}{}{}{}}}",
        position.line,
        position.offset,
        format_if!(
            highlight_position != position, 
            " h l {} o {}", 
            highlight_position.line,  
            highlight_position.offset
        ),
        format_if!(
            sticky_offset != &Cursor::new(*position).sticky_offset,
            " s_o {}",
            sticky_offset
        ),
        format_if!(
            state != &CursorState::default(),
            " state: {:?},",
            state
        ),
);
d!(for Cursor: Cursor::new(d!()));

impl Borrow<Position> for Cursor {
    fn borrow(&self) -> &Position {
        &self.position
    }
}
impl Borrow<Position> for &Cursor {
    fn borrow(&self) -> &Position {
        &self.position
    }
}

impl From<Position> for Cursor {
    fn from(p: Position) -> Self {
        Cursor::new(p)
    }
}

impl From<&Position> for Cursor {
    fn from(p: &Position) -> Self {
        Cursor::new(*p)
    }
}

impl From<(Position, Position)> for Cursor {
    fn from((p, h): (Position, Position)) -> Self {
        Cursor::new_with_highlight(p, h)
    }
}

impl From<&(Position, Position)> for Cursor {
    fn from(&(p, h): &(Position, Position)) -> Self {
        Cursor::new_with_highlight(p, h)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use platform_types::pos;
    #[test]
    fn these_two_cursors_are_sorted_equally() {
        use std::cmp::Ordering::Equal;
        let mut c1 = Cursor::new(pos! {l 1 o 2});
        c1.set_highlight_position(pos! {l 3 o 4});
        c1.sticky_offset = d!();

        let mut c2 = Cursor::new(pos! {l 3 o 4});
        c2.set_highlight_position(pos! {l 1 o 2});
        c2.sticky_offset = d!();

        assert_eq!(c1.cmp(&c2), Equal);
        assert_eq!(c2.cmp(&c1), Equal);
    }
}
