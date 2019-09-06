use macros::{d, fmt_display, ord};
use platform_types::{CharOffset, Position};
use std::borrow::Borrow;
pub use vec1::{vec1, Vec1};

#[derive(Clone, Debug)]
pub enum CursorState {
    None,
    PressedAgainstWall,
}
d!(for CursorState: CursorState::None);

ord!(and friends for CursorState: state, other in {
    use std::cmp::Ordering::*;
    match (state, other) {
        (CursorState::None, CursorState::None) | (CursorState::PressedAgainstWall, CursorState::PressedAgainstWall) => Equal,
        (CursorState::None, CursorState::PressedAgainstWall) => Less,
        (CursorState::PressedAgainstWall, CursorState::None) => Greater
    }
});

#[derive(Clone, Copy, Debug)]
pub enum SetPositionAction {
    ClearHighlight,
    ClearHighlightOnlyIfItMatchesNewPosition,
    OldPositionBecomesHighlightIfItIsNone,
}
d!(for SetPositionAction: SetPositionAction::ClearHighlight);

#[derive(Clone, Debug)]
pub struct Cursor {
    // These are private so we can make sure whether to clear highlight is considered on each
    // mutation of `position. And we can use a state we don't otherwise want,
    // `highlight_position == Some(position)` to represent a state we do:
    // `highlight_position == None`.
    position: Position,
    highlight_position: Position,
    pub sticky_offset: CharOffset,
    pub state: CursorState,
}

impl Cursor {
    pub fn new(position: Position) -> Self {
        Self::new_with_highlight(position, position)
    }

    pub fn new_with_highlight(position: Position, highlight_position: Position) -> Self {
        Cursor {
            position,
            sticky_offset: position.offset,
            highlight_position,
            state: d!(),
        }
    }

    pub fn get_position(&self) -> Position {
        self.position
    }

    pub fn set_position(&mut self, position: Position) {
        self.set_position_custom(position, d!())
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

    pub fn get_highlight_position(&self) -> Option<Position> {
        Some(self.highlight_position).filter(|&p| p != self.position)
    }

    /// Equivalent to `c.get_highlight_position().unwrap_or(c.get_position())`
    pub fn get_highlight_position_or_position(&self) -> Position {
        self.highlight_position
    }

    pub fn set_highlight_position<P: Into<Option<Position>>>(&mut self, position: P) {
        self.highlight_position = position.into().unwrap_or(self.position);
    }
}

fmt_display! {
    for Cursor : Cursor {
            position,
            sticky_offset,
            highlight_position,
            ..
        } in "{}({}){}",
        position,
        sticky_offset,
        //kind of annoying duplication here.
        Some(highlight_position).filter(|&p| p != position).map(|h| format!("h:{}", h)).unwrap_or_default()
}
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

ord!(and friends for Cursor: c, other in {
    // We don't really have a preferred ordering for ranges with the same start and end. So we
    // treat two cursors where one's position is the other's highlight_position and vice-versa
    // as equal.
    let min = std::cmp::min(c.position, c.highlight_position);
    let max = std::cmp::max(c.position, c.highlight_position);

    let other_min = std::cmp::min(other.position, other.highlight_position);
    let other_max = std::cmp::max(other.position, other.highlight_position);

    min.cmp(&other_min)
        .then_with(|| max.cmp(&other_max))
        .then_with(|| c.sticky_offset.cmp(&other.sticky_offset))
        .then_with(|| c.state.cmp(&other.state))
});

#[cfg(test)]
mod tests {
    use super::*;
    use platform_types::pos;
    #[test]
    fn these_two_cursors_are_equal() {
        let mut c1 = Cursor::new(pos! {l 1 o 2});
        c1.set_highlight_position(pos! {l 3 o 4});
        c1.sticky_offset = d!();

        let mut c2 = Cursor::new(pos! {l 3 o 4});
        c2.set_highlight_position(pos! {l 1 o 2});
        c2.sticky_offset = d!();

        assert_eq!(c1, c2);
    }
}
