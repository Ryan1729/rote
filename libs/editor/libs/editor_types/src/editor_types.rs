use macros::{d, fmt_display, integer_newtype, usize_newtype};
use platform_types::{CharOffset, Position};
use std::borrow::Borrow;
use std::ops::{Add, Sub};
pub use vec1::{Vec1, vec1};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CursorState {
    None,
    PressedAgainstWall,
}
d!(for CursorState: CursorState::None);

#[derive(Clone, Copy, Debug)]
pub enum SetPositionAction {
    ClearHighlight,
    ClearHighlightOnlyIfItMatchesNewPosition,
    OldPositionBecomesHighlightIfItIsNone,
}
d!(for SetPositionAction: SetPositionAction::ClearHighlight);

#[derive(Clone, Debug, PartialEq, Eq)]
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
        Cursor {
            position,
            sticky_offset: position.offset,
            highlight_position: position,
            state: d!(),
        }
    }

    pub fn get_position(&self) -> Position {
        self.position
    }

    pub fn set_position(&mut self, position: Position) {
        self.set_position_custom(position, d!())
    }

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
