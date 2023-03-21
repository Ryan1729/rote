use macros::{d, fmt_debug, fmt_display, format_if, ord};
pub use platform_types::{AbsoluteCharOffset, CharOffset, CursorState, Position, pos};
use std::borrow::Borrow;
use core::str::FromStr;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum CursorParseError {
    NumberParseError,
    EndOfStr,
    Unhandled
}

impl FromStr for Cursor {
    type Err = CursorParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use CursorParseError::*;
        if s.is_empty() { return Ok(cur!{}) }

        #[derive(Clone, Copy, Debug)]
        enum State {
            L,
            LNum,
            O,
            ONum,
            H,
            HL,
            HLNum,
            HO,
            HONum,
        }

        let mut l = 0;
        let mut o = 0;
        let mut h_l = 0;
        let h_o;
        let mut state = State::L;
        let mut chars = s.chars();
        loop {
            state = match (state, chars.next()) {
                (State::L, Some('l')) => State::LNum,
                (State::LNum, Some(' ')) => State::LNum,
                (State::LNum, Some(ch @ '0'..='9')) => {
                    let mut byte_arr = [0; 16];
                    let mut i = 0;
                    byte_arr[i] = ch as u32 as u8;
                    i += 1;

                    loop {
                        match chars.next() {
                            Some(c @ '0'..='9') => {
                                byte_arr[i] = c as u32 as u8;
                                i += 1;
                            },
                            Some(' ') => {
                                l = core::str::from_utf8(&byte_arr[..i])
                                    .map_err(|_| NumberParseError)
                                    .and_then(|s|
                                        FromStr::from_str(s)
                                            .map_err(|_| NumberParseError)
                                    )
                                    ?;
                                break State::O;
                            },
                            Some(_) => return Err(Unhandled),
                            None => return Err(EndOfStr)
                        }
                    }
                },
                (State::O, Some('o')) => State::ONum,
                (State::ONum, Some(' ')) => State::ONum,
                (State::ONum, Some(ch @ '0'..='9')) => {
                    let mut byte_arr = [0; 16];
                    let mut i = 0;
                    byte_arr[i] = ch as u32 as u8;
                    i += 1;

                    loop {
                        match chars.next() {
                            Some(c @ '0'..='9') => {
                                byte_arr[i] = c as u32 as u8;
                                i += 1;
                            },
                            Some(' ') | None => {
                                o = core::str::from_utf8(&byte_arr[..i])
                                    .map_err(|_| NumberParseError)
                                    .and_then(|s|
                                        FromStr::from_str(s)
                                            .map_err(|_| NumberParseError)
                                    )
                                    ?;
                                break State::H;
                            },
                            Some(_) => return Err(Unhandled),
                        }
                    }
                },
                (State::H, None) => return Ok(cur!{pos!{l l, o o}}),
                (State::H, Some(' ')) => State::H,
                (State::H, Some('h')) => State::HL,
                (State::HL, Some(' ')) => State::HL,
                (State::HL, Some('l')) => State::HLNum,
                (State::HLNum, Some(' ')) => State::HLNum,
                (State::HLNum, Some(ch @ '0'..='9')) => {
                    let mut byte_arr = [0; 16];
                    let mut i = 0;
                    byte_arr[i] = ch as u32 as u8;
                    i += 1;

                    loop {
                        match chars.next() {
                            Some(c @ '0'..='9') => {
                                byte_arr[i] = c as u32 as u8;
                                i += 1;
                            },
                            Some(' ') => {
                                h_l = core::str::from_utf8(&byte_arr[..i])
                                    .map_err(|_| NumberParseError)
                                    .and_then(|s|
                                        FromStr::from_str(s)
                                            .map_err(|_| NumberParseError)
                                    )
                                    ?;
                                break State::HO;
                            },
                            Some(_) => return Err(Unhandled),
                            None => return Err(EndOfStr)
                        }
                    }
                },
                (State::HO, Some('o')) => State::HONum,
                (State::HONum, Some(' ')) => State::HONum,
                (State::HONum, Some(ch @ '0'..='9')) => {
                    let mut byte_arr = [0; 16];
                    let mut i = 0;
                    byte_arr[i] = ch as u32 as u8;
                    i += 1;

                    loop {
                        match chars.next() {
                            Some(c @ '0'..='9') => {
                                byte_arr[i] = c as u32 as u8;
                                i += 1;
                            },
                            Some(' ') | None => {
                                h_o = core::str::from_utf8(&byte_arr[..i])
                                    .map_err(|_| NumberParseError)
                                    .and_then(|s|
                                        FromStr::from_str(s)
                                            .map_err(|_| NumberParseError)
                                    )
                                    ?;
                                return Ok(cur!{pos!{l l, o o}, pos!{l h_l, o h_o}})
                            },
                            Some(_) => return Err(Unhandled),
                        }
                    }
                },
                (_, Some(_)) => return Err(Unhandled),
                (_, None) => return Err(EndOfStr),
            };
        }
    }
}

#[test]
fn from_str_works_on_these_examples() {
    assert_eq!(
        Cursor::from_str(""),
        Ok(cur!{})
    );

    assert_eq!(
        Cursor::from_str("l 1 o 2"),
        Ok(cur!{l 1 o 2})
    );

    assert_eq!(
        Cursor::from_str("l 1 o 2 h l 3 o 4"),
        Ok(cur!{l 1 o 2 h l 3 o 4})
    );
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
