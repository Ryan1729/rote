use super::*;
use macros::fmt_debug;

#[macro_export]
macro_rules! r {
    ($s:expr) => {
        Rope::from_str(&$s)
    };
}

#[macro_export]
macro_rules! c_r {
    ($s:expr) => {
        CursoredRope::from(Rope::from_str(&$s))
    };
}

#[macro_export]
macro_rules! t_b {
    ($s:expr) => {{
        let t: TextBuffer = $s.into();
        t
    }};
    ($s:expr, $cursors: expr) => {{
        let mut t: TextBuffer = $s.into();
        t.set_cursors_from_vec1($cursors);
        t
    }};
}

#[macro_export]
macro_rules! cursor_assert {
    (
        $buffer:expr
        $(, p: $pos: expr)?
        $(, h: $highlight_position: expr)?
        $(, s: $state: pat)?
        $(,)?) => {{
        let c = $buffer.borrow_cursors().first();

        $(
            assert_eq!(c.get_position(), $pos, "positions do not match");
        )*

        $(
            assert_eq!(
                c.get_highlight_position(),
                $highlight_position.into(),
                "highlight positions do not match"
            );
        )*

        $(
            match c.state {
                $state => {},
                _ => panic!("{:?} does not match {}", c.state, stringify!($state))
            }
        )*

    }};
}

pub struct IgnoringStateSingle<'cursor>(pub &'cursor Cursor);

fmt_debug!(<'a> for IgnoringStateSingle<'a>:
     IgnoringStateSingle(cursor) in
     "{{ position: {:?}, highlight_position: {:?}, sticky_offset: {:?} }}",
      cursor.get_position(), cursor.get_highlight_position(), cursor.sticky_offset
 );

impl<'a, 'b> PartialEq<IgnoringStateSingle<'b>> for IgnoringStateSingle<'a> {
    fn eq(&self, other: &IgnoringStateSingle<'b>) -> bool {
        self.0.get_position() == other.0.get_position()
            && self.0.get_highlight_position() == other.0.get_highlight_position()
            && self.0.sticky_offset == other.0.sticky_offset
    }
}

pub struct IgnoringState<'cursors>(pub &'cursors Cursors);

fmt_debug!(<'a> for IgnoringState<'a>:
     IgnoringState(cursors) in "{:?}", cursors.iter().map(IgnoringStateSingle).collect::<Vec<_>>()
 );

impl<'a, 'b> PartialEq<IgnoringState<'b>> for IgnoringState<'a> {
    fn eq(&self, other: &IgnoringState<'b>) -> bool {
        self.0.iter().map(IgnoringStateSingle).collect::<Vec<_>>()
            == other.0.iter().map(IgnoringStateSingle).collect::<Vec<_>>()
    }
}

pub struct IgnoringHistory<'buffer>(pub &'buffer TextBuffer);

fmt_debug!(<'a> for IgnoringHistory<'a>:
     IgnoringHistory(b) in "{{ rope: {:?}, cursors: {:?} }}", b.borrow_rope(), IgnoringState(b.borrow_cursors())
 );

impl<'a, 'b> PartialEq<IgnoringHistory<'b>> for IgnoringHistory<'a> {
    fn eq(&self, other: &IgnoringHistory<'b>) -> bool {
        self.0.borrow_rope() == other.0.borrow_rope()
        && IgnoringState(self.0.borrow_cursors()) == IgnoringState(other.0.borrow_cursors())
    }
}

#[macro_export]
macro_rules! assert_text_buffer_eq_ignoring_history {
    ($left:expr, $right:expr) => {
        assert_eq!(
            $crate::test_macros::IgnoringHistory(&$left),
            $crate::test_macros::IgnoringHistory(&$right),
            stringify!($left != $right (ignoring history))
        );
    };
}

#[macro_export]
macro_rules! text_buffer_eq_ignoring_history {
    ($left:expr, $right:expr) => {
        $crate::test_macros::IgnoringHistory(&$left)
            == $crate::test_macros::IgnoringHistory(&$right)
    };
}

#[macro_export]
macro_rules! assert_text_buffer_rope_eq {
    ($left:expr, $right:expr) => {
        assert_eq!(
            $left.rope,
            $right.rope,
            stringify!($left != $right (ignoring everything but the rope))
        );
    };
}

#[macro_export]
macro_rules! text_buffer_rope_eq {
    ($left:expr, $right:expr) => {
        $left.rope == $right.rope
    };
}

