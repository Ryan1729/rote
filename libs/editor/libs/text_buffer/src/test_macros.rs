use super::*;
use macros::fmt_debug;

#[macro_export]
macro_rules! r {
    ($s:expr) => {
        Rope::from_str(&$s)
    };
}

#[macro_export]
macro_rules! t_b {
    ($s:expr) => {{
        let t: TextBuffer = $s.into();
        t
    }};
}

#[macro_export]
macro_rules! cursor_assert {
    (
        $buffer:expr
        $(, p: $pos: expr)?
        $(, h: $highlight_position: expr)?
        $(, s: $state: expr)?
        $(,)?) => {{
        let c = $buffer.cursors.first();

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
            assert_eq!(c.state, $state);
        )*

    }};
}

pub struct IgnoringHistory<'buffer>(pub &'buffer TextBuffer);

fmt_debug!(<'a> for IgnoringHistory<'a>:
     IgnoringHistory(b) in "{{ rope: {:?}, cursors: {:?} }}", b.rope, b.cursors
 );

impl<'a, 'b> PartialEq<IgnoringHistory<'b>> for IgnoringHistory<'a> {
    fn eq(&self, other: &IgnoringHistory<'b>) -> bool {
        self.0.rope == other.0.rope && self.0.cursors == other.0.cursors
    }
}

macro_rules! assert_text_buffer_eq_ignoring_history {
    ($left:expr, $right:expr) => {
        assert_eq!(
            $crate::test_macros::IgnoringHistory(&$left),
            $crate::test_macros::IgnoringHistory(&$right),
            stringify!($left != $right (ignoring history))
        );
    };
}
