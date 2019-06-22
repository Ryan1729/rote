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
