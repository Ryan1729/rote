use crate::{desired_indent_delta};
use editor_types::{CharOffset, Cursor, Position, cur, pos};
use panic_safe_rope::Rope;

// Short for assert. We can be this brief becasue this is specific to this
// module
macro_rules! a {
    ($rope: expr, $cursor: expr, $expected: expr) => {
        let rope = Rope::from($rope);
        assert_eq!(desired_indent_delta(&rope, $cursor), $expected);
    }
}

#[test]
fn on_this_already_indented_example() {
    a!(r#"
  1
  2
  3
"#, 
        cur!{l 2 o 0},
        0
    );
}

#[test]
fn on_this_over_indented_example() {
    a!(r#"
  1
   2
  3
"#, 
        cur!{l 2 o 0},
        -1
    );
}

#[test]
fn on_this_under_indented_example() {
    a!(r#"
  1
 2
  3
"#, 
        cur!{l 2 o 0},
        1
    );
}

#[test]
fn on_this_already_indented_brace_example() {
    a!(r#"
{
    a
}
"#, 
        cur!{l 2 o 0},
        0
    );
}

#[test]
fn on_this_over_indented_brace_example() {
    a!(r#"
{
     a
}
"#, 
        cur!{l 2 o 0},
        -1
    );
}

#[test]
fn on_this_under_indented_brace_example() {
    a!(r#"
{
   a
}
"#, 
        cur!{l 2 o 0},
        1
    );
}