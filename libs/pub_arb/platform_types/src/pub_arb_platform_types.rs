pub use platform_types::{BufferMove};
use arb_macros::{arb_enum};

arb_enum!{
    pub fn buffer_move() -> BufferMove {
        Left => Just(Left),
        Right => Just(Right),
        ToStart => Just(ToStart),
        ToEnd => Just(ToEnd),
    }
}