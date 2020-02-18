use super::*;
#[derive(Clone, Copy, Debug)]
pub enum Move {
    Up,
    Down,
    Left,
    Right,
    ToLineStart,
    ToLineEnd,
    ToBufferStart,
    ToBufferEnd,
    ToPreviousLikelyEditLocation,
    ToNextLikelyEditLocation,
}
use Move::*;

fmt_display!(for Move: r#move in "{}", match r#move {
    Up => ">",
    Down => "v",
    Left => "<",
    Right => ">",
    ToLineStart => "Line<",
    ToLineEnd => "Line>",
    ToBufferStart => "Buffer<",
    ToBufferEnd => "Buffer>",
    ToPreviousLikelyEditLocation => "Edit<",
    ToNextLikelyEditLocation => "Edit>",
});

macro_rules! to_num {
    ($m: expr) => {
        match $m {
            Up => 0,
            Down => 1,
            Left => 2,
            Right => 3,
            ToLineStart => 4,
            ToLineEnd => 5,
            ToBufferStart => 6,
            ToBufferEnd => 7,
            ToPreviousLikelyEditLocation => 8,
            ToNextLikelyEditLocation => 9,
        }
    };
}

ord!(and friends for Move: r#move, other in to_num!(r#move).cmp(&to_num!(other)));

impl std::ops::Not for Move {
    type Output = Move;

    fn not(self) -> Self::Output {
        match self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
            ToLineStart => ToLineEnd,
            ToLineEnd => ToLineStart,
            ToBufferStart => ToBufferEnd,
            ToBufferEnd => ToBufferStart,
            ToPreviousLikelyEditLocation => ToNextLikelyEditLocation,
            ToNextLikelyEditLocation => ToPreviousLikelyEditLocation,
        }
    }
}
