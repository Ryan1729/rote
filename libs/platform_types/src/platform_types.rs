#[macro_export]
macro_rules! d {
    () => {
        Default::default()
    };
    (for $name:ident is $code:expr) => {
        impl Default for $name {
            fn default() -> Self {
                $code
            }
        }
    };
}

#[macro_export]
macro_rules! dg {
    ($thing:expr) => {
        if cfg!(debug_assertions) {
            dbg!($thing)
        } else {
            $thing
        }
    };
}

#[cfg(feature = "invariant-checking")]
#[macro_export]
macro_rules! invariant_violation {
    () => ({
        invariant_violation(&format!("invariant was violated! {}:{}", file!(), line!()));
        panic!("invariant was violated!")
    });
    ($code:block, $($rest:tt)*) => {
        invariant_violation!($($rest)*)
    };
    ($msg:expr) => ({
        invariant_violation(&format!("{} {}:{}", $msg, file!(), line!()));
        panic!($msg)
    });
    ($msg:expr,) => (
        invariant_violation!($msg)
    );
    ($fmt:expr, $($arg:tt)+) => ({
        invariant_violation(&format!($fmt, $($arg)*));
        panic!($fmt, $($arg)*)
    });
}

#[cfg(not(feature = "invariant-checking"))]
#[macro_export]
macro_rules! invariant_violation {
    ($code:block, $($rest:tt)*) => {
        $code
    };
    ($($whatever:tt)*) => {};
}

#[cfg(feature = "invariant-checking")]
#[macro_export]
macro_rules! invariant_assert {
    ($($arg:tt)+) => ({
        assert!($($arg)*)
    });
}

#[cfg(not(feature = "invariant-checking"))]
#[macro_export]
macro_rules! invariant_assert {
    ($($whatever:tt)*) => {};
}

#[cfg(feature = "invariant-checking")]
#[macro_export]
macro_rules! invariant_assert_eq {
    ($($arg:tt)+) => ({
        assert_eq!($($arg)*)
    });
}

#[cfg(not(feature = "invariant-checking"))]
#[macro_export]
macro_rules! invariant_assert_eq {
    ($($whatever:tt)*) => {};
}

// This is only slightly nicer to use than using the body of the macro directly, but
// it's nice to have all the features stuff in one place as a form of documentation.
#[macro_export]
macro_rules! invariants_checked {
    () => {{
        cfg!(feature = "invariant-checking")
    }};
}

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
}

#[derive(Clone, Copy, Debug)]
pub enum Input {
    None,
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    SetSizes(Sizes),
    MoveAllCursors(Move),
}

d!(for Input is Input::None);

#[derive(Clone, Copy, Debug)]
pub struct Sizes {
    pub screen_w: Option<f32>,
    pub screen_h: Option<f32>,
    pub char_w: Option<f32>,
    pub line_h: Option<f32>,
}

#[macro_export]
macro_rules! Sizes {
    {
        screen_w: $screen_w:expr,
        screen_h: $screen_h:expr,
        char_w: $char_w:expr,
        line_h: $line_h:expr $(,)?
    } => (
        Sizes {
            screen_w: $screen_w.into(),
            screen_h: $screen_h.into(),
            char_w: $char_w.into(),
            line_h: $line_h.into(),
        }
    );
}

#[derive(Default)]
pub struct View {
    pub buffers: Vec<BufferView>,
}

#[derive(Copy, Clone, Debug)]
pub enum BufferViewKind {
    Edit,
    StatusLine,
}

pub struct BufferView {
    pub kind: BufferViewKind,
    pub screen_position: (f32, f32),
    pub bounds: (f32, f32),
    pub color: [f32; 4],
    //TODO make this a &str or a char iterator
    pub chars: String,
}

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd is Cmd::NoCmd);

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
