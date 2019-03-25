#[macro_export]
macro_rules! d {
    () => {
        Default::default()
    };
    (for $name:ident is $code:expr) => {
        impl Default for $name {
            fn default() -> $name {
                $code
            }
        }
    };
}

pub enum Input {
    NoInput,
    Insert(char),
    Delete,
    ResetScroll,
    ScrollVertically(f32),
    ScrollHorizontally(f32),
    SetSizes(Sizes),
}

d!(for Input is Input::NoInput);

pub struct Sizes {
    pub screen_w: Option<f32>,
    pub screen_h: Option<f32>,
    pub line_h: Option<f32>,
}

#[macro_export]
macro_rules! Sizes {
    {
        screen_w: $screen_w:expr,
        screen_h: $screen_h:expr,
        line_h: $line_h:expr $(,)?
    } => (
        Sizes {
            screen_w: $screen_w.into(),
            screen_h: $screen_h.into(),
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
    pub chars: String,
}

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd is Cmd::NoCmd);

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
