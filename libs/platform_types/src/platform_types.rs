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
}

d!(for Input is Input::NoInput);

#[derive(Default)]
pub struct View {
    pub buffers: Vec<BufferView>,
}

#[derive(Copy, Clone, Debug)]
pub enum BufferViewKind {
    Edit,
    StatusLine
}

pub struct BufferView {
    pub kind: BufferViewKind,
    pub screen_position: (f32, f32),
    //bounds: (f32, f32),
    //color: [f32; 4],
    pub chars: String,
}

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd is Cmd::NoCmd);

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
