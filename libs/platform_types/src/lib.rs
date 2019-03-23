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
}

d!(for Input is Input::NoInput);

#[derive(Default)]
pub struct View {
    pub buffers: Vec<BufferView>,
}

pub struct BufferView {
    //screen_position: (f32, f32),
    //bounds: (f32, f32),
    //color: [f32; 4],
    pub chars: String,
}

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd is Cmd::NoCmd);

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
