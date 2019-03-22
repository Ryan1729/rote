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
}

d!(for Input is Input::NoInput);

pub struct View {}

d!(for View is View{});

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

d!(for Cmd is Cmd::NoCmd);

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
