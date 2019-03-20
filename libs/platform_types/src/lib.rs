pub enum Input {
    NoInput,
}

pub struct View {}

pub enum Cmd {
    NoCmd, //The plan is to communicate things like saving to the platform layer with this
}

pub type UpdateAndRender = fn(Input) -> (View, Cmd);
