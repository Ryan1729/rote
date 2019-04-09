use macros::d;
use platform_types::{Cmd, Input, View};

fn update_and_render<'view>(view: &mut View<'view>, input: Input) -> Cmd {
    use lazy_static::lazy_static;
    lazy_static! {
        static ref STATE_MUTEX: std::sync::Mutex<editor::State> =
            std::sync::Mutex::new(editor::new());
    }
    match STATE_MUTEX.try_lock() {
        Ok(mut state) => editor::update_and_render(&mut state, view, input),
        Err(e) => {
            if cfg!(feature = "invariant-checking") {
                panic!("STATE_MUTEX already borrowed!? \n{}", e);
            }
            d!()
        }
    }
}

fn main() {
    platform_layer::run(update_and_render);
}
