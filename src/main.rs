use macros::d;
use platform_types::{Input, UpdateAndRenderOutput};

fn update_and_render(input: Input) -> UpdateAndRenderOutput {
    use lazy_static::lazy_static;
    lazy_static! {
        static ref STATE_MUTEX: std::sync::Mutex<editor::State> =
            std::sync::Mutex::new(editor::new());
    }
    match STATE_MUTEX.try_lock() {
        Ok(mut state) => editor::update_and_render(&mut state, input),
        Err(e) => {
            if cfg!(feature = "invariant-checking") {
                panic!("STATE_MUTEX already borrowed!? \n{}", e);
            }
            d!()
        }
    }
}

fn main() {
    println!("{} v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
    platform_layer::run(update_and_render);
}
