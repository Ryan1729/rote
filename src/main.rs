use macros::d;
use platform_types::{BufferName, BufferView, EditorAPI, Input, UpdateAndRenderOutput};
use editor::State;

// We expect that the `EditorAPI` function pointers will never be called at the same
// time, so the `Err` case of `try_lock` should never occur.

fn state_call<In, Out: Default>(r#in: In, callback: fn(&mut State, In) -> Out) -> Out {
    use lazy_static::lazy_static;
    lazy_static! {
        static ref STATE_MUTEX: std::sync::Mutex<State> =
            std::sync::Mutex::new(editor::new());
    }
    match STATE_MUTEX.try_lock() {
        Ok(mut state) => callback(&mut state, r#in),
        Err(e) => {
            if cfg!(feature = "invariant-checking") {
                panic!("STATE_MUTEX already borrowed!? \n{}", e);
            }
            d!()
        }
    }
}

fn update_and_render(input: Input) -> UpdateAndRenderOutput {
    state_call(input, editor::update_and_render)
}

fn load_buffer_view(buffer_name: &BufferName) -> Option<BufferView> {
    state_call(buffer_name, editor::load_buffer_view)
}

fn main() {
    println!("{} v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));

    platform_layer::run(EditorAPI {
        update_and_render,
        load_buffer_view,
    });
}
