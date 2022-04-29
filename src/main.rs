use macros::d;
use platform_types::{BufferName, EditorAPI, Input, UpdateAndRenderOutput, LoadBufferViewsResult};
use editor::State;

// We expect that the `EditorAPI` function pointers will never be called at the same
// time, so the `Err` case of `try_lock` should never occur.

fn state_call<In, Out>(
    r#in: In,
    callback: fn(&mut State, In) -> Out,
    fallback: fn() -> Out,
) -> Out {
    use lazy_static::lazy_static;
    lazy_static! {
        static ref STATE_MUTEX: std::sync::Mutex<State> =
            std::sync::Mutex::new(editor::new());
    }
    match STATE_MUTEX.try_lock() {
        Ok(mut state) => callback(&mut state, r#in),
        Err(e) => {
            assert!(!cfg!(feature = "invariant-checking"), "STATE_MUTEX already borrowed!? \n{}", e);
            fallback()
        }
    }
}

fn update_and_render(input: Input) -> UpdateAndRenderOutput {
    fn fallback() -> UpdateAndRenderOutput {
        d!()
    }
    state_call(input, editor::update_and_render, fallback)
}

fn load_buffer_views(buffer_names: &[BufferName]) -> Vec<LoadBufferViewsResult> {
    fn fallback() -> Vec<LoadBufferViewsResult> {
        vec![Err("load_buffer_views fallback".to_string())]
    }
    fn load_buffer_views_helper(
        state: &mut State,
        buffer_names: &[BufferName]
    ) -> Vec<LoadBufferViewsResult> {
        buffer_names.iter().map(|n| editor::load_buffer_view(state, n)).collect()
    }

    state_call(buffer_names, load_buffer_views_helper, fallback)
}

fn main() {
    println!("{} v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));

    platform_layer::run(EditorAPI {
        update_and_render,
        load_buffer_views,
    });
}
