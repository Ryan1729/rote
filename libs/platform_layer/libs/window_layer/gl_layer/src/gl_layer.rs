pub use gl_layer_types::*;

use shared::Res;
use window_layer_types::{TextOrRect, U24Dimensions};

pub struct State {
    open_gl: open_gl::State,
}

macros::fmt_debug!(for State: _ in "{}", "State");

pub use text_rendering::FONT_LICENSE;

/// # Safety
/// The passed `load_fn` must always return accurate function pointer 
/// values, or null on failure.
/// # Errors
/// Returns `Err` if errors occur during the text rendering or Open GL
/// initialization.
pub unsafe fn init<'load_fn, 'font>(
    clear_colour: [f32; 4],
    load_fn: &open_gl::LoadFn<'load_fn>,
    text_rendering: &text_rendering::State<'font>
) -> Res<State> {
    init_inner(
        clear_colour,
        load_fn,
        text_rendering,
    )
}

fn init_inner<'load_fn, 'font>(
    clear_colour: [f32; 4],
    load_fn: &open_gl::LoadFn<'load_fn>,
    text_rendering_state: &text_rendering::State<'font>
) -> Res<State> {
    Ok(
        State {
            open_gl: 
            // SAFETY:
            // The passed `load_fn` must always return accurate function pointer 
            // values, or null on failure.
            unsafe { 
                open_gl::State::new(
                    clear_colour,
                    text_rendering_state.texture_dimensions(),
                    load_fn
                )
            }?,
        }
    )
}

pub fn set_dimensions(state: &mut State, dimensions: U24Dimensions) {
    state.open_gl.set_dimensions(dimensions);
}

#[perf_viz::record]
pub fn render<'font>(
    state: &mut State,
    text_or_rects: &[TextOrRect],
    dimensions: U24Dimensions,
    text_rendering: &mut text_rendering::State<'font>
) {
    state.open_gl.begin_frame();

    let resize_texture = |(new_width, new_height): U24Dimensions| {
        eprint!("\r                            \r");
        eprintln!("Resizing glyph texture -> {}x{}", new_width, new_height);

        open_gl::State::resize_texture((new_width, new_height));
    };

    let replacement_vertices = text_rendering.render_vertices(
        text_or_rects,
        dimensions,
        |rect: window_layer_types::TextureRect, tex_data: &_| {
            // Update part of gpu texture with new glyph alpha values
            open_gl::State::update_texture(
                rect.x,
                rect.y,
                rect.w,
                rect.h,
                tex_data,
            );
        },
        resize_texture,
    );

    if let Some(vertices) = replacement_vertices {
        perf_viz::record_guard!("open_gl.draw_vertices");
        state.open_gl.draw_vertices(vertices);
    }

    state.open_gl.end_frame();
}

pub fn cleanup(
    state: &State,
) {
    state.open_gl.cleanup();
}