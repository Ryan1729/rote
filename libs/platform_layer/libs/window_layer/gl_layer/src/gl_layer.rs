pub use gl_layer_types::*;

use platform_types::{CharDim};
use shared::Res;

pub struct State<'font> {
    open_gl: open_gl::State,
    text_rendering: text_rendering::State<'font>,
}

macros::fmt_debug!(for State<'_>: _ in "{}", "State");

pub use text_rendering::FONT_LICENSE;

/// # Safety
/// The passed `load_fn` must always return accurate function pointer 
/// values, or null on failure.
/// # Errors
/// Returns `Err` if errors occur during the text rendering or Open GL
/// initialization.
pub unsafe fn init<'load_fn>(
    hidpi_factor: f32,
    clear_colour: [f32; 4],
    load_fn: &open_gl::LoadFn<'load_fn>,
) -> Res<State<'static>> {
    init_inner(
        hidpi_factor,
        clear_colour,
        load_fn,
    )
}

fn init_inner<'load_fn>(
    hidpi_factor: f32,
    clear_colour: [f32; 4],
    load_fn: &open_gl::LoadFn<'load_fn>,
) -> Res<State<'static>> {
    let text_rendering_state = text_rendering::new(
        hidpi_factor
    )?;

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
            text_rendering: text_rendering_state,
        }
    )
}

#[must_use]
pub fn get_char_dims(state: &State, text_sizes: &[f32]) -> Vec<CharDim> {
    state.text_rendering.get_char_dims(text_sizes)
}

pub fn set_dimensions(state: &mut State, hidpi_factor: f32, dimensions: Dimensions) {
    state.open_gl.set_dimensions(dimensions);

    state.text_rendering.set_dimensions(hidpi_factor);
}

#[perf_viz::record]
pub fn render(
    state: &mut State,
    text_or_rects: &[TextOrRect],
    dimensions: Dimensions,
) {
    state.open_gl.begin_frame();

    let resize_texture = |(new_width, new_height): Dimensions| {
        eprint!("\r                            \r");
        eprintln!("Resizing glyph texture -> {}x{}", new_width, new_height);

        open_gl::State::resize_texture((new_width, new_height));
    };

    let replacement_vertices = state.text_rendering.render_vertices(
        text_or_rects,
        dimensions,
        |rect: text_rendering::TextureRect, tex_data: &_| {
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