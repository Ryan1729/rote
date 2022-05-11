pub use gl_layer_types::*;

use platform_types::{CharDim};
use shared::Res;

pub struct State<'font> {
    open_gl: open_gl::State,
    text_rendering: text_rendering::State<'font>,
}

macros::fmt_debug!(for State<'_>: _ in "{}", "State");

pub use text_rendering::FONT_LICENSE;

pub fn init<'load_fn>(
    hidpi_factor: f32,
    clear_colour: [f32; 4],
    load_fn: &open_gl::LoadFn<'load_fn>,
) -> Res<State<'static>> {
    let text_rendering_state = text_rendering::new(
        hidpi_factor
    )?;

    Ok(
        State {
            open_gl: open_gl::State::new(
                clear_colour,
                text_rendering_state.texture_dimensions(),
                load_fn
            )?,
            text_rendering: text_rendering_state,
        }
    )
}

pub fn get_char_dims(state: &State, text_sizes: &[f32]) -> Vec<CharDim> {
    state.text_rendering.get_char_dims(text_sizes)
}

pub fn set_dimensions(state: &mut State, hidpi_factor: f32, wh: (u32, u32)) {
    state.open_gl.set_dimensions(wh);

    state.text_rendering.set_dimensions(hidpi_factor);
}

#[perf_viz::record]
pub fn render(
    state: &mut State,
    text_or_rects: &[TextOrRect],
    width: u32,
    height: u32,
) -> Res<()> {
    state.open_gl.begin_frame();

    let resize_texture = |new_width: u32, new_height: u32| {
        eprint!("\r                            \r");
        eprintln!("Resizing glyph texture -> {}x{}", new_width, new_height);

        open_gl::State::resize_texture(new_width, new_height);
    };

    let replacement_vertices = state.text_rendering.render_vertices(
        &text_or_rects,
        (width, height),
        |rect: text_rendering::TextureRect, tex_data: &_| {
            // Update part of gpu texture with new glyph alpha values
            open_gl::State::update_texture(
                rect.min.x as _,
                rect.min.y as _,
                rect.width() as _,
                rect.height() as _,
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

    Ok(())
}

pub fn cleanup(
    state: &State,
) -> Res<()> {
    state.open_gl.cleanup()
}