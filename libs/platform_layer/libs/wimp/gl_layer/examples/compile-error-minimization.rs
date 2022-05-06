/// This is meant to be the smallest reasonable example, and perhaps a template
/// for other examples/applications.
use glutin_wrapper::{Api, GlProfile, GlRequest};
use platform_types::{screen_positioning::*};
use shared::Res;
use macros::{d};

pub use gl_layer_types::{TextLayout, TextOrRect, TextSpec, VisualSpec};

use platform_types::{CharDim};

pub struct State<'font> {
    open_gl: open_gl::State,
    text_rendering: text_rendering::State<'font>,
}

macros::fmt_debug!(for State<'_>: _ in "{}", "State");

pub use text_rendering::FONT_LICENSE;

pub fn init<'load_fn>(
    hidpi_factor: f32,
    clear_colour: [f32; 4], // the clear colour currently flashes up on exit.
    load_fn: &'load_fn open_gl::LoadFn,
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

pub fn set_dimensions(state: &mut State, hidpi_factor: f32, wh: (i32, i32)) {
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

fn main() -> Res<()> {
    let events = glutin_wrapper::event_loop::EventLoop::new();
    let glutin_wrapper_context = glutin_wrapper::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            glutin_wrapper::window::WindowBuilder::new()
                .with_inner_size(
                    glutin_wrapper::dpi::Size::Logical(glutin_wrapper::dpi::LogicalSize::new(683.0, 393.0))
                )
                .with_title("compile error minimization"),
            &events,
        )?;
    let glutin_wrapper_context = unsafe { glutin_wrapper_context.make_current().map_err(|(_, e)| e)? };

    const TEXT_SIZE: f32 = 16.0;

    let mut hidpi_factor = 1.0;

    let mut gl_state = init(
        hidpi_factor as f32,
        [0.3, 0.3, 0.3, 1.0],
        &|symbol| {
            // SAFETY: The underlying library has promised to pass us a nul 
            // terminated pointer.
            let cstr = unsafe { std::ffi::CStr::from_ptr(symbol as _) };
    
            let s = cstr.to_str().unwrap();
    
            glutin_wrapper_context.get_proc_address(s) as _
        },
    )?;

    let mut loop_helper = spin_sleep::LoopHelper::builder()
        .report_interval_s(1./500.)
        .build_with_target_rate(250.0);

    let mut running = true;
    let mut dimensions = glutin_wrapper_context
        .window()
        .inner_size();

    let text_colour = [0.9, 0.9, 0.9, 1.0];

    let mut frame_count: u32 = 0;

    {
        events.run(move |event, _, control_flow| {
            use glutin_wrapper::event::*;

            match event {
                Event::MainEventsCleared if running => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_wrapper_context.window().request_redraw();
                }
                Event::RedrawRequested(_) => {
                    frame_count = frame_count.wrapping_add(1);
                    let width = dimensions.width as f32;
                    let height = dimensions.height as f32;

                    let mut text_and_rects = Vec::with_capacity(16);

                    let rate = loop_helper.report_rate().unwrap_or_default();

                    let text = &format!(
                        "Hello world! {rate:.2} FPS on frame {frame_count}"
                    );

                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text,
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(0.0, height - TEXT_SIZE),
                                max: ssxy!(width, height),
                            },
                            colour: text_colour,
                            ..d!()
                        },
                        layout: TextLayout::Unbounded,
                    }));

                    render(&mut gl_state, &text_and_rects, width as _, height as _)
                        .expect("render didn't work");

                    glutin_wrapper_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");
                    loop_helper.loop_sleep();

                    // We want to track the time that the message loop takes too!
                    loop_helper.loop_start();
                }
                Event::NewEvents(StartCause::Init) => {
                    // At least try to measure the first frame accurately
                    loop_helper.loop_start();
                }
                Event::WindowEvent { event, .. } => {
                    macro_rules! quit {
                        () => {{
                            running = false;

                            let _ = cleanup(&gl_state);

                            *control_flow = glutin_wrapper::event_loop::ControlFlow::Exit;
                        }};
                    }

                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            hidpi_factor = scale_factor;
                        }
                        WindowEvent::Resized(size) => {
                            glutin_wrapper_context.resize(size);
                            dimensions = size;
                            set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (dimensions.width as _, dimensions.height as _),
                            );
                        }
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    ..
                                },
                            ..
                        }=> {
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                },
                                _ => (),
                            };
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }
}
