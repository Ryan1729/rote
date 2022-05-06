/// This is meant to be the smallest reasonable example, and perhaps a template
/// for other examples/applications.
use window_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec, Api, GlProfile, GlRequest};
use platform_types::screen_positioning::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let events = window_layer::event_loop::EventLoop::new();
    let window_layer_context = window_layer::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            window_layer::window::WindowBuilder::new()
                .with_inner_size(
                    window_layer::dpi::Size::Logical(window_layer::dpi::LogicalSize::new(683.0, 393.0))
                )
                .with_title("hello-world"),
            &events,
        )?;
    let window_layer_context = unsafe { window_layer_context.make_current() }.map_err(|(_, e)| e)?;

    const TEXT_SIZE: f32 = 16.0;

    let mut hidpi_factor = 1.0;

    let mut gl_state = gl_layer::init(
        hidpi_factor as f32,
        [0.3, 0.3, 0.3, 1.0],
        &|symbol| {
            // SAFETY: The underlying library has promised to pass us a nul 
            // terminated pointer.
            let cstr = unsafe { std::ffi::CStr::from_ptr(symbol as _) };
    
            let s = cstr.to_str().unwrap();
    
            window_layer_context.get_proc_address(s) as _
        },
    )?;

    let mut loop_helper = spin_sleep::LoopHelper::builder()
        .report_interval_s(1./500.)
        .build_with_target_rate(250.0);

    let mut running = true;
    let mut dimensions = window_layer_context
        .window()
        .inner_size();

    let text_colour = [0.9, 0.9, 0.9, 1.0];

    let mut frame_count: u32 = 0;

    {
        events.run(move |event, _, control_flow| {
            use window_layer::event::*;

            match event {
                Event::MainEventsCleared if running => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    window_layer_context.window().request_redraw();
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
                            ..<_>::default()
                        },
                        layout: TextLayout::Unbounded,
                    }));

                    gl_layer::render(&mut gl_state, &text_and_rects, width as _, height as _)
                        .expect("gl_layer::render didn't work");

                    window_layer_context
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

                            let _ = gl_layer::cleanup(&gl_state);

                            *control_flow = window_layer::event_loop::ControlFlow::Exit;
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
                            window_layer_context.resize(size);
                            dimensions = size;
                            gl_layer::set_dimensions(
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
