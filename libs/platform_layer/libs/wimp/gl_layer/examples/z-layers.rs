/// This is an example/test demonstrating that all of the 2^16 = 65536 values for z that this crate
/// allows, actually work. This might seems like a silly thing to have to test, but GPUs and OpenGL
/// are silly things. Besides, this should be a nice simple example of how to setup a simple
/// sceene with this crate.
use gl_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use glutin::{Api, GlProfile, GlRequest};
use platform_types::{
    floating_point::{next_largest_f32_if_normal_or_0, next_smallest_f32_if_normal_or_0},
    *,
};
use shared::Res;

fn main() -> Res<()> {
    let events = glutin::event_loop::EventLoop::new();
    let glutin_context = glutin::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .build_windowed(
            glutin::window::WindowBuilder::new()
                .with_inner_size((683, 393).into())
                .with_title("z-layers example"),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    const TEXT_SIZE: f32 = 128.0;
    const HELP_SIZE: f32 = 16.0;

    let text_sizes = [TEXT_SIZE, HELP_SIZE];

    let (mut gl_state, _) = gl_layer::init(
        glutin_context.window().hidpi_factor() as f32,
        &text_sizes,
        |symbol| glutin_context.get_proc_address(symbol) as _,
    )?;

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(250.0);

    let mut running = true;
    let mut dimensions = glutin_context
        .window()
        .inner_size()
        .to_physical(glutin_context.window().hidpi_factor());

    let first_colour = [0.0, 0.6, 0.6, 1.0];
    let text_colour = [0.9, 0.9, 0.9, 1.0];
    let second_colour = [0.6, 0.6, 0.0, 1.0];

    // User manipulatable state
    let mut first_z: f32 = 0.25;
    let mut second_z: f32 = next_largest_f32_if_normal_or_0(first_z);

    {
        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            match event {
                Event::EventsCleared if running => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                }
                Event::WindowEvent {
                    event: WindowEvent::RedrawRequested,
                    ..
                } => {
                    let width = dimensions.width as f32;
                    let height = dimensions.height as f32;

                    // The idea here is that we impose a 4 by 4 grid on the screen
                    // with the upper left corner notated as (0, 0) and the lower right as (4, 4).
                    let x0 = 0.0;
                    let y0 = 0.0;

                    let x1 = width / 4.0;
                    let y1 = height / 4.0;

                    let x2 = x1 * 2.0;
                    let y2 = y1 * 2.0;

                    let x3 = x1 * 3.0;
                    let y3 = y1 * 3.0;

                    let x4 = width;
                    let y4 = height;

                    let mut text_and_rects = Vec::with_capacity(4);

                    text_and_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: ScreenSpaceRect {
                            min: (x0, y0),
                            max: (x3, y3),
                        },
                        color: first_colour,
                        z: next_smallest_f32_if_normal_or_0(first_z),
                    }));
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "first",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (x0, y1),
                                max: (x3, y3),
                            },
                            color: text_colour,
                            z: next_smallest_f32_if_normal_or_0(first_z),
                        },
                        layout: TextLayout::Wrap,
                    }));
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "1st",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (x1, y2),
                                max: (x3, y3),
                            },
                            color: text_colour,
                            z: next_smallest_f32_if_normal_or_0(next_smallest_f32_if_normal_or_0(
                                first_z,
                            )),
                        },
                        layout: TextLayout::Wrap,
                    }));

                    text_and_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: ScreenSpaceRect {
                            min: (x1, y1),
                            max: (x4, y4),
                        },
                        color: second_colour,
                        z: next_smallest_f32_if_normal_or_0(second_z),
                    }));
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "second",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (x1, y1),
                                max: (x4, y4),
                            },
                            color: text_colour,
                            z: next_smallest_f32_if_normal_or_0(second_z),
                        },
                        layout: TextLayout::Wrap,
                    }));
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "2nd",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (x1, y2),
                                max: (x4, y4),
                            },
                            color: text_colour,
                            z: next_smallest_f32_if_normal_or_0(next_smallest_f32_if_normal_or_0(
                                second_z,
                            )),
                        },
                        layout: TextLayout::Wrap,
                    }));

                    let z_text = format!("{:#?}", (first_z, second_z));
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: &z_text,
                        size: HELP_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (x3, y0),
                                max: (x4, y1),
                            },
                            color: text_colour,
                            z: next_smallest_f32_if_normal_or_0(next_smallest_f32_if_normal_or_0(
                                second_z,
                            )),
                        },
                        layout: TextLayout::Wrap,
                    }));

                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "Press + or - and there should be no change to the apparent layers things are on.",
                        size: HELP_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (x0, y3),
                                max: (x1, y4),
                            },
                            color: text_colour,
                            z: next_smallest_f32_if_normal_or_0(next_smallest_f32_if_normal_or_0(
                                second_z,
                            )),
                        },
                        layout: TextLayout::Wrap,
                    }));

                    gl_layer::render(&mut gl_state, text_and_rects, width as _, height as _)
                        .expect("gl_layer::render didn't work");

                    glutin_context
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

                            *control_flow = glutin::event_loop::ControlFlow::Exit;
                        }};
                    }

                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        WindowEvent::Resized(size) => {
                            let window = glutin_context.window();
                            let hidpi_factor = window.hidpi_factor();
                            glutin_context.resize(size.to_physical(hidpi_factor));
                            let ls = window.inner_size();
                            dimensions = ls.to_physical(hidpi_factor);
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
                        } => {
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                }
                                VirtualKeyCode::Up => {
                                    //second_z = second_z.saturating_sub(1);
                                    second_z = next_smallest_f32_if_normal_or_0(second_z);
                                }
                                VirtualKeyCode::Down => {
                                    //second_z = second_z.saturating_add(1);
                                    second_z = next_largest_f32_if_normal_or_0(second_z);
                                }
                                VirtualKeyCode::Left => {
                                    //first_z = first_z.saturating_sub(1);
                                    first_z = next_smallest_f32_if_normal_or_0(first_z);
                                }
                                VirtualKeyCode::Right => {
                                    //first_z = first_z.saturating_add(1);
                                    first_z = next_largest_f32_if_normal_or_0(first_z);
                                }
                                // AKA plus
                                VirtualKeyCode::Equals => {
                                    //second_z = second_z.saturating_add(1);
                                    second_z = next_largest_f32_if_normal_or_0(second_z);
                                    //first_z = first_z.saturating_add(1);
                                    first_z = next_largest_f32_if_normal_or_0(first_z);
                                }
                                VirtualKeyCode::Minus => {
                                    //second_z = second_z.saturating_sub(1);
                                    second_z = next_smallest_f32_if_normal_or_0(second_z);
                                    //first_z = first_z.saturating_sub(1);
                                    first_z = next_smallest_f32_if_normal_or_0(first_z);
                                }
                                _ => (),
                            };
                            println!("{:?}", (first_z, second_z));
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }

    Ok(())
}
