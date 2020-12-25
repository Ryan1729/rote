/// This is an example/test that was originally made wit hthe intent of demonstrating a bug where 
/// the colour of text, when changed from one colour to another and back again, would get stuck,
/// and it would not change when it was expected to.
use gl_layer::{ColouredText, TextLayout, TextOrRect, TextSpec, VisualSpec};
use glutin::{Api, GlProfile, GlRequest};
use platform_types::*;
use shared::Res;

macro_rules! d {
    () => {
        Default::default()
    };
}

fn main() -> Res<()> {
    let events = glutin::event_loop::EventLoop::new();
    let glutin_context = glutin::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            glutin::window::WindowBuilder::new()
                .with_inner_size(
                    glutin::dpi::Size::Logical(glutin::dpi::LogicalSize::new(683.0, 393.0))
                )
                .with_title("text-colour-changes example"),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    const TEXT_SIZE: f32 = 128.0;
    const HELP_SIZE: f32 = 16.0;

    const TEXT_SIZES: [f32; 2] = [TEXT_SIZE, HELP_SIZE];

    let mut hidpi_factor = 1.0;

    let (mut gl_state, _) = gl_layer::init(
        hidpi_factor as f32,
        &TEXT_SIZES,
        [0.3, 0.3, 0.3, 1.0],
        |symbol| glutin_context.get_proc_address(symbol) as _,
    )?;

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(250.0);

    let mut running = true;
    let mut dimensions = glutin_context
        .window()
        .inner_size();

    /// All I claim about this is that it favours dimmer colours sometimes.
    macro_rules! grey_scale_dim {
        ($colour: expr) => {{
            let colour = $colour;
            let new_colour = if colour[0] < colour[1] {
                colour[0]
            } else if colour[1] < colour[2] {
                colour[1]
            } else {
                colour[2]
            };
            [
                new_colour,
                new_colour,
                new_colour,
                colour[3],
            ]
        }};
    }
    /// All I claim about this is that it favours brighter colours sometimes.
    macro_rules! grey_scale_bright {
        ($colour: expr) => {{
            let colour = $colour;
            let new_colour = if colour[0] > colour[1] {
                colour[0]
            } else if colour[1] > colour[2] {
                colour[1]
            } else {
                colour[2]
            };
            [
                new_colour,
                new_colour,
                new_colour,
                colour[3],
            ]
        }};
    }

    let first_colour = [0.0, 0.6, 0.6, 1.0];
    let text_colour = [0.9, 0.9, 0.9, 1.0];
    let second_colour = [0.6, 0.6, 0.0, 1.0];

    // User manipulatable state
    let mut first_z: u16 = 0;
    let mut second_z: u16 = first_z + 1;
    let mut alt_colour = false;
    let mut auto_advance = false;

    {
        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            macro_rules! advance {
                () => {
                    second_z = second_z.saturating_add(1);
                    first_z = first_z.saturating_add(1);
                    alt_colour = !alt_colour;
                };
            }

            // As of this writing, issues on https://github.com/rust-windowing/winit ,
            // specifically #1124 and #883, suggest that the it is up in the air as to
            // whether the modifiers field on some of the matches below will actually
            // be eventually removed or not. So, in the meantime, I choose the path 
            // that is the least work right now, since it seems unlikely for the amount
            // of work it will be later to grow significantly. Time will tell.
            #[allow(deprecated)]
            match event {
                Event::MainEventsCleared if running => {
                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                }
                Event::RedrawRequested(_) => {
                    if auto_advance {
                        advance!();
                    }

                    let width = dimensions.width as f32;
                    let height = dimensions.height as f32;

                    // The idea here is that we impose a 4 by 4 grid on the screen
                    // with the upper left corner notated as (0, 0) and the lower right as (4, 4).
                    let x0 = 0.0;
                    let y0 = 0.0;

                    let x1 = width / 4.0;
                    let y1 = height / 4.0;

                    #[allow(unused_variables)]
                    let x2 = x1 * 2.0;
                    let y2 = y1 * 2.0;

                    let x3 = x1 * 3.0;
                    let y3 = y1 * 3.0;

                    let x4 = width;
                    let y4 = height;

                    let mut text_and_rects = Vec::with_capacity(4);

                    text_and_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x0, y0),
                            max: ssxy!(x3, y3),
                        },
                        colour: first_colour,
                        z: first_z,
                    }));
                    let first_z_sub_1 = first_z.saturating_sub(1);
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "first",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(x0, y1),
                                max: ssxy!(x3, y3),
                            },
                            colour: text_colour,
                            z: first_z_sub_1,
                        },
                        layout: TextLayout::Unbounded,
                    }));
                    let first_z_sub_2 = first_z.saturating_sub(2);
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "1st",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(x1, y2),
                                max: ssxy!(x3, y3),
                            },
                            colour: text_colour,
                            z: first_z_sub_2,
                        },
                        layout: TextLayout::Unbounded,
                    }));

                    text_and_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: ScreenSpaceRect {
                            min: ssxy!(x1, y1),
                            max: ssxy!(x4, y4),
                        },
                        colour: second_colour,
                        z: second_z,
                    }));
                    let second_z_sub_1 = second_z.saturating_sub(1);
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "second",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(x1, y1),
                                max: ssxy!(x4, y4),
                            },
                            colour: text_colour,
                            z: second_z_sub_1,
                        },
                        layout: TextLayout::Unbounded,
                    }));
                    let second_z_sub_2 = second_z.saturating_sub(2);
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "2nd",
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(x1, y2),
                                max: ssxy!(x4, y4),
                            },
                            colour: text_colour,
                            z: second_z_sub_2,
                        },
                        layout: TextLayout::Unbounded,
                    }));

                    let z_text = format!(
                        "{},{},{}\n{},{},{}{:#?}",
                        first_z,
                        first_z_sub_1,
                        first_z_sub_2,
                        second_z,
                        second_z_sub_1,
                        second_z_sub_2,
                        (gl_layer::z_to_f32(first_z), gl_layer::z_to_f32(second_z))
                    );
                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: &z_text,
                        size: HELP_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(x3, y0),
                                max: ssxy!(x4, y1),
                            },
                            colour: text_colour,
                            ..d!()
                        },
                        layout: TextLayout::Unbounded,
                    }));

                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "Press + or - and there should be no change to the apparent layers things are on.",
                        size: HELP_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: ssxy!(x0, y3),
                                max: ssxy!(x1, y4),
                            },
                            colour: text_colour,
                            ..d!()
                        },
                        layout: TextLayout::Unbounded,
                    }));

                    if alt_colour {
                        for t_or_r in text_and_rects.iter_mut() {
                            use TextOrRect::*;
                            match t_or_r {
                                Rect(ref mut spec) => {
                                    spec.colour = grey_scale_dim!(spec.colour);
                                },
                                Text(ref mut spec) => {
                                    spec.spec.colour = grey_scale_bright!(spec.spec.colour);
                                },
                                MulticolourText(ref mut spec) => {
                                    for ColouredText { ref mut colour, .. } in spec.text.iter_mut() {
                                        *colour = grey_scale_bright!(*colour)
                                    }
                                }
                            }
                        }
                    }

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
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            hidpi_factor = scale_factor;
                        }
                        WindowEvent::Resized(size) => {
                            glutin_context.resize(size);
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
                                    modifiers,
                                    ..
                                },
                            ..
                        } if modifiers.shift() => {
                            const SHIFT: u16 = 65535 / 16;
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                }
                                VirtualKeyCode::Up => {
                                    second_z = second_z.saturating_sub(SHIFT);
                                }
                                VirtualKeyCode::Down => {
                                    second_z = second_z.saturating_add(SHIFT);
                                }
                                VirtualKeyCode::Left => {
                                    first_z = first_z.saturating_sub(SHIFT);
                                }
                                VirtualKeyCode::Right => {
                                    first_z = first_z.saturating_add(SHIFT);
                                }
                                VirtualKeyCode::Return => {
                                    alt_colour = !alt_colour;
                                }
                                _ => {}
                            }
                        },
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    modifiers,
                                    ..
                                },
                            ..
                        } if !modifiers.shift() => {
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                }
                                VirtualKeyCode::Up => {
                                    second_z = second_z.saturating_sub(1);
                                }
                                VirtualKeyCode::Down => {
                                    second_z = second_z.saturating_add(1);
                                }
                                VirtualKeyCode::Left => {
                                    first_z = first_z.saturating_sub(1);
                                }
                                VirtualKeyCode::Right => {
                                    first_z = first_z.saturating_add(1);
                                }
                                // AKA plus
                                VirtualKeyCode::Equals => {
                                    advance!();
                                }
                                VirtualKeyCode::Minus => {
                                    second_z = second_z.saturating_sub(1);
                                    first_z = first_z.saturating_sub(1);
                                }
                                VirtualKeyCode::Space => {
                                    auto_advance = !auto_advance;
                                }
                                VirtualKeyCode::Return => {
                                    alt_colour = !alt_colour;
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
}
