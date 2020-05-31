/// This is an example/test that is used to ensure that the scroll related stuff exported by
/// `platform_types` works properly. This visual stuff is eaiser to verify visually.
use gl_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use glutin::{Api, GlProfile, GlRequest};
use platform_types::{screen_positioning::*, *};
use shared::Res;
use macros::{d, SaturatingSub};

use core::f32::{INFINITY};

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
                .with_title("scroll visualiztion example/test"),
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

    let first_colour = [0.0, 0.6, 0.6, 1.0];
    let text_colour = [0.9, 0.9, 0.9, 1.0];
    let second_colour = [0.6, 0.6, 0.0, 1.0];

    let error_colour = [0.6, 0.0, 0.0, 1.0];

    let first_z: u16 = 1 << 15;
    let second_z: u16 = 1 << 14;

    let apron = apron!(1.0);
    let move_amount: NonNegF32 = non_neg_f32!(16.0);

    // User manipulatable state
    let mut scroll_xy: ScrollXY = d!();
    let mut text_box_xywh = tbxywh!(
        dimensions.width as f32 * 0.25,
        dimensions.height as f32 * 0.25,
        dimensions.width as f32 / 2.0,
        dimensions.height as f32 / 2.0
    );
    let mut cursor_xy: TextSpaceXY = d!();
    let mut visibility_attempt = VisibilityAttemptResult::Succeeded;
    let mut attempt_count = 0;

    {
        events.run(move |event, _, control_flow| {
            use glutin::event::*;

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
                    let width = dimensions.width as f32;
                    let height = dimensions.height as f32;

                    let mut text_and_rects = Vec::with_capacity(16);

                    text_and_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: text_box_xywh.into(),
                        color: first_colour,
                        z: first_z,
                    }));
                    let first_z_add_1 = first_z.saturating_add(1);
                    /*text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: if xy_is_visible(cursor_xy) {
                            "is visible"
                        } else {
                            "is not visible"
                        },
                        size: TEXT_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (dimensions.width as f32 / 2.0, 0.0),
                                max: (dimensions.width as f32, dimensions.height as f32),
                            },
                            color: text_colour,
                            z: first_z_add_1,
                        },
                        layout: TextLayout::Wrap,
                    }));*/

                    let text_space_ssr = ssxywh!(
                        text_space_to_screen_space(
                            scroll_xy,
                            text_box_xywh.xy,
                            d!()
                        ),
                        text_box_xywh.wh
                    ).into();

                    text_and_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: text_space_ssr,
                        color: second_colour,
                        z: second_z,
                    }));
                    
                    let cursor_screen_xy = text_space_to_screen_space(
                        scroll_xy,
                        text_box_xywh.xy,
                        cursor_xy
                    );

                    let text_box_ssr = ssxywh!(
                        text_box_xywh.xy.into(),
                        text_box_xywh.wh
                    ).into();

                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: "▏",
                        size: HELP_SIZE,
                        spec: VisualSpec {
                            rect: ssxywh!(
                                cursor_screen_xy,
                                text_box_xywh.wh
                            ).into(),
                            color: if inside_rect(cursor_screen_xy, text_box_ssr) { 
                                text_colour 
                            } else { 
                                error_colour 
                            },
                            z: first_z_add_1,
                        },
                        layout: TextLayout::Wrap,
                    }));

                    let info_text = &format!(
                        "scroll_xy {:?}\ntext_box_xywh: {:?} {:?}\ncursor_xy: {:?}\nattempt #{} {:?}\nSpacebar to make attempt to make the cursor visible.\nArrows or scroll wheel along with various combinations of Ctrl and Shift to move things",
                        scroll_xy,
                        text_box_xywh.xy,
                        text_box_xywh.wh,
                        cursor_xy,
                        attempt_count,
                        visibility_attempt, 
                    );

                    text_and_rects.push(TextOrRect::Text(TextSpec {
                        text: info_text,
                        size: HELP_SIZE,
                        spec: VisualSpec {
                            rect: ScreenSpaceRect {
                                min: (
                                    0.0,
                                    dimensions.height as f32
                                    - (info_text.lines().count() as f32 * HELP_SIZE)
                                ),
                                max: (dimensions.width as f32, dimensions.height as f32),
                            },
                            color: text_colour,
                            ..d!()
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
                        } if modifiers.shift() && !modifiers.ctrl() => {
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                }
                                VirtualKeyCode::Up => {
                                    text_box_xywh.xy.y -= move_amount;
                                }
                                VirtualKeyCode::Down => {
                                    text_box_xywh.xy.y += move_amount;                                }
                                VirtualKeyCode::Left => {
                                    text_box_xywh.xy.x -= move_amount;
                                }
                                VirtualKeyCode::Right => {
                                    text_box_xywh.xy.x += move_amount;
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
                        } if !modifiers.shift() && modifiers.ctrl() => {
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                }
                                VirtualKeyCode::Up => {
                                    text_box_xywh.wh.h = pos_f32_trunc!(
                                        text_box_xywh.wh.h.get() - move_amount.get()
                                    );
                                }
                                VirtualKeyCode::Down => {
                                    text_box_xywh.wh.h = pos_f32_trunc!(
                                        text_box_xywh.wh.h.get() + move_amount.get()
                                    );                                }
                                VirtualKeyCode::Left => {
                                    text_box_xywh.wh.w = pos_f32_trunc!(
                                        text_box_xywh.wh.w.get() - move_amount.get()
                                    );
                                }
                                VirtualKeyCode::Right => {
                                    text_box_xywh.wh.w = pos_f32_trunc!(
                                        text_box_xywh.wh.w.get() + move_amount.get()
                                    );
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
                        } if modifiers.is_empty() => {
                            match keypress {
                                VirtualKeyCode::Escape => {
                                    quit!();
                                }
                                VirtualKeyCode::Up => {
                                    cursor_xy.y -= move_amount;
                                }
                                VirtualKeyCode::Down => {
                                    cursor_xy.y += move_amount;                                }
                                VirtualKeyCode::Left => {
                                    cursor_xy.x -= move_amount;
                                }
                                VirtualKeyCode::Right => {
                                    cursor_xy.x += move_amount;
                                }
                                // AKA plus
                                VirtualKeyCode::Equals => {
                                    
                                }
                                VirtualKeyCode::Minus => {
                                    
                                }
                                VirtualKeyCode::Space => {
                                    visibility_attempt = attempt_to_make_xy_visible(
                                        &mut scroll_xy,
                                        text_box_xywh,
                                        apron,
                                        cursor_xy,
                                    );
                                    attempt_count += 1;
                                }
                                _ => (),
                            };
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers,
                            ..
                        } if modifiers.is_empty() => {
                            scroll_xy.y += y * 16.0;
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers,
                            ..
                        } if modifiers.shift() => {
                            scroll_xy.x += y * 16.0;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }
}
