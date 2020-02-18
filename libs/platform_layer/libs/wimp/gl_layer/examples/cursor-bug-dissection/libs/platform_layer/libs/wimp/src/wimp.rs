// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin::{dpi::LogicalPosition, Api, GlProfile, GlRequest};
use std::collections::VecDeque;
use std::path::PathBuf;
use std::time::Duration;
use wimp_render::{get_find_replace_info, FindReplaceInfo, get_go_to_position_info, GoToPositionInfo};

use file_chooser;
use macros::d;
use platform_types::{screen_positioning::screen_to_text_box, *};
use shared::{transform_status, BufferStatus, BufferStatusMap, BufferStatusTransition, Res};
use wimp_render::{Navigation, PhysicalButtonState, UIState};

mod clipboard_layer {
    pub use clipboard::ClipboardProvider;
    use shared::Res;
    /// This enum exists so we can do dynamic dispatch on `ClipboardProvider` instances even though
    /// the trait requires `Sized`. The reason  we want to do that, is so that if we try to run this
    /// on a platform where `clipboard::ClipboardContext::new` retirns an `Err` we can continue
    /// operation, just without system clipboard support.
    pub enum Clipboard {
        System(clipboard::ClipboardContext),
        Fallback(clipboard::nop_clipboard::NopClipboardContext),
    }

    impl clipboard::ClipboardProvider for Clipboard {
        fn new() -> Res<Self> {
            let result: Result<
                clipboard::ClipboardContext,
                clipboard::nop_clipboard::NopClipboardContext,
            > = clipboard::ClipboardContext::new().map_err(|err| {
                eprintln!("System clipboard not supported. {}", err);
                // `NopClipboardContext::new` always returns an `Ok`
                clipboard::nop_clipboard::NopClipboardContext::new().unwrap()
            });

            let output = match result {
                Ok(ctx) => Clipboard::System(ctx),
                Err(ctx) => Clipboard::Fallback(ctx),
            };

            // `get_clipboard` currently relies on this neer returning `Err`.
            Ok(output)
        }
        fn get_contents(&mut self) -> Res<String> {
            match self {
                Clipboard::System(ctx) => ctx.get_contents(),
                Clipboard::Fallback(ctx) => ctx.get_contents(),
            }
        }
        fn set_contents(&mut self, s: String) -> Res<()> {
            match self {
                Clipboard::System(ctx) => ctx.set_contents(s),
                Clipboard::Fallback(ctx) => ctx.set_contents(s),
            }
        }
    }

    pub fn get_clipboard() -> Clipboard {
        // As you can see in the implementation of the `new` method, it always returns `Ok`
        Clipboard::new().unwrap()
    }
}
use clipboard_layer::{get_clipboard, Clipboard, ClipboardProvider};

#[perf_viz::record]
pub fn run(update_and_render: UpdateAndRender) -> Res<()> {
    const EVENTS_PER_FRAME: usize = 16;

    if cfg!(target_os = "linux") {
        use std::env;
        // winit wayland is currently still wip
        if env::var("WINIT_UNIX_BACKEND").is_err() {
            env::set_var("WINIT_UNIX_BACKEND", "x11");
        }
        // disables vsync sometimes on x11
        if env::var("vblank_mode").is_err() {
            env::set_var("vblank_mode", "0");
        }
    }

    let title = "dissection";

    let mut clipboard: Clipboard = get_clipboard();

    let events = glutin::event_loop::EventLoop::new();
    let event_proxy = events.create_proxy();

    let glutin_context = glutin::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            glutin::window::WindowBuilder::new()
                .with_inner_size(
                    glutin::dpi::Size::Logical(glutin::dpi::LogicalSize::new(1024.0, 576.0))
                 )
                .with_title(title),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    let mut current_hidpi_factor = 1.0;
    macro_rules! get_hidpi_factor {
        () => {
            current_hidpi_factor
        }
    }

    dbg!(glutin_context.get_pixel_format());

    let (mut gl_state, char_dims) = gl_layer::init(
        get_hidpi_factor!() as f32,
        &wimp_render::TEXT_SIZES,
        wimp_render::TEXT_BACKGROUND_COLOUR,
        |symbol| glutin_context.get_proc_address(symbol) as _,
    )?;

    let font_info = wimp_render::get_font_info(&char_dims);

    const TARGET_RATE: f64 = 128.0; //250.0);

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(TARGET_RATE);

    let mut running = true;
    let mut dimensions = glutin_context
        .window()
        .inner_size();

    macro_rules! screen_wh {
        () => {
            ScreenSpaceWH {
                w: dimensions.width as f32,
                h: dimensions.height as f32,
            }
        };
    }

    macro_rules! get_non_font_size_dependents {
        ($mode: expr) => {{
            let wh = screen_wh!();
            let FindReplaceInfo {
                find_text_xywh,
                replace_text_xywh,
                ..
            } = get_find_replace_info(font_info, wh);
            let GoToPositionInfo {
                input_text_xywh,
                ..
            } = get_go_to_position_info(font_info, wh);
            SizeDependents {
                buffer_xywh: wimp_render::get_edit_buffer_xywh(
                    $mode,
                    font_info,
                    screen_wh!()
                )
                .into(),
                find_xywh: find_text_xywh.into(),
                replace_xywh: replace_text_xywh.into(),
                go_to_position_xywh: input_text_xywh.into(),
                font_info: font_info.into(),
            }
        }};
    }

    let (mut view, mut cmds) = {
        let (v, c) = update_and_render(Input::SetSizeDependents(
            get_non_font_size_dependents!(d!())
        ));

        let mut cs = VecDeque::with_capacity(EVENTS_PER_FRAME);
        cs.push_back(c);

        (v, cs)
    };

    // If you didn't click on the same symbol, counting that as a double click seems like it
    // would be annoying.
    let mouse_epsilon_radius: f32 = {
        let (w, h) = (font_info.text_char_dim.w, font_info.text_char_dim.h);

        (if w < h { w } else { h }) / 2.0
    };

    let (mut last_click_x, mut last_click_y) = (std::f32::NAN, std::f32::NAN);

    let mut ui: UIState = d!();

    let mut dt = Duration::from_nanos(((1.0 / TARGET_RATE) * 1_000_000_000.0) as u64);

    macro_rules! mouse_within_radius {
        () => {
            false
        };
    }

    use std::sync::mpsc::channel;
    enum EditedFilesThread {
        Quit,
        Buffers(g_i::State, Vec<edited_storage::BufferInfo>),
    }

    // into the editor thread
    let (editor_in_sink, editor_in_source) = channel();
    // out of the editor thread
    let (editor_out_sink, editor_out_source) = channel();

    let mut editor_join_handle = Some(
        std::thread::Builder::new()
            .name("editor".to_string())
            .spawn(move || {
                while let Ok(input) = editor_in_source.recv() {
                    let was_quit = Input::Quit == input;
                    let pair = update_and_render(input);
                    let _hope_it_gets_there = editor_out_sink.send(pair);
                    if was_quit {
                        return;
                    }
                }
            })
            .expect("Could not start editor thread!"),
    );

    {
        macro_rules! call_u_and_r {
            ($input:expr) => {
                ui.note_interaction();
            };
        }

        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            // eventually we'll likely want to tell the editor, and have it decide whether/how
            // to display it to the user.
            macro_rules! handle_platform_error {
                ($err: expr) => {
                    let error = format!("{},{}: {}", file!(), line!(), $err);
                    eprintln!("{}", error);
                    call_u_and_r!(Input::NewScratchBuffer(Some(error)));
                };
            }

            match event {
                Event::MainEventsCleared if running => {
                    for _ in 0..EVENTS_PER_FRAME {
                        match editor_out_source.try_recv() {
                            Ok((v, c)) => {
                                view = v;
                                cmds.push_back(c);
                            }
                            _ => break,
                        };
                    }

                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                }
                Event::RedrawRequested(_) => {
                    ui.frame_init();
                    if_changed::dbg!(&ui.keyboard);

                    let (text_and_rects, input) =
                        wimp_render::view(
                            &mut ui,
                            &view,
                            &font_info,
                            screen_wh!(),
                            dt
                        );
                    let width = dimensions.width;
                    let height = dimensions.height;

                    gl_layer::render(&mut gl_state, text_and_rects, width as _, height as _)
                        .expect("gl_layer::render didn't work");

                    glutin_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");

                    if let Some(input) = input {
                        call_u_and_r!(input);
                    }

                    if let Some(rate) = loop_helper.report_rate() {
                        glutin_context.window().set_title(title);
                    }

                    ui.frame_end();
                    perf_viz::start_record!("sleepin'");
                    loop_helper.loop_sleep();
                    perf_viz::end_record!("sleepin'");

                    perf_viz::end_record!("main loop");
                    perf_viz::start_record!("main loop");

                    // We want to track the time that the message loop takes too!
                    dt = loop_helper.loop_start();
                }
                Event::NewEvents(StartCause::Init) => {
                    // At least try to measure the first frame accurately
                    perf_viz::start_record!("main loop");
                    dt = loop_helper.loop_start();
                }
                Event::WindowEvent { event, .. } => {
                    macro_rules! quit {
                        () => {{
                            perf_viz::end_record!("main loop");
                            call_u_and_r!(Input::Quit);

                            running = false;

                            // If we got here, we assume that we've sent a Quit input to the editor thread so it will stop.
                            match editor_join_handle.take() {
                                Some(j_h) => j_h.join().expect("Could not join editor thread!"),
                                None => {}
                            };

                            perf_viz::output!();

                            let _ = gl_layer::cleanup(&gl_state);

                            *control_flow = glutin::event_loop::ControlFlow::Exit;
                        }};
                    }

                    macro_rules! text_box_xy {
                        () => {{
                            let xy = wimp_render::get_current_buffer_rect(
                                view.current_buffer_id,
                                view.menu.get_mode(),
                                font_info,
                                screen_wh!(),
                            )
                            .xy;

                            d!()
                        }};
                    }

                    macro_rules! switch_menu_mode {
                        ($mode: expr) => {
                            let mode = $mode;

                            call_u_and_r!(Input::SetMenuMode(mode));

                            call_u_and_r!(Input::SetSizeDependents(SizeDependents {
                                buffer_xywh: wimp_render::get_edit_buffer_xywh(
                                    mode,
                                    font_info,
                                    screen_wh!()
                                )
                                .into(),
                                find_xywh: None,
                                replace_xywh: None,
                                go_to_position_xywh: None,
                                font_info: None,
                            }));
                        };
                    }

                    const ALT: ModifiersState = ModifiersState::ALT;
                    const CTRL: ModifiersState = ModifiersState::CTRL;
                    const SHIFT: ModifiersState = ModifiersState::SHIFT;
                    
                    // As of this writing, issues on https://github.com/rust-windowing/winit ,
                    // specifically #1124 and #883, suggest that the it is up in the air as to
                    // whether the modifiers field on some of the matches below will actually
                    // be eventually removed or not. So, in the meantime, I choose the path 
                    // that is the least work right now, since it seems unlikely for the amount
                    // of work it will be later to grow significantly. Time will tell.
                    #[allow(deprecated)]
                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        WindowEvent::ScaleFactorChanged {
                            scale_factor,
                            ..
                        } => {
                            current_hidpi_factor = scale_factor;
                        }
                        WindowEvent::Resized(size) => {
                            let hidpi_factor = get_hidpi_factor!();
                            glutin_context.resize(size);
                            dimensions = size;
                            call_u_and_r!(Input::SetSizeDependents(
                                get_non_font_size_dependents!(view.menu.get_mode())
                            ));
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (dimensions.width as _, dimensions.height as _),
                            );
                        }
                        WindowEvent::Focused(is_focused) => {
                            dbg!("set to ", is_focused);
                            ui.window_is_focused = is_focused;
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
                        } if modifiers.is_empty() => match if cfg!(debug_assertions) {dbg!(keypress)} else {keypress} {
                            VirtualKeyCode::Escape => {
                                call_u_and_r!(Input::SetSizeDependents(SizeDependents {
                                    buffer_xywh: wimp_render::get_edit_buffer_xywh(
                                        d!(),
                                        font_info,
                                        screen_wh!()
                                    )
                                    .into(),
                                    find_xywh: None,
                                    replace_xywh: None,
                                    go_to_position_xywh: None,
                                    font_info: None,
                                }));
                                call_u_and_r!(Input::CloseMenuIfAny);
                            }
                            _ => (),
                        },
                        WindowEvent::MouseInput {
                            button: MouseButton::Left,
                            state: ElementState::Pressed,
                            modifiers,
                            ..
                        } // allow things like Shift-Alt-Click
                        if (!modifiers).intersects(!CTRL) => {
                            let replace_or_add = if modifiers.ctrl() {
                                ReplaceOrAdd::Add
                            } else {
                                ReplaceOrAdd::Replace
                            };

                            call_u_and_r!(Input::SetCursor(text_box_xy!(), replace_or_add));
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        })
    }
}
