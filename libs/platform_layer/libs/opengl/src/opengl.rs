// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin::dpi::LogicalPosition;
use glutin::{Api, GlProfile, GlRequest};
use glyph_brush::{
    rusttype::Error as FontError, rusttype::Font, rusttype::Scale, Bounds, GlyphBrush,
    GlyphBrushBuilder, HighlightRange, Layout, PixelCoords, Section,
};
use std::path::PathBuf;


use file_chooser;
use gl_layer::RenderExtras;
use macros::d;
use platform_types::{
    BufferView, BufferViewKind, CharDim, Cmd, Highlight, Input, ReplaceOrAdd, ScreenSpaceWH,
    ScreenSpaceXY, Sizes, UpdateAndRender, View,
};

pub struct FontInfo<'a> {
    font: Font<'a>,
    text_scale: Scale,
    text_char_dim: CharDim,
    status_scale: Scale,
    status_char_dim: CharDim,
}

impl FontInfo<'static> {
    pub fn new(hidpi_factor: f32) -> Result<Self, FontError> {
        const FONT_BYTES: &[u8] = include_bytes!("./fonts/FiraCode-Retina-plus-CR-and-LF.ttf");
        let font: Font<'static> = Font::from_bytes(FONT_BYTES)?;
        let text_size: f32 = 60.0;
        let status_size: f32 = 22.0;

        let text_scale = Scale::uniform((text_size * hidpi_factor).round());
        let status_scale = Scale::uniform((status_size * hidpi_factor).round());

        macro_rules! get_char_dim {
            ($scale:ident) => {
                CharDim {
                    w: {
                        // We currently assume the font is monospaced.
                        let em_space_char = '\u{2003}';
                        let h_metrics = font.glyph(em_space_char).scaled($scale).h_metrics();

                        h_metrics.advance_width
                    },
                    h: {
                        let v_metrics = font.v_metrics($scale);

                        v_metrics.ascent + -v_metrics.descent + v_metrics.line_gap
                    },
                }
            };
        }

        let text_char_dim = get_char_dim!(text_scale);
        let status_char_dim = get_char_dim!(status_scale);

        Ok(Self {
            font,
            text_scale,
            text_char_dim,
            status_scale,
            status_char_dim,
        })
    }
}

pub fn get_glyph_brush<'font, A: Clone>(font_info: &FontInfo<'font>) -> GlyphBrush<'font, A> {
    GlyphBrushBuilder::using_font(font_info.font.clone())
        // Leaving this at the default of 0.1 makes the cache get cleared too often.
        // Putting this at 1.0 means that the characters are visibly poorly kerned.
        // This value seems like a happy medium at the moment.
        .gpu_cache_position_tolerance(0.25)
        .build()
}

mod clipboard_layer {
    pub use clipboard::ClipboardProvider;
    /// This enum exists so we can do dynamic dispatch on `ClipboardProvider` instances even though
    /// the trait requires `Sized`. The reason  we want to do that, is so that if we try to run this
    /// on a platform where `clipboard::ClipboardContext::new` retirns an `Err` we can continue
    /// operation, just without system clipboard support.
    pub enum Clipboard {
        System(clipboard::ClipboardContext),
        Fallback(clipboard::nop_clipboard::NopClipboardContext),
    }

    impl clipboard::ClipboardProvider for Clipboard {
        fn new() -> Result<Self, Box<std::error::Error>> {
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
        fn get_contents(&mut self) -> Result<String, Box<std::error::Error>> {
            match self {
                Clipboard::System(ctx) => ctx.get_contents(),
                Clipboard::Fallback(ctx) => ctx.get_contents(),
            }
        }
        fn set_contents(&mut self, s: String) -> Result<(), Box<std::error::Error>> {
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
pub fn run(update_and_render: UpdateAndRender) -> gl_layer::Res<()> {
    run_inner(update_and_render)
}

// This extra fn is a workaround for the record attribute causing a "procedural macros cannot
// expand to macro definitions" error otherwise.According to issue #54727, this is because there
// is some worry that all the macro hygiene edge cases may not be handled.
fn run_inner(update_and_render: UpdateAndRender) -> gl_layer::Res<()> {
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

    let mut clipboard: Clipboard = get_clipboard();

    #[derive(Clone, Debug)]
    enum CustomEvent {
        OpenFile(PathBuf),
    }
    unsafe impl Send for CustomEvent {}
    unsafe impl Sync for CustomEvent {}

    use glutin::event_loop::EventLoop;
    let events: EventLoop<CustomEvent> = glutin::event_loop::EventLoop::new_user_event();
    let event_proxy = events.create_proxy();
    let title = "rote";

    let glutin_context = glutin::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .build_windowed(
            glutin::window::WindowBuilder::new()
                .with_inner_size((1024, 576).into())
                .with_title(title),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    let scroll_multiplier: f32 = 16.0;
    let font_info = FontInfo::new(glutin_context.window().hidpi_factor() as f32)?;

    let mut glyph_brush = get_glyph_brush(&font_info);

    let mut gl_state = gl_layer::init(&glyph_brush, |symbol| {
        glutin_context.get_proc_address(symbol) as _
    })?;

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(250.0);

    let mut running = true;
    let mut dimensions = glutin_context
        .window()
        .inner_size()
        .to_physical(glutin_context.window().hidpi_factor());

    let (mut view, mut cmd) = update_and_render(Input::SetSizes(Sizes! {
        screen: ScreenSpaceWH { w: dimensions.width as f32, h: dimensions.height as f32 },
        text_char_dim: font_info.text_char_dim,
        status_char_dim: font_info.status_char_dim,
    }));

    // If you didn't click on the same symbol, counting that as a double click seems like it
    // would be annoying.
    let mouse_epsilon_radius: f32 = {
        let (w, h) = (font_info.text_char_dim.w, font_info.text_char_dim.h);

        (if w < h { w } else { h }) / 2.0
    };

    let (mut mouse_x, mut mouse_y) = (0.0, 0.0);
    let (mut last_click_x, mut last_click_y) = (std::f32::NAN, std::f32::NAN);

    macro_rules! mouse_within_radius {
        () => {
            (last_click_x - mouse_x).abs() <= mouse_epsilon_radius
                && (last_click_y - mouse_y).abs() <= mouse_epsilon_radius
        };
    }

    use std::sync::mpsc::channel;

    // into the editor thread
    let (in_tx, in_rx) = channel();
    // out of the editor thread
    let (out_tx, out_rx) = channel();

    let mut join_handle = Some(
        std::thread::Builder::new()
            .name("editor".to_string())
            .spawn(move || {
                while let Ok(input) = in_rx.recv() {
                    let was_quit = Input::Quit == input;
                    let pair = update_and_render(input);
                    let _hope_it_gets_there = out_tx.send(pair);
                    if was_quit {
                        return;
                    }
                }
            })
            .expect("Could not start editor thread!"),
    );

    let mut mouse_state = glutin::event::ElementState::Released;
    {
        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            macro_rules! call_u_and_r {
                ($input:expr) => {
                    let _hope_it_gets_there = in_tx.send($input);
                };
            }

            // eventually we'll likely want to tell the editor, and have it decide whether/how
            // to display it to the user.
            macro_rules! handle_platform_error {
                ($err: expr) => (
                    eprintln!("{}", $err);
                );
            }

            match event {
                Event::EventsCleared if running => {
                    match out_rx.try_recv() {
                        Ok((v, c)) => {
                            view = v;
                            cmd = c;
                        }
                        _ => {}
                    };

                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                }
                Event::WindowEvent {
                    event: WindowEvent::RedrawRequested,
                    ..
                } => {
                    let width = dimensions.width as u32;
                    let height = dimensions.height as f32;

                    {
                        let extras = render_buffer_view(&mut glyph_brush, &view, &font_info);

                        gl_layer::render(
                            &mut gl_state,
                            &mut glyph_brush,
                            width as _,
                            height as _,
                            extras,
                        )
                        .expect("gl_layer::render didn't work");
                    }

                    glutin_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");

                    if let Some(rate) = loop_helper.report_rate() {
                        glutin_context.window().set_title(&format!(
                            "{} {:.0} FPS {:?} click {:?}",
                            title,
                            rate,
                            (mouse_x, mouse_y),
                            (last_click_x, last_click_y),
                        ));
                    }

                    match cmd.take() {
                        Cmd::SetClipboard(s) => {
                            if let Err(err) = clipboard.set_contents(s) {
                                handle_platform_error!(err);
                            }
                        }
                        Cmd::NoCmd => {}
                    }

                    perf_viz::start_record!("sleepin'");
                    loop_helper.loop_sleep();
                    perf_viz::end_record!("sleepin'");

                    perf_viz::end_record!("main loop");
                    perf_viz::start_record!("main loop");

                    // We want to track the time that the message loop takes too!
                    loop_helper.loop_start();
                }
                Event::UserEvent(e) => {
                    match e {
                        CustomEvent::OpenFile(p) => {
                            match std::fs::read_to_string(&p) {
                                Ok(s) => { call_u_and_r!(Input::LoadedFile(p, s)); }
                                Err(err) => {handle_platform_error!(err);}
                            }
                        }
                    }
                }
                Event::NewEvents(StartCause::Init) => {
                    // At least try to measure the first frame accurately
                    perf_viz::start_record!("main loop");
                    loop_helper.loop_start();
                }
                Event::WindowEvent { event, .. } => {
                    macro_rules! quit {
                        () => {{
                            perf_viz::end_record!("main loop");
                            call_u_and_r!(Input::Quit);
                            running = false;

                            // If we got here, we assume that we've sent a Quit input to the editor thread so it will stop.
                            match join_handle.take() {
                                Some(j_h) => j_h.join().expect("Could not join editor thread!"),
                                None => {}
                            };

                            perf_viz::output!();

                            let _ = gl_layer::cleanup(&gl_state);

                            *control_flow = glutin::event_loop::ControlFlow::Exit;
                        }};
                    }

                    if cfg!(feature = "print-raw-input") {
                        match &event {
                            &WindowEvent::KeyboardInput { ref input, .. } => {
                                println!(
                                    "{:?}",
                                    (
                                        input.virtual_keycode.unwrap_or(VirtualKeyCode::WebStop),
                                        input.state
                                    )
                                );
                            }
                            _ => {}
                        }
                    }

                    use platform_types::Move;
                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        WindowEvent::Resized(size) => {
                            let window = glutin_context.window();
                            let dpi = window.hidpi_factor();
                            glutin_context.resize(size.to_physical(dpi));
                            let ls = window.inner_size();
                            dimensions = ls.to_physical(dpi);
                            call_u_and_r!(Input::SetSizes(Sizes! {
                                screen: ScreenSpaceWH { w: dimensions.width as f32, h: dimensions.height as f32 },
                                text_char_dim: None,
                                status_char_dim: None,
                            }));
                            gl_layer::set_dimensions(dimensions.width as _, dimensions.height as _);

                            //if we don't reset the cache like this then we render a stretched
                            //version of the text on windoe resize.
                            let (t_w, t_h) = glyph_brush.texture_dimensions();
                            glyph_brush.resize_texture(t_w, t_h);
                        }
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    modifiers:
                                        ModifiersState {
                                            ctrl: true,
                                            shift: false,
                                            alt: true,
                                            ..
                                        },
                                    ..
                                },
                            ..
                        } => match keypress {
                            VirtualKeyCode::Key0 => {
                                call_u_and_r!(Input::InsertNumbersAtCursors);
                            }
                            _ => (),
                        },
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    modifiers:
                                        ModifiersState {
                                            ctrl: true,
                                            shift: false,
                                            alt: false,
                                            ..
                                        },
                                    ..
                                },
                            ..
                        } => match keypress {
                            VirtualKeyCode::Key0 => {
                                call_u_and_r!(Input::ResetScroll);
                            }
                            VirtualKeyCode::Home => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToBufferStart));
                            }
                            VirtualKeyCode::End => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToBufferEnd));
                            }
                            VirtualKeyCode::Left => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToPreviousLikelyEditLocation));
                            }
                            VirtualKeyCode::Right => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToNextLikelyEditLocation));
                            }
                            VirtualKeyCode::A => {
                                call_u_and_r!(Input::SelectAll);
                            }
                            VirtualKeyCode::C => {
                                call_u_and_r!(Input::Copy);
                            }
                            VirtualKeyCode::O => {
                                println!("VirtualKeyCode::O");
                                let proxy = std::sync::Arc::new(
                                    std::sync::Mutex::new(event_proxy.clone())
                                );
                                let proxy = proxy.clone();
                                file_chooser::single(move |p: PathBuf| {
                                    let _bye = proxy.lock()
                                        .expect("file_chooser thread private mutex locked!?")
                                        .send_event(CustomEvent::OpenFile(p));
                                })
                            }
                            VirtualKeyCode::V => {
                                call_u_and_r!(Input::Paste(clipboard.get_contents().ok()));
                            }
                            VirtualKeyCode::X => {
                                call_u_and_r!(Input::Cut);
                            }
                            VirtualKeyCode::Y => {
                                call_u_and_r!(Input::Redo);
                            }
                            VirtualKeyCode::Z => {
                                call_u_and_r!(Input::Undo);
                            }
                            _ => (),
                        },
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    modifiers:
                                        ModifiersState {
                                            ctrl: true,
                                            shift: true,
                                            ..
                                        },
                                    ..
                                },
                            ..
                        } => match keypress {
                            VirtualKeyCode::Home => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(
                                    Move::ToBufferStart
                                ));
                            }
                            VirtualKeyCode::End => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(
                                    Move::ToBufferEnd
                                ));
                            }
                            VirtualKeyCode::Left => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::ToPreviousLikelyEditLocation));
                            }
                            VirtualKeyCode::Right => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::ToNextLikelyEditLocation));
                            }
                            VirtualKeyCode::Z => {
                                call_u_and_r!(Input::Redo);
                            }
                            _ => (),
                        },
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    modifiers:
                                        ModifiersState {
                                            ctrl: false,
                                            shift: false,
                                            ..
                                        },
                                    ..
                                },
                            ..
                        } => match keypress {
                            VirtualKeyCode::Escape => {
                                quit!();
                            }
                            VirtualKeyCode::Back => {
                                call_u_and_r!(Input::Delete);
                            }
                            VirtualKeyCode::Up => {
                                call_u_and_r!(Input::MoveAllCursors(Move::Up));
                            }
                            VirtualKeyCode::Down => {
                                call_u_and_r!(Input::MoveAllCursors(Move::Down));
                            }
                            VirtualKeyCode::Left => {
                                call_u_and_r!(Input::MoveAllCursors(Move::Left));
                            }
                            VirtualKeyCode::Right => {
                                call_u_and_r!(Input::MoveAllCursors(Move::Right));
                            }
                            VirtualKeyCode::Home => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToLineStart));
                            }
                            VirtualKeyCode::End => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToLineEnd));
                            }
                            _ => (),
                        },
                        WindowEvent::KeyboardInput {
                            input:
                                KeyboardInput {
                                    state: ElementState::Pressed,
                                    virtual_keycode: Some(keypress),
                                    modifiers:
                                        ModifiersState {
                                            ctrl: false,
                                            shift: true,
                                            ..
                                        },
                                    ..
                                },
                            ..
                        } => match keypress {
                            VirtualKeyCode::Up => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::Up));
                            }
                            VirtualKeyCode::Down => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::Down));
                            }
                            VirtualKeyCode::Left => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::Left));
                            }
                            VirtualKeyCode::Right => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::Right));
                            }
                            VirtualKeyCode::Home => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(
                                    Move::ToLineStart
                                ));
                            }
                            VirtualKeyCode::End => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::ToLineEnd));
                            }
                            _ => (),
                        },
                        WindowEvent::ReceivedCharacter(mut c) => {
                            if
                             c != '\u{1}'       // "start of heading" (sent with Ctrl-a)
                             && c != '\u{3}'    // "end of text" (sent with Ctrl-c)
                             && c != '\u{8}'    // backspace
                             && c != '\u{f}'    // "shift in" AKA use black ink apparently, (sent with Ctrl-o)
                             && c != '\u{16}'  // "synchronous idle" (sent with Ctrl-v)
                             && c != '\u{18}'  // "cancel" (sent with Ctrl-x)
                             && c != '\u{19}'  // "end of medium" (sent with Ctrl-y)
                             && c != '\u{1a}'  // "substitute" (sent with Ctrl-z)
                             && c != '\u{7f}'   // delete
                            {
                                if c == '\r' {
                                    c = '\n';
                                }
                                call_u_and_r!(Input::Insert(c));
                            }
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers: ModifiersState { shift: false, .. },
                            ..
                        } => {
                            call_u_and_r!(Input::ScrollVertically(y * scroll_multiplier));
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers: ModifiersState { shift: true, .. },
                            ..
                        } => {
                            call_u_and_r!(Input::ScrollHorizontally(-y * scroll_multiplier));
                        }
                        WindowEvent::CursorMoved {
                            position: LogicalPosition { x, y },
                            modifiers,
                            ..
                        } => {
                            mouse_x = x as f32;
                            mouse_y = y as f32;


                            match modifiers {
                                ModifiersState {
                                    ctrl: false, shift: false, ..
                                } => {
                                    let mut cursor_icon = glutin::window::CursorIcon::Text;
                                    // If there were more than two kinds of cursor this would be more
                                    // complicated. This might be an argument for changing the view to
                                    // arrays of structs of each type instead. Or maybe some form that has
                                    // no order to it at all.
                                    for v in view.buffers.iter() {
                                        match v.kind {
                                            BufferViewKind::StatusLine => {
                                                if mouse_y >= v.screen_position.y {
                                                    cursor_icon = d!();
                                                }
                                            }
                                            _ => {}
                                        }
                                    }

                                    glutin_context.window().set_cursor_icon(cursor_icon);

                                    if mouse_state == ElementState::Pressed && !mouse_within_radius!() {
                                        call_u_and_r!(Input::DragCursors(ScreenSpaceXY {
                                            x: mouse_x,
                                            y: mouse_y
                                        }));
                                    }
                                }
                                _ => {}
                            }


                        }
                        WindowEvent::MouseInput {
                            button: MouseButton::Left,
                            state: ElementState::Pressed,
                            modifiers:
                                ModifiersState {
                                    ctrl,
                                    shift: false,
                                    ..
                                },
                            ..
                        } => {
                            mouse_state = ElementState::Pressed;

                            let replace_or_add = if ctrl {
                                ReplaceOrAdd::Add
                            } else {
                                ReplaceOrAdd::Replace
                            };

                            let input = if mouse_within_radius!() {
                                Input::SelectCharTypeGrouping(
                                    ScreenSpaceXY {
                                        x: mouse_x,
                                        y: mouse_y
                                    },
                                    replace_or_add
                                )
                            } else {
                                Input::SetCursor(
                                    ScreenSpaceXY {
                                        x: mouse_x,
                                        y: mouse_y
                                    },
                                    replace_or_add
                                )
                            };

                            call_u_and_r!(input);
                        }
                        WindowEvent::MouseInput {
                            button: MouseButton::Left,
                            state: ElementState::Released,
                            ..
                        } => {
                            mouse_state = ElementState::Released;
                            last_click_x = mouse_x;
                            last_click_y = mouse_y;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }
}

/// As of this writing, casting f32 to i32 is undefined behaviour if the value does not fit!
/// https://github.com/rust-lang/rust/issues/10184
fn f32_to_i32_or_max(f: f32) -> i32 {
    if f >= i32::min_value() as f32 && f <= i32::max_value() as f32 {
        f as i32
    } else {
        // make this the default so NaN etc. end up here
        i32::max_value()
    }
}

fn highlight_to_pixel_coords(
    &Highlight { min, max, .. }: &Highlight,
    (x, y): (f32, f32),
    CharDim { w, h }: CharDim,
) -> PixelCoords {
    let mut pixel_coords: PixelCoords = d!();

    pixel_coords.min.x = f32_to_i32_or_max(min.offset.0 as f32 * w + x);
    pixel_coords.min.y = f32_to_i32_or_max(min.line as f32 * h + y);
    pixel_coords.max.x = f32_to_i32_or_max(max.offset.0 as f32 * w + x);
    pixel_coords.max.y = f32_to_i32_or_max((max.line + 1) as f32 * h + y);

    pixel_coords
}

pub fn render_buffer_view<A: Clone>(
    glyph_brush: &mut GlyphBrush<A>,
    view: &View,
    FontInfo {
        text_scale,
        text_char_dim,
        status_scale,
        ..
    }: &FontInfo,
) -> RenderExtras {
    let mut status_line_position = None;
    let mut highlight_ranges = Vec::new();
    perf_viz::start_record!("for &BufferView");
    for &BufferView {
        kind,
        bounds,
        color,
        ref chars,
        screen_position,
        ref highlights,
    } in view.buffers.iter()
    {
        // Without a background the edit buffer(s) show through the status line(s)
        if let BufferViewKind::StatusLine = kind {
            status_line_position = Some(screen_position.into());
        }

        perf_viz::record_guard!("glyph_brush.queue");
        let text = {
            chars
            // perf_viz::record_guard!("map unprinatbles to symbols for themselves");
            // let s = chars
            //     .chars()
            //     .map(|c| {
            //         // map unprinatbles to symbols for themselves
            //         // if c < 0x20 as char {
            //         //     std::char::from_u32(c as u32 | 0x2400u32).unwrap_or(c)
            //         // } else {
            //         c
            //         // }
            //     })
            //     .collect::<String>();
            // s
        };
        glyph_brush.queue(Section {
            text: &text,
            scale: if let BufferViewKind::StatusLine = kind {
                *status_scale
            } else {
                *text_scale
            },
            screen_position: screen_position.into(),
            bounds,
            color,
            layout: Layout::default_wrap(),
            z: match kind {
                BufferViewKind::Edit => gl_layer::EDIT_Z,
                BufferViewKind::Cursor => gl_layer::CURSOR_Z,
                BufferViewKind::StatusLine => gl_layer::STATUS_Z,
            },
            ..Section::default()
        });

        let mut rect_bounds: Bounds = d!();
        rect_bounds.max = bounds.into();

        perf_viz::start_record!("highlight_ranges.extend");
        highlight_ranges.extend(highlights.iter().map(|h| HighlightRange {
            pixel_coords: highlight_to_pixel_coords(h, screen_position.into(), *text_char_dim),
            bounds: rect_bounds,
            color: h.color,
            z: gl_layer::HIGHLIGHT_Z,
        }));
        perf_viz::end_record!("highlight_ranges.extend");
    }
    perf_viz::end_record!("for &BufferView");

    RenderExtras {
        status_line_position,
        status_scale: *status_scale,
        highlight_ranges,
    }
}

#[cfg(test)]
mod tests;
