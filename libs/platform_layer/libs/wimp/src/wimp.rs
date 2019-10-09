// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin::{dpi::LogicalPosition, event::ElementState, Api, GlProfile, GlRequest};
use std::cmp::max;
use std::path::PathBuf;

use file_chooser;
use gl_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use macros::{c, d, ord};
use platform_types::*;
use shared::Res;

type Colour = [f32; 4];

const CHROME_BACKGROUND_COLOUR: Colour = c![7.0 / 256.0, 7.0 / 256.0, 7.0 / 256.0];

const TEXT_SIZE: f32 = 60.0;
const STATUS_SIZE: f32 = 22.0;
const TAB_SIZE: f32 = 16.0;

/// You can use any u8 as a base, and this function will make a z that allows UI widgets to use
/// some more layers for other stuff by adding small numbers to it. Say 1, 2, 3 etc.
const fn z_from_base(base: u8) -> u16 {
    (base as u16) << 8
}

// Reminder: smaller means closer
const EDIT_Z: u16 = z_from_base(1 << 7);
const HIGHLIGHT_Z: u16 = z_from_base(0b11 << 6);
const CURSOR_Z: u16 = z_from_base(1 << 6);
const STATUS_BACKGROUND_Z: u16 = z_from_base(1 << 5);
const TAB_Z: u16 = STATUS_BACKGROUND_Z;
const STATUS_Z: u16 = z_from_base(1 << 4);

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
    run_inner(update_and_render)
}

// This extra fn is a workaround for the record attribute causing a "procedural macros cannot
// expand to macro definitions" error otherwise.According to issue #54727, this is because there
// is some worry that all the macro hygiene edge cases may not be handled.
fn run_inner(update_and_render: UpdateAndRender) -> Res<()> {
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
        .with_depth_buffer(24)
        .build_windowed(
            glutin::window::WindowBuilder::new()
                .with_inner_size((1024, 576).into())
                .with_title(title),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    let scroll_multiplier: f32 = 16.0;

    let text_sizes = [TEXT_SIZE, STATUS_SIZE, TAB_SIZE];

    let (mut gl_state, char_dims) = gl_layer::init(
        glutin_context.window().hidpi_factor() as f32,
        &text_sizes,
        |symbol| glutin_context.get_proc_address(symbol) as _,
    )?;

    debug_assert!(
        char_dims.len() >= text_sizes.len(),
        "gl_layer::init didn't pass enough sizes"
    );

    let font_info = FontInfo {
        text_char_dim: char_dims[0],
        status_char_dim: char_dims[1],
        tab_char_dim: char_dims[2],
    };

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(250.0);

    let mut running = true;
    let mut dimensions = glutin_context
        .window()
        .inner_size()
        .to_physical(glutin_context.window().hidpi_factor());

    let (mut view, mut cmd) = update_and_render(Input::SetSizes(Sizes! {
        screen: ScreenSpaceWH { w: dimensions.width as f32, h: dimensions.height as f32 },
        font_info: font_info
    }));

    // If you didn't click on the same symbol, counting that as a double click seems like it
    // would be annoying.
    let mouse_epsilon_radius: f32 = {
        let (w, h) = (font_info.text_char_dim.w, font_info.text_char_dim.h);

        (if w < h { w } else { h }) / 2.0
    };

    let (mut last_click_x, mut last_click_y) = (std::f32::NAN, std::f32::NAN);

    let mut ui: UIState = d!();

    macro_rules! mouse_within_radius {
        () => {
            (last_click_x - ui.mouse_pos.x).abs() <= mouse_epsilon_radius
                && (last_click_y - ui.mouse_pos.y).abs() <= mouse_epsilon_radius
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

    {
        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            macro_rules! call_u_and_r {
                ($input:expr) => {
                    let _hope_it_gets_there = in_tx.send($input);
                };
            }

            // eventually we'll likely want to tell  the editor, and have it decide whether/how
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
                    ui.frame_init();

                    let width = dimensions.width;
                    let height = dimensions.height;

                    let (text_and_rects, input) = render_buffer_view(
                        &mut ui,
                        &view,
                        &font_info,
                        (width as _, height as _)
                    );

                    gl_layer::render(
                        &mut gl_state,
                        text_and_rects,
                        width as _,
                        height as _,
                    )
                    .expect("gl_layer::render didn't work");

                    glutin_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");

                    match cmd.take() {
                        Cmd::SetClipboard(s) => {
                            if let Err(err) = clipboard.set_contents(s) {
                                handle_platform_error!(err);
                            }
                        }
                        Cmd::NoCmd => {}
                    }

                    if let Some(input) = input{
                        call_u_and_r!(input);
                    }

                    if let Some(rate) = loop_helper.report_rate() {
                        glutin_context.window().set_title(&format!(
                            "{} {:.0} FPS {:?} click {:?}",
                            title,
                            rate,
                            (ui.mouse_pos.x, ui.mouse_pos.y),
                            (last_click_x, last_click_y),
                        ));
                    }

                    ui.frame_end();
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

                    match event {
                        WindowEvent::CloseRequested => quit!(),
                        WindowEvent::Resized(size) => {
                            let window = glutin_context.window();
                            let hidpi_factor = window.hidpi_factor();
                            glutin_context.resize(size.to_physical(hidpi_factor));
                            let ls = window.inner_size();
                            dimensions = ls.to_physical(hidpi_factor);
                            call_u_and_r!(Input::SetSizes(Sizes! {
                                screen: ScreenSpaceWH { w: dimensions.width as f32, h: dimensions.height as f32 },
                                font_info: None,
                            }));
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (dimensions.width as _, dimensions.height as _)
                            );
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
                            VirtualKeyCode::Tab => {
                                call_u_and_r!(Input::NextBuffer);
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
                            VirtualKeyCode::Tab => {
                                call_u_and_r!(Input::PreviousBuffer);
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
                            VirtualKeyCode::Tab => {
                                call_u_and_r!(Input::TabIn);
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
                            VirtualKeyCode::Tab => {
                                call_u_and_r!(Input::TabOut);
                            }
                            _ => (),
                        },
                        WindowEvent::ReceivedCharacter(mut c) => {
                            if
                             c != '\u{1}'       // "start of heading" (sent with Ctrl-a)
                             && c != '\u{3}'    // "end of text" (sent with Ctrl-c)
                             && c != '\u{8}'    // backspace
                             && c != '\u{9}'    // horizontal tab
                             && c != '\u{f}'    // "shift in" AKA use black ink apparently, (sent with Ctrl-o)
                             && c != '\u{16}'   // "synchronous idle" (sent with Ctrl-v)
                             && c != '\u{18}'   // "cancel" (sent with Ctrl-x)
                             && c != '\u{19}'   // "end of medium" (sent with Ctrl-y)
                             && c != '\u{1a}'   // "substitute" (sent with Ctrl-z)
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
                            ui.mouse_pos = ScreenSpaceXY { x: x as f32, y: y as f32};

                            match modifiers {
                                ModifiersState {
                                    ctrl: false, shift: false, ..
                                } => {
                                    let mut cursor_icon = glutin::window::CursorIcon::Text;
                                    if ui.mouse_pos.y >= status_line_y(font_info.status_char_dim, dimensions.height as _) {
                                        cursor_icon = d!();
                                    }

                                    glutin_context.window().set_cursor_icon(cursor_icon);

                                    if ui.left_mouse_state.is_pressed() && !mouse_within_radius!() {
                                        call_u_and_r!(Input::DragCursors(ui.mouse_pos));
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
                            ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;

                            let replace_or_add = if ctrl {
                                ReplaceOrAdd::Add
                            } else {
                                ReplaceOrAdd::Replace
                            };

                            let input = if mouse_within_radius!() {
                                Input::SelectCharTypeGrouping(
                                    ui.mouse_pos,
                                    replace_or_add
                                )
                            } else {
                                Input::SetCursor(
                                    ui.mouse_pos,
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
                            ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;
                            last_click_x = ui.mouse_pos.x;
                            last_click_y = ui.mouse_pos.y;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }
}

// This is probably excessive. We can make this smaller if there is a measuarable perf impact
// but given this goes on the stack, that seems unlikely?
type UIId = [u64; 32];

macro_rules! id {
    ($thing: expr) => {{
        let mut id: UIId = [0; 32];
        // TODO is the compilier smart enough to avoid the allocation here?
        let s = format!(
            "{:p}{}",
            // Take advantage of the auto-deref behaviour to get the furthest down reference,
            // which is the one that is most likely to be different.
            &*$thing,
            concat!("?", column!(), "?", line!(), "?", file!(), "?padding")
        );
        let slice = s.as_bytes();

        let mut i = 0;
        for chunk in slice.chunks_exact(8) {
            let mut value = 0u64;
            value |= (chunk[0] as u64) << 56;
            value |= (chunk[1] as u64) << 48;
            value |= (chunk[2] as u64) << 40;
            value |= (chunk[3] as u64) << 32;
            value |= (chunk[4] as u64) << 24;
            value |= (chunk[5] as u64) << 16;
            value |= (chunk[6] as u64) << 8;
            value |= (chunk[7] as u64) << 0;
            id[i] = value;
            i += 1;
            if i >= id.len() {
                break;
            }
        }

        id
    }};
}

macro_rules! dbg_id {
    ($id: expr) => {{
        // let mut s = String::with_capacity(32 * 4);
        // 'outer: for n in $id.iter() {
        //     let bytes = n.to_be_bytes();
        //     for &byte in bytes.iter() {
        //         if byte == 0 {
        //             break 'outer;
        //         }
        //         s.push(byte as char);
        //     }
        // }
        // dbg!(s);
        // $id
    }};
}

#[derive(Clone, Copy, Debug)]
enum PhysicalButtonState {
    Released,
    Pressed,
    ReleasedThisFrame,
    PressedThisFrame,
}
ord!(and friends for PhysicalButtonState: s, other in {
    use PhysicalButtonState::*;
    let s = match s {
        Released => 0,
        Pressed => 1,
        ReleasedThisFrame => 2,
        PressedThisFrame => 3,
    };

    let other = match other {
        Released => 0,
        Pressed => 1,
        ReleasedThisFrame => 2,
        PressedThisFrame => 3,
    };

    s.cmp(&other)
});

d!(for PhysicalButtonState: PhysicalButtonState::Released);

impl PhysicalButtonState {
    fn decay(&mut self) {
        *self = match *self {
            Self::ReleasedThisFrame => Self::Released,
            Self::PressedThisFrame => Self::Pressed,
            other => other,
        }
    }

    fn is_pressed(&self) -> bool {
        match *self {
            Self::ReleasedThisFrame | Self::Released => false,
            Self::PressedThisFrame | Self::Pressed => true,
        }
    }
}

struct UIState {
    mouse_pos: ScreenSpaceXY,
    left_mouse_state: PhysicalButtonState,
    active: UIId,
    hot: UIId,
    next_hot: UIId,
}

const UI_ID_ZERO: UIId = [0; 32];

d!(for UIState: UIState {
    mouse_pos: d!(),
    left_mouse_state: d!(),
    hot: UI_ID_ZERO,
    active: UI_ID_ZERO,
    next_hot: UI_ID_ZERO,
});

impl UIState {
    pub fn set_not_active(&mut self) {
        self.active = UI_ID_ZERO;
    }
    pub fn set_active(&mut self, id: UIId) {
        self.active = id;
    }
    pub fn set_next_hot(&mut self, id: UIId) {
        self.next_hot = id;
    }
    #[allow(dead_code)]
    pub fn set_not_hot(&mut self) {
        self.hot = UI_ID_ZERO;
    }
    pub fn frame_init(&mut self) {
        if self.active == UI_ID_ZERO {
            self.hot = self.next_hot;
        }
        self.next_hot = UI_ID_ZERO;
        // dbg!("frame_init");
        // dbg!(self.active != UI_ID_ZERO);
        // dbg!(self.hot != UI_ID_ZERO);
        // dbg!(self.next_hot != UI_ID_ZERO);
    }
    pub fn frame_end(&mut self) {
        // This needs to go here instead of in int, so that we actually see the undecayed state
        // for the first frame after the input event.
        self.left_mouse_state.decay();
    }
}

fn inside_rect(
    ScreenSpaceXY { x, y }: ScreenSpaceXY,
    ScreenSpaceRect { min, max }: ScreenSpaceRect,
) -> bool {
    x > min.0 && x <= max.0 && y > min.1 && y <= max.1
}

#[derive(Clone, Copy, Debug)]
enum ButtonState {
    Usual,
    Hover,
    Pressed,
}
ord!(and friends for ButtonState: s, other in {
    use ButtonState::*;
    let s = match s {
        Usual => 0,
        Hover => 1,
        Pressed => 2,
    };

    let other = match other {
        Usual => 0,
        Hover => 1,
        Pressed => 2,
    };

    s.cmp(&other)
});

type DoButtonResult = (bool, ButtonState);

/// calling this once will swallow multiple clicks on the button. We could either
/// pass in and return the number of clicks to fix that, or this could simply be
/// called multiple times per frame (once for each click).
fn do_button_logic(ui: &mut UIState, id: UIId, rect: ScreenSpaceRect) -> DoButtonResult {
    use ButtonState::*;
    let mut clicked = false;

    let mouse_pos = ui.mouse_pos;
    let mouse_state = ui.left_mouse_state;

    let inside = inside_rect(mouse_pos, rect);

    if ui.active == id {
        if mouse_state == PhysicalButtonState::ReleasedThisFrame {
            clicked = ui.hot == id && inside;
            dbg!("ui.set_not_active();", clicked);
            dbg_id!(id);
            ui.set_not_active();
        }
    } else if ui.hot == id {
        if mouse_state == PhysicalButtonState::PressedThisFrame {
            dbg!("ui.set_active(id);");
            dbg_id!(id);
            ui.set_active(id);
        }
    }

    if inside {
        dbg!("ui.set_next_hot(id);");
        dbg_id!(id);
        ui.set_next_hot(id);
    }

    let state = if ui.active == id && mouse_state.is_pressed() {
        Pressed
    } else if ui.hot == id {
        Hover
    } else {
        Usual
    };

    (clicked, state)
}

#[test]
fn do_button_logic_does_not_flash_like_it_used_to() {
    use std::f32::INFINITY;
    use ButtonState::*;

    let mut ui: UIState = d!();
    let id = id!(&0);
    let rect = ScreenSpaceRect {
        min: (-INFINITY, -INFINITY),
        max: (INFINITY, INFINITY),
    };

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Hover));
    ui.frame_end();

    ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Pressed));
    ui.frame_end();

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Pressed));
    ui.frame_end();

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Pressed));
    ui.frame_end();

    ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (true, Hover));
    ui.frame_end();

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Hover));
    ui.frame_end();

    ui.frame_init();
    assert_eq!(do_button_logic(&mut ui, id, rect), (false, Hover));
    ui.frame_end();
}
struct OutlineButtonSpec<'text> {
    text: &'text str,
    size: f32,
    layout: TextLayout,
    padding: f32,
    margin: f32,
    rect: ScreenSpaceRect,
    background_colour: Colour,
    text_colour: Colour,
    highlight_colour: Colour,
    z: u16,
}

fn enlarge_by(
    ScreenSpaceRect {
        min: (min_x, min_y),
        max: (max_x, max_y),
    }: ScreenSpaceRect,
    enlarge_amount: f32,
) -> ScreenSpaceRect {
    ScreenSpaceRect {
        min: (min_x - enlarge_amount, min_y - enlarge_amount),
        max: (max_x + enlarge_amount, max_y + enlarge_amount),
    }
}

fn shrink_by(
    ScreenSpaceRect {
        min: (min_x, min_y),
        max: (max_x, max_y),
    }: ScreenSpaceRect,
    shrink_amount: f32,
) -> ScreenSpaceRect {
    ScreenSpaceRect {
        min: (min_x + shrink_amount, min_y + shrink_amount),
        max: (max_x - shrink_amount, max_y - shrink_amount),
    }
}

fn do_outline_button<'view>(
    ui: &mut UIState,
    id: UIId,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    OutlineButtonSpec {
        text,
        size,
        layout,
        padding,
        margin,
        rect,
        background_colour,
        text_colour,
        highlight_colour,
        z,
    }: OutlineButtonSpec<'view>,
) -> bool {
    use ButtonState::*;
    let (clicked, state) = do_button_logic(ui, id, rect);

    macro_rules! push_text {
        () => {
            text_or_rects.push(TextOrRect::Text(TextSpec {
                text,
                size,
                layout,
                spec: VisualSpec {
                    rect: shrink_by(rect, padding),
                    color: text_colour,
                    z: z.saturating_sub(1),
                },
            }));
        };
    }

    match state {
        Pressed => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                color: highlight_colour,
                z,
            }));
            push_text!();
        }
        Hover => {
            // outline
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect: enlarge_by(rect, margin),
                color: highlight_colour,
                z: z.saturating_add(1),
            }));

            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                color: background_colour,
                z,
            }));
            push_text!();
        }
        Usual => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                color: background_colour,
                z,
            }));
            push_text!();
        }
    }

    clicked
}

const TAB_MIN_W: f32 = 128.0;
const TAB_MARGIN_RATIO: f32 = 1.0 / 128.0;
const TAB_PADDING_RATIO: f32 = 1.0 / 64.0;

fn usize_to_f32_or_65536(n: usize) -> f32 {
    use std::convert::TryFrom;
    u16::try_from(n).unwrap_or(u16::max_value()).into()
}

fn render_buffer_view<'view>(
    ui: &mut UIState,
    view: &'view View,
    FontInfo {
        text_char_dim,
        status_char_dim,
        tab_char_dim,
    }: &FontInfo,
    (width, height): (f32, f32),
) -> (Vec<TextOrRect<'view>>, Option<Input>) {
    let tab_y = 0.0;
    let edit_y = tab_y + tab_char_dim.h;

    let tab_count: f32 = usize_to_f32_or_65536(max(view.buffers.len(), 1));
    let tab_w = width / tab_count;
    let tab_w = if tab_w < TAB_MIN_W {
        tab_w
    } else {
        // NaN ends up here
        TAB_MIN_W
    };
    let tab_padding = tab_w * TAB_PADDING_RATIO;
    let tab_margin = tab_w * TAB_MARGIN_RATIO;

    let mut text_or_rects = Vec::with_capacity(view.buffers.len());
    let mut input = None;
    perf_viz::start_record!("for &BufferView");
    for (i, BufferView { name, .. }) in view.buffers.iter().enumerate() {
        let mut screen_position = text_to_screen(TextSpaceXY::default(), view.scroll);
        screen_position.y -= edit_y;
        let min_x = usize_to_f32_or_65536(i) * tab_w + tab_padding + view.tab_scroll;
        let max_x = usize_to_f32_or_65536(i + 1) * tab_w - tab_padding + view.tab_scroll;

        if do_outline_button(
            ui,
            id!(name),
            &mut text_or_rects,
            OutlineButtonSpec {
                text: &name,
                size: TAB_SIZE,
                layout: TextLayout::SingleLine,
                padding: tab_padding,
                margin: tab_margin,
                rect: ScreenSpaceRect {
                    min: (min_x, tab_y),
                    max: (max_x, tab_char_dim.h),
                },
                background_colour: CHROME_BACKGROUND_COLOUR,
                text_colour: c![0.6, 0.6, 0.6],
                highlight_colour: c![0.6, 0.6, 0.0],
                z: TAB_Z,
            },
        ) {
            input = Some(Input::SelectBuffer(i))
        }
    }

    if let Some(BufferView {
        chars,
        highlights,
        cursors,
        ..
    }) = view.visible_buffers[0].and_then(|i| view.buffers.get(i))
    {
        let text = {
            chars
            // perf_viz::record_guard!("map unprinatbles to symbols for themselves");
            // let s = chars
            //     .chars()
            //     .map(|c| {
            //         // map unprinatbles to symbols for themselves
            //         if c < 0x20 as char {
            //             std::char::from_u32(c as u32 | 0x2400u32).unwrap_or(c)
            //         } else {
            //             c
            //         }
            //     })
            //     .collect::<String>();
            // s
        };

        let ScreenSpaceXY { x, mut y } = text_to_screen(TextSpaceXY::default(), view.scroll);
        y += edit_y;
        text_or_rects.push(TextOrRect::Text(TextSpec {
            text: &text,
            size: TEXT_SIZE,
            layout: TextLayout::Wrap,
            spec: VisualSpec {
                rect: ScreenSpaceRect {
                    min: (x, y),
                    ..d!()
                },
                color: c![0.3, 0.3, 0.9],
                z: EDIT_Z,
            },
        }));

        perf_viz::start_record!("text_or_rects.extend");
        let CharDim { w, h }: CharDim = *text_char_dim;
        text_or_rects.extend(highlights.iter().map(
            |Highlight {
                 min, max, color, ..
             }| {
                TextOrRect::Rect(VisualSpec {
                    rect: ScreenSpaceRect {
                        min: (min.offset.0 as f32 * w + x, min.line as f32 * h + y),
                        max: (max.offset.0 as f32 * w + x, (max.line + 1) as f32 * h + y),
                    },
                    color: *color,
                    z: HIGHLIGHT_Z,
                })
            },
        ));
        perf_viz::end_record!("text_or_rects.extend");

        for c in cursors.iter() {
            let mut screen_xy = position_to_screen_space(c.position, *text_char_dim, view.scroll);
            screen_xy.y += edit_y;
            text_or_rects.push(TextOrRect::Text(TextSpec {
                text: "▏",
                size: TEXT_SIZE,
                layout: TextLayout::SingleLine,
                spec: VisualSpec {
                    rect: ScreenSpaceRect {
                        min: screen_xy.into(),
                        max: (width, text_char_dim.h),
                    },
                    color: match c.state {
                        CursorState::None => c![0.9, 0.3, 0.3],
                        CursorState::PressedAgainstWall => c![0.9, 0.9, 0.3],
                    },
                    z: CURSOR_Z,
                },
            }));
        }
    }
    perf_viz::end_record!("for &BufferView");

    //
    //   Status line
    //
    let rect = ScreenSpaceRect {
        min: (0.0, status_line_y(*status_char_dim, height)),
        max: (width, height),
    };

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect,
        color: CHROME_BACKGROUND_COLOUR,
        z: STATUS_BACKGROUND_Z,
    }));

    text_or_rects.push(TextOrRect::Text(TextSpec {
        text: &view.status_line.chars,
        size: STATUS_SIZE,
        layout: TextLayout::SingleLine,
        spec: VisualSpec {
            rect,
            color: c![0.3, 0.9, 0.3],
            z: STATUS_Z,
        },
    }));

    (text_or_rects, input)
}

fn status_line_y(status_char_dim: CharDim, height: f32) -> f32 {
    height - status_char_dim.h
}
