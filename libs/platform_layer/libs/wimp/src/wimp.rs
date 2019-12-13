// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin::{dpi::LogicalPosition, Api, GlProfile, GlRequest};
use std::collections::VecDeque;
use std::path::PathBuf;
use std::time::Duration;
use wimp_render::{get_find_replace_info, FindReplaceInfo};

use file_chooser;
use macros::d;
use platform_types::{screen_positioning::screen_to_text_box, *};
use shared::{BufferStatus, BufferStatuses, Res};
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
    run_inner(update_and_render)
}

// This extra fn is a workaround for the record attribute causing a "procedural macros cannot
// expand to macro definitions" error otherwise.According to issue #54727, this is because there
// is some worry that all the macro hygiene edge cases may not be handled.
fn run_inner(update_and_render: UpdateAndRender) -> Res<()> {
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

    let title = "rote";

    let mut args = std::env::args();
    //exe name
    args.next();

    let mut data_dir = None;

    const VERSION: &'static str = "--version";
    const HELP: &'static str = "--help";
    const DATA_DIR_OVERRIDE: &'static str = "--data-dir-override";

    while let Some(s) = args.next() {
        let s: &str = &s;
        match s {
            HELP => {
                let accepted_args = [VERSION, HELP, DATA_DIR_OVERRIDE];
                println!("accepted args: ");
                for arg in accepted_args.iter() {
                    print!("    {}", arg);
                    if *arg == DATA_DIR_OVERRIDE {
                        print!(" <data directory path>");
                    }
                    println!()
                }
                std::process::exit(0)
            }
            VERSION => {
                println!("{} version {}", title, env!("CARGO_PKG_VERSION"));
                std::process::exit(0)
            }
            DATA_DIR_OVERRIDE => {
                data_dir = Some(args.next().ok_or_else(|| {
                    format!(
                        "{0} needs an argument. For example: {0} ./data",
                        DATA_DIR_OVERRIDE
                    )
                })?)
                .map(PathBuf::from);
            }
            _ => {
                eprintln!("unknown arg {:?}", s);
                std::process::exit(1)
            }
        }
    }

    let data_dir = data_dir
        .or_else(|| {
            directories::ProjectDirs::from("com", "ryanwiedemann", title)
                .map(|proj_dirs| proj_dirs.data_dir().to_owned())
        })
        .ok_or("Could not find app data dir")?;

    match std::fs::metadata(&data_dir) {
        Ok(meta) => {
            if meta.is_dir() {
                Ok(())
            } else {
                Err("data_dir existed but was not a directory!".to_owned())
            }
        }
        Err(err) => {
            if err.kind() == std::io::ErrorKind::NotFound {
                std::fs::create_dir_all(&data_dir)
                    .map_err(|e| e.to_string())
                    .and_then(|_| {
                        std::fs::metadata(&data_dir)
                            .map_err(|e| e.to_string())
                            .and_then(|meta| {
                                if meta.is_dir() {
                                    Ok(())
                                } else {
                                    Err("data_dir was created but was not a directory!".to_owned())
                                }
                            })
                    })
            } else {
                Err(err.to_string())
            }
        }
    }?;

    let edited_files_dir = data_dir.join("edited_files_v1/");
    let edited_files_index_path = data_dir.join("edited_files_v1_index.txt");

    let mut clipboard: Clipboard = get_clipboard();

    #[derive(Clone, Debug)]
    enum CustomEvent {
        OpenFile(PathBuf),
        SaveNewFile(PathBuf, g_i::Index),
        SendBuffersToBeSaved,
        EditedBufferError(String),
    }
    unsafe impl Send for CustomEvent {}
    unsafe impl Sync for CustomEvent {}

    use glutin::event_loop::EventLoop;
    let events: EventLoop<CustomEvent> = glutin::event_loop::EventLoop::with_user_event();
    let event_proxy = events.create_proxy();

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
    dbg!(glutin_context.get_pixel_format());

    let scroll_multiplier: f32 = 16.0;

    let (mut gl_state, char_dims) = gl_layer::init(
        glutin_context.window().hidpi_factor() as f32,
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
        .inner_size()
        .to_physical(glutin_context.window().hidpi_factor());

    macro_rules! screen_wh {
        () => {
            ScreenSpaceWH {
                w: dimensions.width as f32,
                h: dimensions.height as f32,
            }
        };
    }

    let (mut view, mut cmds) = {
        let FindReplaceInfo {
            find_text_xywh,
            replace_text_xywh,
            ..
        } = get_find_replace_info(font_info, screen_wh!());
        let (v, c) = update_and_render(Input::SetSizeDependents(SizeDependents {
            buffer_xywh: wimp_render::get_edit_buffer_xywh(d!(), font_info, screen_wh!()).into(),
            find_xywh: find_text_xywh.into(),
            replace_xywh: replace_text_xywh.into(),
            font_info: font_info.into(),
        }));

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
            (last_click_x - ui.mouse_pos.x).abs() <= mouse_epsilon_radius
                && (last_click_y - ui.mouse_pos.y).abs() <= mouse_epsilon_radius
        };
    }

    let buffer_statuses = std::sync::Arc::new(BufferStatuses::new(
        16, /* TODO use initial buffer count */
    ));
    use std::sync::mpsc::channel;

    // TODO set up edited files thread
    // * simple demo printing
    // * setup communication and printout transfeerred data
    // * actually write stuff to disk

    // into the edited files thread
    let (edited_files_in_sink, edited_files_in_source) = channel();

    enum EditedFilesThread {
        Quit,
        Buffers(g_i::State, Vec<BufferView>),
    }

    let mut edited_files_join_handle = Some({
        let proxy = event_proxy.clone();
        let buffer_statuses_ref = buffer_statuses.clone();

        std::thread::Builder::new()
            .name("edited_files".to_string())
            .spawn(move || {
                // TODO read channel from ui thread periodically to update buffer_statuses_ref
                // and check if it is time to write to disk.
                loop {
                    macro_rules! handle_message {
                        ($message: expr) => {{
                            use EditedFilesThread::*;
                            match $message {
                                Quit => return,
                                Buffers(index_state, buffers) => {
                                    match edited_storage::store_buffers(
                                        &edited_files_dir,
                                        &edited_files_index_path,
                                        buffers,
                                        index_state,
                                        buffer_statuses_ref.get_mut_possibly_blocking(),
                                    ) {
                                        Ok(_) => {}
                                        Err(e) => {
                                            use std::error::Error;
                                            let _hope_it_gets_there =
                                                proxy.send_event(CustomEvent::EditedBufferError(
                                                    e.description().to_owned(),
                                                ));
                                        }
                                    }
                                }
                            }
                        }};
                    }

                    // 20 * 50 = 1_000, 60_000 ms = 1 minute
                    // so this waits roughly a minute plus waiting time for messages
                    const QUIT_CHECK_COUNT: u32 = 20; // * 60;
                    for _ in 0..QUIT_CHECK_COUNT {
                        std::thread::sleep(Duration::from_millis(50));
                        if let Ok(message) = edited_files_in_source.try_recv() {
                            handle_message!(message);
                        }
                    }

                    let _hope_it_gets_there = proxy.send_event(CustomEvent::SendBuffersToBeSaved);

                    if let Ok(message) = edited_files_in_source.recv() {
                        handle_message!(message);
                    }
                }
            })
            .expect("Could not start edited_files thread!")
    });

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
        let buffer_statuses_ref = buffer_statuses.clone();
        events.run(move |event, _, control_flow| {
            use glutin::event::*;

            macro_rules! call_u_and_r {
                ($input:expr) => {
                    ui.note_interaction();
                    let _hope_it_gets_there = editor_in_sink.send($input);
                };
            }

            // eventually we'll likely want to tell  the editor, and have it decide whether/how
            // to display it to the user.
            macro_rules! handle_platform_error {
                ($err: expr) => {
                    eprintln!("{}", $err);
                };
            }

            macro_rules! save_to_disk {
                ($path: expr, $str: expr, $buffer_index: expr) => {
                    match std::fs::write($path, $str) {
                        Ok(_) => {
                            call_u_and_r!(Input::SetBufferPath($buffer_index, $path.to_path_buf()));
                        }
                        Err(err) => {
                            handle_platform_error!(err);
                        }
                    }
                };
            }

            macro_rules! load_file {
                ($path: expr) => {{
                    let p = $path;
                    match std::fs::read_to_string(&p) {
                        Ok(s) => {
                            call_u_and_r!(Input::LoadedFile(p, s));
                        }
                        Err(err) => {
                            handle_platform_error!(err);
                        }
                    }
                }};
            }

            match event {
                Event::EventsCleared if running => {
                    let buffer_statuses = buffer_statuses_ref.get_mut_without_blocking_for_one_consumer();
                    for _ in 0..EVENTS_PER_FRAME {
                        match editor_out_source.try_recv() {
                            Ok((v, c)) => {
                                view = v;
                                if let Some(index) = view.edited_buffer_index {
                                    buffer_statuses.insert(view.index_state, index, BufferStatus::EditedAndUnSaved);
                                }

                                cmds.push_back(c);
                            }
                            _ => break,
                        };
                    }

                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                }
                Event::WindowEvent {
                    event: WindowEvent::RedrawRequested,
                    ..
                } => {
                    ui.frame_init();
                    if_changed::dbg!(&ui.keyboard);

                    let (text_and_rects, input) =
                        wimp_render::view(
                            &mut ui,
                            &view,
                            &font_info,
                            screen_wh!(),
                            dt,
                            buffer_statuses_ref.get_non_blocking()
                        );
                    let width = dimensions.width;
                    let height = dimensions.height;

                    gl_layer::render(&mut gl_state, text_and_rects, width as _, height as _)
                        .expect("gl_layer::render didn't work");

                    glutin_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");

                    for _ in 0..EVENTS_PER_FRAME {
                        if let Some(cmd) = cmds.pop_front() {
                            match cmd {
                                Cmd::SetClipboard(s) => {
                                    if let Err(err) = clipboard.set_contents(s) {
                                        handle_platform_error!(err);
                                    }
                                }
                                Cmd::LoadFile(path) => load_file!(path),
                                Cmd::NoCmd => {}
                            }
                        } else {
                            break;
                        }
                    }

                    if let Some(input) = input {
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
                    dt = loop_helper.loop_start();
                }
                Event::UserEvent(e) => match e {
                    CustomEvent::OpenFile(p) => load_file!(p),
                    CustomEvent::SaveNewFile(ref p, index) => {
                        // The fact we need to store the index and retreive it later, potentially
                        // across multiple updates, is why this thread needs to know about the
                        // generational indices.
                        if let Some(b) = index
                            .get(view.index_state)
                            .and_then(|i| view.buffers.get(i))
                        {
                            save_to_disk!(p, &b.data.chars, index);
                        }
                    }
                    CustomEvent::SendBuffersToBeSaved => {
                        let _hope_it_gets_there = edited_files_in_sink.send(
                            EditedFilesThread::Buffers(view.index_state, view.buffers.clone())
                        );
                    }
                    CustomEvent::EditedBufferError(e) => {
                        // TODO show warning dialog to user and ask if they want to continue
                        // without edited file saving or save and restart. If they do, then it
                        // should be made obvious visually that the feature is not working right
                        // now.
                        handle_platform_error!(e);
                    }
                },
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
                            let _hope_it_gets_there = edited_files_in_sink.send(EditedFilesThread::Quit);
                            running = false;

                            // If we got here, we assume that we've sent a Quit input to the editor thread so it will stop.
                            match editor_join_handle.take() {
                                Some(j_h) => j_h.join().expect("Could not join editor thread!"),
                                None => {}
                            };

                            match edited_files_join_handle.take() {
                                Some(j_h) => j_h.join().expect("Could not join edited_files thread!"),
                                None => {}
                            };

                            perf_viz::output!();

                            let _ = gl_layer::cleanup(&gl_state);

                            *control_flow = glutin::event_loop::ControlFlow::Exit;
                        }};
                    }

                    macro_rules! file_chooser_call {
                        ($func: ident, $path: ident in $event: expr) => {
                            let proxy =
                                std::sync::Arc::new(std::sync::Mutex::new(event_proxy.clone()));
                            let proxy = proxy.clone();
                            file_chooser::$func(move |$path: PathBuf| {
                                let _bye = proxy
                                    .lock()
                                    .expect("file_chooser thread private mutex locked!?")
                                    .send_event($event);
                            })
                        };
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

                            screen_to_text_box(ui.mouse_pos, xy)
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
                            let FindReplaceInfo {
                                find_text_xywh,
                                replace_text_xywh,
                                ..
                            } = get_find_replace_info(font_info, screen_wh!());
                            call_u_and_r!(Input::SetSizeDependents(SizeDependents {
                                buffer_xywh: wimp_render::get_edit_buffer_xywh(
                                    view.menu.get_mode(),
                                    font_info,
                                    screen_wh!()
                                )
                                .into(),
                                find_xywh: find_text_xywh.into(),
                                replace_xywh: replace_text_xywh.into(),
                                font_info: None,
                            }));
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
                                if wimp_render::inside_tab_area(ui.mouse_pos, font_info) {
                                    let width = dimensions.width;
                                    let height = dimensions.height;

                                    wimp_render::make_active_tab_visible(
                                        &mut ui,
                                        &view,
                                        &font_info,
                                        (width as _, height as _),
                                    );
                                } else {
                                    call_u_and_r!(Input::ResetScroll);
                                }
                            }
                            VirtualKeyCode::Home => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToBufferStart));
                            }
                            VirtualKeyCode::End => {
                                call_u_and_r!(Input::MoveAllCursors(Move::ToBufferEnd));
                            }
                            VirtualKeyCode::Left => {
                                call_u_and_r!(Input::MoveAllCursors(
                                    Move::ToPreviousLikelyEditLocation
                                ));
                            }
                            VirtualKeyCode::Right => {
                                call_u_and_r!(Input::MoveAllCursors(
                                    Move::ToNextLikelyEditLocation
                                ));
                            }
                            VirtualKeyCode::A => {
                                call_u_and_r!(Input::SelectAll);
                            }
                            VirtualKeyCode::C => {
                                call_u_and_r!(Input::Copy);
                            }
                            VirtualKeyCode::D => {
                                call_u_and_r!(Input::ExtendSelectionWithSearch);
                            }
                            VirtualKeyCode::F => {
                                call_u_and_r!(Input::SetMenuMode(MenuMode::FindReplace));
                                call_u_and_r!(Input::SetSizeDependents(SizeDependents {
                                    buffer_xywh: wimp_render::get_edit_buffer_xywh(
                                        MenuMode::FindReplace,
                                        font_info,
                                        screen_wh!()
                                    )
                                    .into(),
                                    find_xywh: None,
                                    replace_xywh: None,
                                    font_info: None,
                                }));
                            }
                            VirtualKeyCode::O => {
                                file_chooser_call!(single, p in CustomEvent::OpenFile(p));
                            }
                            VirtualKeyCode::P => {
                                call_u_and_r!(Input::SetMenuMode(MenuMode::FileSwitcher));
                                call_u_and_r!(Input::SetSizeDependents(SizeDependents {
                                    buffer_xywh: wimp_render::get_edit_buffer_xywh(
                                        MenuMode::FileSwitcher,
                                        font_info,
                                        screen_wh!()
                                    )
                                    .into(),
                                    find_xywh: None,
                                    replace_xywh: None,
                                    font_info: None,
                                }));
                            }
                            VirtualKeyCode::S => {
                                if let Some((i, buffer)) = view.get_visible_index_and_buffer() {
                                    match buffer.name {
                                        BufferName::Scratch(_) => {
                                            file_chooser_call!(
                                                save,
                                                p in CustomEvent::SaveNewFile(p, i)
                                            );
                                        }
                                        BufferName::Path(ref p) => {
                                            save_to_disk!(p, &buffer.data.chars, i);
                                        }
                                    }
                                }
                            }
                            VirtualKeyCode::T => {
                                call_u_and_r!(Input::NewScratchBuffer);
                            }
                            VirtualKeyCode::V => {
                                call_u_and_r!(Input::Paste(clipboard.get_contents().ok()));
                            }
                            VirtualKeyCode::W => match view.current_buffer_id {
                                BufferId {
                                    kind: BufferIdKind::Text,
                                    index,
                                    ..
                                } => {
                                    call_u_and_r!(Input::CloseBuffer(index));
                                }
                                _ => {
                                    call_u_and_r!(Input::CloseMenuIfAny);
                                }
                            },
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
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(
                                    Move::ToPreviousLikelyEditLocation
                                ));
                            }
                            VirtualKeyCode::Right => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(
                                    Move::ToNextLikelyEditLocation
                                ));
                            }
                            VirtualKeyCode::S => {
                                if let Some(i) = view.visible_buffer {
                                    file_chooser_call!(
                                        save,
                                        p in CustomEvent::SaveNewFile(p, i)
                                    );
                                }
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
                        } => match dbg!(keypress) {
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
                                    font_info: None,
                                }));
                                call_u_and_r!(Input::CloseMenuIfAny);
                            }
                            VirtualKeyCode::Back => {
                                call_u_and_r!(Input::Delete);
                            }
                            VirtualKeyCode::Up => {
                                call_u_and_r!(Input::MoveAllCursors(Move::Up));
                                ui.navigation = Navigation::Up;
                            }
                            VirtualKeyCode::Down => {
                                call_u_and_r!(Input::MoveAllCursors(Move::Down));
                                ui.navigation = Navigation::Down;
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
                                ui.navigation = Navigation::Up;
                            }
                            VirtualKeyCode::Down => {
                                call_u_and_r!(Input::ExtendSelectionForAllCursors(Move::Down));
                                ui.navigation = Navigation::Down;
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
                            VirtualKeyCode::Return => {
                                call_u_and_r!(Input::Insert('\n'));
                            }
                            _ => (),
                        },
                        WindowEvent::ReceivedCharacter(mut c) => {
                            if c != '\u{1}'       // "start of heading" (sent with Ctrl-a)
                             && c != '\u{3}'    // "end of text" (sent with Ctrl-c)
                             && c != '\u{4}'    // "end of transmission" (sent with Ctrl-d)
                             && c != '\u{6}'    // "acknowledge" (sent with Ctrl-f)
                             && c != '\u{8}'    // backspace (sent with Ctrl-h)
                             && c != '\u{9}'    // horizontal tab (sent with Ctrl-i)
                             && c != '\u{f}'    // "shift in" AKA use black ink apparently, (sent with Ctrl-o)
                             && c != '\u{10}'   // "data link escape" AKA interprt the following as raw data, (sent with Ctrl-p)
                             && c != '\u{13}'   // "device control 3" (sent with Ctrl-s)
                             && c != '\u{14}'   // "device control 4" (sent with Ctrl-t)
                             && c != '\u{16}'   // "synchronous idle" (sent with Ctrl-v)
                             && c != '\u{17}'   // "end of transmission block" (sent with Ctrl-w)
                             && c != '\u{18}'   // "cancel" (sent with Ctrl-x)
                             && c != '\u{19}'   // "end of medium" (sent with Ctrl-y)
                             && c != '\u{1a}'   // "substitute" (sent with Ctrl-z)
                             && c != '\u{1b}'   // escape
                             && c != '\u{7f}'
                            // delete
                            {
                                if c == '\r' {
                                    c = '\n';
                                }

                                if c == '\n' {
                                    use BufferIdKind::*;
                                    match view.current_buffer_id.kind {
                                        None => {
                                            ui.navigation = Navigation::Interact;
                                        }
                                        Text => {
                                            call_u_and_r!(Input::Insert(c));
                                        }
                                        Find | Replace | FileSwitcher => {
                                            call_u_and_r!(Input::SubmitForm);
                                        }
                                    }
                                } else {
                                    call_u_and_r!(Input::Insert(c));
                                }
                            }
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers: ModifiersState { shift: false, .. },
                            ..
                        } => {
                            let scroll_y = y * scroll_multiplier;
                            if wimp_render::inside_tab_area(ui.mouse_pos, font_info) {
                                ui.tab_scroll += scroll_y;
                            } else {
                                call_u_and_r!(Input::ScrollVertically(scroll_y));
                            }
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers: ModifiersState { shift: true, .. },
                            ..
                        } => {
                            let scroll_y = y * scroll_multiplier;
                            if wimp_render::inside_tab_area(ui.mouse_pos, font_info) {
                                ui.tab_scroll += scroll_y;
                            } else {
                                call_u_and_r!(Input::ScrollHorizontally(scroll_y));
                            }
                        }
                        WindowEvent::CursorMoved {
                            position: LogicalPosition { x, y },
                            modifiers,
                            ..
                        } => {
                            ui.mouse_pos = ScreenSpaceXY {
                                x: x as f32,
                                y: y as f32,
                            };

                            match modifiers {
                                ModifiersState {
                                    ctrl: false,
                                    shift: false,
                                    ..
                                } => {
                                    let cursor_icon = if wimp_render::should_show_text_cursor(
                                        ui.mouse_pos,
                                        view.current_buffer_id,
                                        view.menu.get_mode(),
                                        font_info,
                                        screen_wh!(),
                                    ) {
                                        glutin::window::CursorIcon::Text
                                    } else {
                                        d!()
                                    };

                                    glutin_context.window().set_cursor_icon(cursor_icon);

                                    if ui.left_mouse_state.is_pressed() && !mouse_within_radius!() {
                                        call_u_and_r!(Input::DragCursors(text_box_xy!()));
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
                                    ctrl, shift: false, ..
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
                                Input::SelectCharTypeGrouping(text_box_xy!(), replace_or_add)
                            } else {
                                Input::SetCursor(text_box_xy!(), replace_or_add)
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
