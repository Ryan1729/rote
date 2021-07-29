// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

use glutin_wrapper::{dpi::LogicalPosition, Api, GlProfile, GlRequest};
use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    time::Duration,
};
use wimp_render::{get_find_replace_info, FindReplaceInfo, get_go_to_position_info, GoToPositionInfo, ViewOutput, ViewAction};
use wimp_types::{ui, ui::{PhysicalButtonState, Navigation}, transform_at, BufferStatus, BufferStatusTransition, CustomEvent, get_clipboard, ClipboardProvider, Dimensions, LabelledCommand, RunConsts, RunState, MenuMode, Pids, PidKind};
use macros::{d, dbg};
use platform_types::{screen_positioning::screen_to_text_box, *};
use shared::{Res};

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

    let title = "rote";

    let mut args = std::env::args();
    //exe name
    args.next();

    let mut data_dir = None;
    let mut hidpi_factor_override = None;
    // We expect the program to most often be opened with either 0 or 1 extra paths.
    let mut extra_paths = Vec::with_capacity(1);

    const VERSION: &str = "--version";
    const HELP: &str = "--help";
    const DATA_DIR_OVERRIDE: &str = "--data-dir-override";
    const HIDPI_OVERRIDE: &str = "--hidpi-override";
    const LICENSE: &str = "--license";
    const FILE: &str = "--file";

    while let Some(s) = args.next() {
        let s: &str = &s;
        match s {
            HELP => {
                let accepted_args = [VERSION, HELP, DATA_DIR_OVERRIDE, HIDPI_OVERRIDE, LICENSE, FILE];
                println!("accepted args: ");
                for arg in accepted_args.iter() {
                    print!("    {}", arg);
                    if *arg == DATA_DIR_OVERRIDE {
                        print!(" <data directory path>");
                    }
                    if *arg == HIDPI_OVERRIDE {
                        print!(" <hidpi factor (positive floating point number)>");
                    }
                    if *arg == FILE {
                        print!(" <path of file to open>");
                    }
                    println!()
                }
                std::process::exit(0)
            }
            VERSION => {
                // We expect the main crate to unconditionally print the version.
                // This is because the main crate has access to the correct
                // version value through the "CARGO_PKG_VERSION" env var.
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

                data_dir = data_dir.map(canonical_or_same);
            }
            HIDPI_OVERRIDE => {
                hidpi_factor_override = Some(args.next().ok_or_else(|| {
                    format!(
                        "{0} needs an argument. For example: {0} 1.5",
                        HIDPI_OVERRIDE
                    )
                })?)
                .and_then(|s| {
                    use std::str::FromStr;
                    f64::from_str(&s).ok()
                });
            }
            LICENSE => {
                println!("{} program by Ryan Wiedemann.", title);
                println!("Source and license available at:");
                println!("    https://github.com/Ryan1729/{}", title);
                println!();
                println!("License for the font:");
                println!("{}", gl_layer::FONT_LICENSE);
                std::process::exit(0)
            }
            FILE => {
                let path = args.next().ok_or_else(|| {
                    format!(
                        "{0} needs an argument. For example: {0} ./file.txt",
                        FILE
                    )
                })
                // Would it be better to open the window and display a path error
                // there instead of `?` here?
                // If someone specified a file, they probably want to open that 
                // particular file, or maybe they made a typo, either way, they 
                // probably would rather have the feedback in their terminal or
                // whatever, so they can correct the file name
                .map(PathBuf::from)?;

                extra_paths.push(canonical_or_same(path));
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

    let running_lock_path = data_dir.join("running.lock");
    use atomicwrites::{AtomicFile, OverwriteBehavior};
    let previous_instance_is_running = {
        let file = AtomicFile::new_with_tmpdir(
            &running_lock_path,
            OverwriteBehavior::DisallowOverwrite,
            &data_dir
        );

        let res = file.write(|f| {
            // In some sense it doesn't matter what we write here, but it seems like it
            // would be nice for it to be different each time it is written.
            use std::time::{SystemTime, UNIX_EPOCH};
            let thing_to_write = match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(duration) => format!("{}\n", duration.as_nanos()),
                Err(e) => format!("-{}\n", e.duration().as_nanos()),
            };

            use std::io::Write;
            f.write_all(thing_to_write.as_bytes())
        });

        match res {
            Ok(()) => false,
            Err(
                atomicwrites::Error::Internal(io_error)
            ) | Err(
                atomicwrites::Error::User(io_error)
            ) if io_error.kind() == std::io::ErrorKind::AlreadyExists =>  {
                true
            },
            Err(_) => { return res.map_err(From::from); }
        }
    };

    let path_mailbox_path = data_dir.join("path_mailbox_path.txt");

    if previous_instance_is_running {
        println!(
            "Previous instance of {} detected, or at least a file at:\n{}",
            title,
            running_lock_path.to_string_lossy()
        );

        let path_count = extra_paths.len();
        if path_count > 0 {
            println!(
                "Adding path{} to {} for the other instance to read.",
                if path_count == 1 { "" } else { "s" },
                path_mailbox_path.to_string_lossy(),
            );

            // This `inner` function lets us use `?` while also capturing the `Res`
            // afterward.
            let inner = |path_mailbox_path: PathBuf| -> Res<()> {
                let path_list = {
                    use std::fs::OpenOptions;

                    let mut path_mailbox = OpenOptions::new()
                        .read(true)
                        .write(true)
                        .create(true)
                        .open(&path_mailbox_path)?;

                    // This size calculation is copied from `initial_buffer_size`
                    // function in rust's std::fs module, including the following
                    // comment:
                    // Allocate one extra byte so the buffer doesn't need to grow before the
                    // final `read` call at the end of the file.  Don't worry about `usize`
                    // overflow because reading will fail regardless in that case.
                    let previous_size = path_mailbox.metadata()
                        .map(|m| m.len() as usize + 1)
                        .unwrap_or(0);
                    let additional_size_estimate = path_count * 256;

                    let mut path_list = String::with_capacity(
                        previous_size + additional_size_estimate
                    );

                    use std::io::Read;
                    path_mailbox.read_to_string(&mut path_list)?;

                    for p in extra_paths {
                        use std::fmt::Write;
                        // Regarding `to_string_lossy`, in other places, e.g.
                        // `edited_storage`, we already don't support non-Unicode
                        // paths.
                        // TODO: is there a reasonable way to avoid these extra
                        // allocations?
                        let _write_on_string_cannot_fail = 
                            write!(&mut path_list, "{}\n", p.to_string_lossy());
                    }

                    path_list
                };

                let file = AtomicFile::new_with_tmpdir(
                    &path_mailbox_path,
                    OverwriteBehavior::AllowOverwrite,
                    &data_dir
                );

                file.write(|f| {
                    use std::io::Write;
                    f.write_all(path_list.as_bytes())
                }).map_err(From::from)
            };

            let res = inner(path_mailbox_path);

            match res {
                Ok(()) => {
                    println!("Seems like we were able to write to the mailbox.");
                },
                Err(_) => {
                    println!("Mailbox write failed.");
                    return res;
                }
            }
        }

        std::process::exit(0)
    }

    let edited_files_dir_buf = data_dir.join("edited_files_v1/");
    let edited_files_index_path_buf = data_dir.join("edited_files_v1_index.txt");

    use glutin_wrapper::event_loop::EventLoop;
    let events: EventLoop<CustomEvent> = glutin_wrapper::event_loop::EventLoop::with_user_event();
    let event_proxy = events.create_proxy();

    let glutin_context = glutin_wrapper::ContextBuilder::new()
        .with_gl_profile(GlProfile::Core)
        //As of now we only need 3.3 for GL_TIME_ELAPSED. Otherwise we could use 3.2.
        .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
        .with_srgb(true)
        .with_depth_buffer(24)
        .build_windowed(
            glutin_wrapper::window::WindowBuilder::new()
                .with_inner_size(
                    glutin_wrapper::dpi::Size::Logical(glutin_wrapper::dpi::LogicalSize::new(1024.0, 576.0))
                 )
                .with_title(title),
            &events,
        )?;
    let glutin_context = unsafe { glutin_context.make_current().map_err(|(_, e)| e)? };

    let mut current_hidpi_factor = 1.0;
    macro_rules! get_hidpi_factor {
        () => {
            hidpi_factor_override.unwrap_or(current_hidpi_factor)
        }
    }

    let (mut gl_state, char_dims) = gl_layer::init(
        get_hidpi_factor!() as f32,
        &wimp_render::TEXT_SIZES,
        wimp_render::TEXT_BACKGROUND_COLOUR,
        |symbol| glutin_context.get_proc_address(symbol) as _,
    )?;

    const TARGET_RATE: f64 = 128.0; //250.0);

    let mut loop_helper = spin_sleep::LoopHelper::builder().build_with_target_rate(TARGET_RATE);

    let mut running = true;

    let startup_description = format!(
        "data_dir: \n{}", data_dir.to_string_lossy()
    );

    use std::sync::mpsc::{channel};

    let mut pids = Pids::default();
    pids.window = std::process::id();

    // into the path mailbox thread
    let (path_mailbox_in_sink, path_mailbox_in_source) = channel();

    #[derive(Debug)]
    enum PathMailboxThread {
        Quit,
    }
    
    let mut path_mailbox_join_handle = Some({
        let running_lock_path = running_lock_path.clone();
        let path_mailbox_path = path_mailbox_path.clone();
        let proxy = event_proxy.clone();

        std::thread::Builder::new()
            .name("path_mailbox".to_string())
            .spawn(move || {
                {
                    let _hope_it_gets_there = proxy.send_event(CustomEvent::Pid(
                        PidKind::PathMailbox,
                        std::process::id()
                    ));
                }
                // If we got an error we should still keep this thread going
                // in case the error is temporary.
                macro_rules! continue_if_err {
                    ($result: expr) => {
                        match $result {
                            Ok(x) => {x},
                            Err(_) => {
                                continue;
                            }
                        }
                    }
                }

                loop {
                    std::thread::sleep(Duration::from_millis(50));

                    if let Ok(message) = path_mailbox_in_source.try_recv() {
                        use PathMailboxThread::*;
                        match message {
                            Quit => {
                                let running_lock_path_string = 
                                    running_lock_path.to_string_lossy().to_string();
                                match std::fs::remove_file(
                                    running_lock_path
                                ) {
                                    Ok(()) => {
                                        println!(
                                            "Deleted {} successfully",
                                            running_lock_path_string
                                        );
                                    }
                                    Err(err) => {
                                        println!(
                                            "Could not delete {}. You may need to do so manually later.",
                                            running_lock_path_string
                                        );
                                        eprintln!("{}", err);
                                    }
                                };
                                return
                            },
                        }
                    }

                    let path_mailbox_string = continue_if_err!(
                        std::fs::read_to_string(
                            &path_mailbox_path
                        )
                    );

                    let file = AtomicFile::new_with_tmpdir(
                        &path_mailbox_path,
                        OverwriteBehavior::AllowOverwrite,
                        &data_dir
                    );

                    // We want to replace the file with a blank one.
                    // Not writing anything to the file achieves this.
                    continue_if_err!(file.write::<(), (), _>(|_| Ok(())));

                    for line in path_mailbox_string.lines() {
                        let _hope_it_gets_there =
                            proxy.send_event(CustomEvent::OpenFile(
                                // We don't know the CWD of the instance that put
                                // this in the mailbox, so it does not make sense
                                // to canonicalize this path ourselves. The code
                                // putting the line in here must do that.
                                std::path::PathBuf::from(line),
                            ));
                    }
                }
            })
            .expect("Could not start path_mailbox thread!")
    });

    // into the edited files thread
    let (edited_files_in_sink, edited_files_in_source) = channel();
    // out of the edited files thread
    let (edited_files_out_sink, edited_files_out_source) = channel();

    #[derive(Debug)]
    enum EditedFilesThread {
        Quit,
        Buffers(g_i::State, Vec<edited_storage::BufferInfo>),
    }

    let mut edited_files_join_handle = Some({
        let edited_files_dir = edited_files_dir_buf.clone();
        let edited_files_index_path = edited_files_index_path_buf.clone();
        let proxy = event_proxy.clone();

        std::thread::Builder::new()
            .name("edited_files".to_string())
            .spawn(move || {
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
                                    ) {
                                        Ok(transitions) => {
                                            for transition in transitions {
                                                let _hope_it_gets_there =
                                                    edited_files_out_sink.send(transition);
                                            }
                                        }
                                        Err(e) => {
                                            let _hope_it_gets_there =
                                                proxy.send_event(CustomEvent::EditedBufferError(
                                                    e.to_string(),
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

    enum EditorThreadOutput {
        Rendered(UpdateAndRenderOutput),
        Pid(u32)
    }

    let mut editor_join_handle = Some(
        std::thread::Builder::new()
            .name("editor".to_string())
            .spawn(move || {
                {
                    let _hope_it_gets_there = editor_out_sink.send(
                        EditorThreadOutput::Pid(std::process::id())
                    );
                }

                while let Ok(input) = editor_in_source.recv() {
                    let was_quit = Input::Quit == input;
                    let rendered = update_and_render(input);
                    let _hope_it_gets_there = editor_out_sink.send(
                        EditorThreadOutput::Rendered(rendered)
                    );
                    if was_quit {
                        return;
                    }
                }
            })
            .expect("Could not start editor thread!"),
    );

    use edited_storage::{load_tab, load_previous_tabs, LoadedTab};

    let loaded_tabs = {
        let mut previous_tabs = load_previous_tabs(&edited_files_dir_buf, &edited_files_index_path_buf);

        // We return early here for similar reasons to the reasons given in the
        // comment by the FILE argument parsing code above.
        for p in extra_paths {
            previous_tabs.push(load_tab(p)?);
        }

        previous_tabs
    };

    macro_rules! wh_from_size {
        ($size: expr) => {{
            let dimensions = $size;
            sswh!{
                dimensions.width as f32,
                dimensions.height as f32,
            }
        }}
    }

    macro_rules! get_non_font_size_dependents_input {
        ($mode: expr, $dimensions: expr) => {{
            let dimensions = $dimensions;
            let FindReplaceInfo {
                find_text_xywh,
                replace_text_xywh,
                ..
            } = get_find_replace_info(dimensions);
            let GoToPositionInfo {
                input_text_xywh,
                ..
            } = get_go_to_position_info(dimensions);
            Input::SetSizeDependents(Box::new(SizeDependents {
                buffer_xywh: wimp_render::get_edit_buffer_xywh(
                    $mode,
                    dimensions
                )
                .into(),
                find_xywh: find_text_xywh.into(),
                replace_xywh: replace_text_xywh.into(),
                go_to_position_xywh: input_text_xywh.into(),
                font_info: dimensions.font.into(),
            }))
        }};
    }

    let mut r_s = {
        let dimensions = Dimensions {
            font: wimp_render::get_font_info(&char_dims),
            window: wh_from_size!(
                glutin_context
                    .window()
                    .inner_size()
            ),
        };

        let (v, c) = update_and_render(
            get_non_font_size_dependents_input!(d!(), dimensions)
        );

        let mut cmds = VecDeque::with_capacity(EVENTS_PER_FRAME);
        cmds.push_back(c);

        let mut ui: ui::State = d!();
        ui.window_is_focused = true;

        let expected_capacity_needed = (loaded_tabs.len() + 1) * 2;
        let buffer_status_map = g_i::Map::with_capacity(g_i::Length::or_max(expected_capacity_needed));

        let clipboard = get_clipboard();

        let mut view: wimp_types::View = d!();
        view.update(v);

        RunState {
            view,
            cmds,
            ui,
            buffer_status_map,
            editor_in_sink,
            dimensions,
            clipboard,
            event_proxy,
            startup_description,
            pids,
            pid_string: String::with_capacity(256),
            stats: d!(),
        }
    };

    // If you didn't click on the same symbol, counting that as a double click seems like it
    // would be annoying.
    let mouse_epsilon_radius: abs::Length = {
        let char_dim = r_s.dimensions.font.text_char_dim;
        let (w, h) = (char_dim.w, char_dim.h);

        (if w < h { w } else { h }).halve()
    };

    let (mut last_click_x, mut last_click_y) = (d!(), d!());

    let mut dt = Duration::from_nanos(((1.0 / TARGET_RATE) * 1_000_000_000.0) as u64);

    macro_rules! mouse_within_radius {
        () => {{
            let mouse_pos = &r_s.ui.mouse_pos;
            abs::Pos::abs(&(last_click_x - mouse_pos.x)) <= mouse_epsilon_radius
                && abs::Pos::abs(&(last_click_y - mouse_pos.y)) <= mouse_epsilon_radius
        }};
    }

    {
        macro_rules! call_u_and_r {
            ($input:expr) => {
                call_u_and_r!(r_s, $input)
            };
            ($vars: ident, $input:expr) => {
                call_u_and_r!(&mut $vars.ui, &$vars.editor_in_sink, $input)
            };
            ($ui: expr, $editor_in_sink: expr, $input: expr) => {{
                if cfg!(feature = "skip-updating-editor-thread") {
                    let input = $input;
                    if let Input::Quit = &input {
                        let _hope_it_gets_there = $editor_in_sink.send(input);
                    }
                } else {
                    $ui.note_interaction();
                    let _hope_it_gets_there = $editor_in_sink.send($input);
                }
            }};
        }

        for (i, LoadedTab{name, data}) in loaded_tabs.into_iter().enumerate() {
            call_u_and_r!(Input::AddOrSelectBuffer(name, data));

            let index_state = r_s.view.index_state();

            // if we bothered saving them before, they were clearly edited.
            r_s.buffer_status_map.insert(
                index_state,
                index_state.new_index(g_i::IndexPart::or_max(i)),
                BufferStatus::EditedAndSaved,
            );
        }

        type CommandVars = RunState;

        let mut r_c: RunConsts = RunConsts {
            commands: std::collections::BTreeMap::new(),
        };

        macro_rules! register_command {
            ($modifiers: expr, $main_key: ident, $label: literal, $(_)? $code: block) => {
                register_command!($modifiers, $main_key, $label, _unused_identifier $code)
            };
            ($modifiers: expr, $main_key: ident, $label: literal, $vars: ident $code: block) => {{
                fn command($vars: &mut CommandVars) {
                    $code
                }
                let key = ($modifiers, VirtualKeyCode::$main_key);  
                debug_assert!(r_c.commands.get(&key).is_none());
                r_c.commands.insert(key, LabelledCommand{ label: $label, command});
            }}
        }

        macro_rules! register_commands {
            ($([$($tokens: tt)*])+) => {
                $(
                    register_command!{
                        $($tokens)*
                    }
                )+
            }
        }

        // eventually we'll likely want to tell the editor, and have it decide whether/how
        // to display it to the user.
        macro_rules! handle_platform_error {
            ($r_s: ident, $err: expr) => {
                handle_platform_error!(&mut $r_s.ui, &$r_s.editor_in_sink, &$r_s.view, $err)
            };
            ($ui: expr, $editor_in_sink: expr, $view: expr, $err: expr) => {
                let error = format!("{},{}: {}", file!(), line!(), $err);
                eprintln!("{}", error);

                let mut saw_same_error = false;

                let view = $view;
                for (_, buffer) in view.buffer_iter() {
                    if let BufferName::Scratch(_) = &buffer.name {
                        if buffer.data.chars == error {
                            saw_same_error = true;
                            break;
                        }
                    }
                }

                if !saw_same_error {
                    call_u_and_r!($ui, $editor_in_sink, Input::NewScratchBuffer(Some(error)));
                }
            };
        }

        macro_rules! save_to_disk {
            ($path: expr, $str: expr, $buffer_index: expr) => {
                save_to_disk!(r_s, $path, $str, $buffer_index)
            };
            ($r_s: ident, $path: expr, $str: expr, $buffer_index: expr) => {
                save_to_disk!(
                    &mut $r_s.ui,
                    &$r_s.editor_in_sink,
                    &mut $r_s.buffer_status_map,
                    &$r_s.view,
                    $path,
                    $str,
                    $buffer_index
                )
            };
            ($ui: expr, $editor_in_sink: expr, $buffer_status_map: expr, $view: expr, $path: expr, $str: expr, $buffer_index: expr) => {
                let index = $buffer_index;
                match std::fs::write($path, $str) {
                    Ok(_) => {
                        transform_at(
                            $buffer_status_map,
                            $view.index_state(), 
                            index,
                            BufferStatusTransition::Save
                        );
                        call_u_and_r!($ui, $editor_in_sink, Input::SavedAs(index, $path.to_path_buf()));
                    }
                    Err(err) => {
                        handle_platform_error!($ui, $editor_in_sink, $view, err);
                    }
                }
            };
        }

        macro_rules! load_file {
            ($path: expr) => {{
                let p = $path;
                match std::fs::read_to_string(&p) {
                    Ok(s) => {
                        call_u_and_r!(Input::AddOrSelectBuffer(BufferName::Path(p), s));

                        // The main reason for this window manipulation is for after
                        // reading from the path mailbox.
                        let window = glutin_context.window();

                        // Notify the user that the file loaded, if we are not
                        // already in focus.
                        use glutin_wrapper::window::UserAttentionType;
                        window.request_user_attention(
                            Some(UserAttentionType::Informational)
                        );
                    }
                    Err(err) => {
                        handle_platform_error!(
                            r_s,
                            format!("{}\npath: {}", err, p.to_string_lossy())
                        );
                    }
                }
            }};
        }

        macro_rules! file_chooser_call {
            ($event_proxy: expr, $func: ident, $default_path: expr, $path: ident in $event: expr) => {
                let proxy =
                    std::sync::Arc::new(std::sync::Mutex::new($event_proxy.clone()));
                let proxy = proxy.clone();
                file_chooser::$func(
                    $default_path, 
                    move |$path: PathBuf| {
                        let _bye = proxy
                            .lock()
                            .expect("file_chooser thread private mutex locked!?")
                            .send_event($event);
                    }
                )
            };
        }

        macro_rules! switch_menu_mode {
            ($mode: expr) => {
                switch_menu_mode!(r_s, $mode)
            };
            ($r_s: expr, $mode: expr) => {
                let r_s = $r_s;
                let mode: MenuMode = $mode;
                
                call_u_and_r!(r_s, Input::SetMenuMode(mode));

                call_u_and_r!(r_s, Input::SetSizeDependents(
                    Box::new(SizeDependents {
                        buffer_xywh: wimp_render::get_edit_buffer_xywh(
                            mode.into(),
                            r_s.dimensions,
                        )
                        .into(),
                        find_xywh: None,
                        replace_xywh: None,
                        go_to_position_xywh: None,
                        font_info: None,
                    })
                ));
            };
        }

        use glutin_wrapper::event::*;

        let empty: ModifiersState = ModifiersState::empty();
        const LOGO: ModifiersState = ModifiersState::LOGO;
        const ALT: ModifiersState = ModifiersState::ALT;
        const CTRL: ModifiersState = ModifiersState::CTRL;
        const SHIFT: ModifiersState = ModifiersState::SHIFT;

        register_commands!{
            [empty, Apps, "Open/Close this menu.", r_s {
                r_s.view.toggle_command_menu();
            }]
            [empty, Escape, "Close menus.", r_s {
                r_s.view.close_menus();

                call_u_and_r!(r_s, Input::SetSizeDependents(
                    Box::new(SizeDependents {
                        buffer_xywh: wimp_render::get_edit_buffer_xywh(
                            d!(),
                            r_s.dimensions
                        )
                        .into(),
                        find_xywh: None,
                        replace_xywh: None,
                        go_to_position_xywh: None,
                        font_info: None,
                    })
                ));
                call_u_and_r!(r_s, Input::CloseMenuIfAny);
            }]
            [empty, F1, "Delete lines.", r_s {
                call_u_and_r!(r_s, Input::DeleteLines);
            }]
            [empty, Back, "Backspace.", r_s {
                call_u_and_r!(r_s, Input::Delete);
            }]
            [empty, Up, "Move all cursors up.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::Up));
                r_s.ui.fresh_navigation = Navigation::Up;
            }]
            [empty, Down, "Move all cursors down.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::Down));
                r_s.ui.fresh_navigation = Navigation::Down;
            }]
            [empty, Left, "Move all cursors left.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::Left));
            }]
            [empty, Right, "Move all cursors right.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::Right));
            }]
            [empty, Home, "Move all cursors to line start.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::ToLineStart));
            }]
            [empty, End, "Move all cursors to line end.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::ToLineEnd));
            }]
            [empty, Tab, "Indent in selection/line.", r_s {
                call_u_and_r!(r_s, Input::TabIn);
            }]
            [CTRL, Home, "Move cursors to start.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToBufferStart))
            }]
            [CTRL, End, "Move cursors to end.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToBufferEnd))
            }]
            [CTRL, Left, "Move cursors to previous likely edit location.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToPreviousLikelyEditLocation))
            }]
            [CTRL, Right, "Move cursors to next likely edit location.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToNextLikelyEditLocation))
            }]
            [CTRL, Key0, "Reset scroll (context sensitive).", r_s {
                let ui = &mut r_s.ui;
                let font_info = r_s.dimensions.font;
                if wimp_render::inside_tab_area(ui.mouse_pos, font_info) {
                    wimp_render::make_active_tab_visible(
                        ui,
                        &r_s.view,
                        r_s.dimensions
                    );
                } else {
                    call_u_and_r!(r_s, Input::ResetScroll);
                }
            }]
            [CTRL, A, "Select all.", state {
                call_u_and_r!(state, Input::SelectAll)
            }]
            [CTRL, C, "Copy.", state {
                call_u_and_r!(state, Input::Copy)
            }]
            [CTRL, D, "Extend selection with search.", state {
                call_u_and_r!(state, Input::ExtendSelectionWithSearch)
            }]
            [CTRL, F, "Find/Replace in current file.", r_s {
                switch_menu_mode!(r_s, MenuMode::FindReplace(FindReplaceMode::CurrentFile));
            }]
            [CTRL, G, "Go to position.", r_s {
                switch_menu_mode!(r_s, MenuMode::GoToPosition);
            }]
            [CTRL, O, "Open file.", r_s {
                file_chooser_call!(
                    r_s.event_proxy,
                    single,
                    r_s.view.current_path(),
                    p in CustomEvent::OpenFile(p)
                );
            }]
            [CTRL, P, "Switch files.", r_s {
                switch_menu_mode!(r_s, MenuMode::FileSwitcher);
            }]
            [CTRL, S, "Save.", r_s {
                let (i, buffer) = r_s.view.current_text_index_and_buffer();
                match buffer.name {
                    BufferName::Scratch(_) => {
                        file_chooser_call!(
                            r_s.event_proxy,
                            save,
                            r_s.view.current_path(),
                            p in CustomEvent::SaveNewFile(p, i)
                        );
                    }
                    BufferName::Path(ref p) => {
                        save_to_disk!(r_s, p, std::borrow::Cow::from(buffer.data.chars.clone()).as_ref(), i);
                    }
                }
            }]
            [CTRL, T, "New scratch buffer.", state {
                call_u_and_r!(state, Input::NewScratchBuffer(None));
            }]
            [CTRL, V, "Paste.", state {
                call_u_and_r!(state, Input::Paste(state.clipboard.get_contents().ok()));
            }]
            [CTRL, W, "Close tab.", r_s {
                match r_s.view.current_buffer_id() {
                    BufferId {
                        kind: BufferIdKind::Text,
                        index,
                        ..
                    } => {
                        call_u_and_r!(r_s, Input::CloseBuffer(index));
                    }
                    _ => {
                        call_u_and_r!(r_s, Input::CloseMenuIfAny);
                    }
                }
            }]
            [CTRL, X, "Cut.", state {
                call_u_and_r!(state, Input::Cut);
            }]
            [CTRL, Y, "Redo.", state {
                call_u_and_r!(state, Input::Redo);
            }]
            [CTRL, Z, "Undo.", state {
                call_u_and_r!(state, Input::Undo);
            }]
            [CTRL | SHIFT, Tab, "Previous Tab.", state {
                call_u_and_r!(
                    state,
                    Input::AdjustBufferSelection(
                        SelectionAdjustment::Previous
                    )
                );
            }]
            [CTRL, Tab, "Next tab.", state {
                call_u_and_r!(
                    state,
                    Input::AdjustBufferSelection(
                        SelectionAdjustment::Next
                    )
                );
            }]
            [CTRL | SHIFT, F1, "Add RunState snapshot", r_s {
                let snapshot = format!("{:#?}", r_s);
                call_u_and_r!(r_s, Input::NewScratchBuffer(snapshot.into()));
            }]
            [CTRL | ALT, Key0, "Insert sequential numbers at cursors.", state {
                call_u_and_r!(state, Input::InsertNumbersAtCursors);
            }]
            [CTRL | ALT, L, "Switch document parsing to next language.", state {
                call_u_and_r!(state, Input::NextLanguage);
            }]
            [CTRL | ALT | SHIFT, L, "Switch document parsing to previous language.", state {
                call_u_and_r!(state, Input::PreviousLanguage);
            }]
            [CTRL | SHIFT, Home, "Move all cursors to buffer start.", state {
                call_u_and_r!(state, Input::ExtendSelectionForAllCursors(
                    Move::ToBufferStart
                ));
            }]
            [CTRL | SHIFT, End, "Move all cursors to buffer end.", state {
                call_u_and_r!(state, Input::ExtendSelectionForAllCursors(
                    Move::ToBufferEnd
                ));
            }]
            [CTRL | SHIFT, Left, "Move all cursors to previous likely edit location.", state {
                call_u_and_r!(state, Input::ExtendSelectionForAllCursors(
                    Move::ToPreviousLikelyEditLocation
                ));
            }]
            [CTRL | SHIFT, Right, "Move all cursors to next likely edit location.", state {
                call_u_and_r!(state, Input::ExtendSelectionForAllCursors(
                    Move::ToNextLikelyEditLocation
                ));
            }]
            [CTRL | SHIFT, S, "Save new file.", r_s {
                let i = r_s.view.current_text_index();
                file_chooser_call!(
                    r_s.event_proxy,
                    save,
                    r_s.view.current_path(),
                    p in CustomEvent::SaveNewFile(p, i)
                );
            }]
            [CTRL | SHIFT, Z, "Redo.", state {
                call_u_and_r!(state, Input::Redo);
            }]
            [CTRL | SHIFT, Slash, "Toggle debug menu.", r_s {
                r_s.view.toggle_debug_menu();
            }]
            [SHIFT, Tab, "Indent out selection/line.", r_s {
                call_u_and_r!(r_s, Input::TabOut);
            }]
            [SHIFT, Up, "Extend selection(s) upward.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::Up));
                r_s.ui.fresh_navigation = Navigation::Up;
            }]
            [SHIFT, Down, "Extend selection(s) downward.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::Down));
                r_s.ui.fresh_navigation = Navigation::Down;
            }]
            [SHIFT, Left, "Extend selection(s) leftward.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::Left));
            }]
            [SHIFT, Right, "Extend selection(s) rightward.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::Right));
            }]
            [SHIFT, Home, "Extend selection(s) to line start.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::ToLineStart));
            }]
            [SHIFT, End, "Extend selection(s) to line end.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::ToLineEnd));
            }]
            [SHIFT, Return, "Insert new line.", r_s {
                // TODO: Do we actually need this command?
                call_u_and_r!(r_s, Input::Insert('\n'));
            }]
            [LOGO | CTRL, Tab, "Move current tab right.", r_s {
                call_u_and_r!(r_s, Input::AdjustBufferSelection(
                    SelectionAdjustment::Move(SelectionMove::Right)
                ));
            }]
            [LOGO | CTRL | SHIFT, Tab, "Move current tab left.", r_s {
                call_u_and_r!(r_s, Input::AdjustBufferSelection(
                    SelectionAdjustment::Move(SelectionMove::Left)
                ));
            }]
            [LOGO | CTRL, Home, "Move current tab to start of row.", r_s {
                call_u_and_r!(r_s, Input::AdjustBufferSelection(
                    SelectionAdjustment::Move(SelectionMove::ToStart)
                ));
            }]
            [LOGO | CTRL, End, "Move current tab to end of row.", r_s {
                call_u_and_r!(r_s, Input::AdjustBufferSelection(
                    SelectionAdjustment::Move(SelectionMove::ToEnd)
                ));
            }]
        }

        macro_rules! perform_command {
            ($key: expr) => {
                if let Some(LabelledCommand{ label, command }) = r_c.commands.get($key) {
                    dbg!(label);
                    command(&mut r_s);
                }
            }
        }

        events.run(move |event, _, control_flow| {
            match event {
                Event::WindowEvent { event, .. } => {
                    macro_rules! quit {
                        () => {{
                            perf_viz::end_record!("main loop");
                            call_u_and_r!(Input::Quit);
                            let _hope_it_gets_there = edited_files_in_sink.send(EditedFilesThread::Quit);
                            let _hope_it_gets_there = path_mailbox_in_sink.send(PathMailboxThread::Quit);
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

                            match path_mailbox_join_handle.take() {
                                Some(j_h) => j_h.join().expect("Could not join path_mailbox thread!"),
                                None => {}
                            };

                            perf_viz::output!();

                            let _ = gl_layer::cleanup(&gl_state);

                            *control_flow = glutin_wrapper::event_loop::ControlFlow::Exit;
                        }};
                    }

                    macro_rules! text_box_xy {
                        () => {{
                            let view = &r_s.view;
                            let xy = wimp_render::get_current_buffer_rect(
                                view.current_buffer_kind(),
                                view.menu_mode(),
                                r_s.dimensions
                            )
                            .xy;

                            screen_to_text_box(r_s.ui.mouse_pos, xy)
                        }};
                    }

                    if cfg!(feature = "print-raw-input") {
                        if let WindowEvent::KeyboardInput { ref input, .. } = event {
                            println!(
                                "{:?}",
                                (
                                    input.virtual_keycode.unwrap_or(VirtualKeyCode::WebStop),
                                    input.state
                                )
                            );
                        }
                    }

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
                            r_s.dimensions.window = wh_from_size!(size);
                            call_u_and_r!(
                                get_non_font_size_dependents_input!(
                                    r_s.view.menu_mode(),
                                    r_s.dimensions
                                )
                            );
                            let sswh!(w, h) = r_s.dimensions.window;
                            gl_layer::set_dimensions(
                                &mut gl_state,
                                hidpi_factor as _,
                                (w.get() as _, h.get() as _),
                            );
                        }
                        WindowEvent::Focused(is_focused) => {
                            dbg!("set to ", is_focused);
                            r_s.ui.window_is_focused = is_focused;
                            if is_focused {
                                // X11 requires us to explicitly unset the window
                                // attention.
                                glutin_context.window().request_user_attention(
                                    None
                                );
                            }
                        }
                        WindowEvent::ReceivedCharacter(mut c) => {
                            if c != '\u{1}'     // "start of heading" (sent with Ctrl-a)
                             && c != '\u{3}'    // "end of text" (sent with Ctrl-c)
                             && c != '\u{4}'    // "end of transmission" (sent with Ctrl-d)
                             && c != '\u{6}'    // "acknowledge" (sent with Ctrl-f)
                             && c != '\u{7}'    // bell (sent with Ctrl-g)
                             && c != '\u{8}'    // backspace (sent with Ctrl-h)
                             && c != '\u{9}'    // horizontal tab (sent with Ctrl-i)
                             && c != '\u{c}'    // new page/form feed (sent with Ctrl-l)
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
                             && c != '\u{7f}'   // delete
                            {
                                if c == '\r' {
                                    c = '\n';
                                }

                                if c == '\n' {
                                    use BufferIdKind::*;
                                    match r_s.view.current_buffer_kind() {
                                        None | FileSwitcher => {
                                            r_s.ui.fresh_navigation = Navigation::Interact;
                                        }
                                        Text => {
                                            call_u_and_r!(Input::Insert(c));
                                        }
                                        Find | Replace | GoToPosition => {
                                            call_u_and_r!(Input::SubmitForm);
                                        }
                                    }
                                } else {
                                    call_u_and_r!(Input::Insert(c));
                                }
                            }
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
                        } => {
                            perform_command!(&(modifiers, keypress))
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers,
                            ..
                        } if modifiers.is_empty() => {
                            let ui = &mut r_s.ui;
                            let scroll_y = y * wimp_render::SCROLL_MULTIPLIER;
                            if wimp_render::inside_tab_area(ui.mouse_pos, r_s.dimensions.font) {
                                ui.tab_scroll -= scroll_y;
                            } else {
                                call_u_and_r!(Input::ScrollVertically(scroll_y));
                            }
                        }
                        WindowEvent::MouseWheel {
                            delta: MouseScrollDelta::LineDelta(_, y),
                            modifiers,
                            ..
                        } if modifiers == SHIFT => {
                            let ui = &mut r_s.ui;
                            let scroll_y = y * wimp_render::SCROLL_MULTIPLIER;
                            if wimp_render::inside_tab_area(ui.mouse_pos, r_s.dimensions.font) {
                                ui.tab_scroll -= scroll_y;
                            } else {
                                call_u_and_r!(Input::ScrollHorizontally(scroll_y));
                            }
                        }
                        WindowEvent::CursorMoved {
                            position,
                            modifiers,
                            ..
                        } => {
                            let ui = &mut r_s.ui;
                            let LogicalPosition::<f32> { x, y } = position.to_logical(get_hidpi_factor!());
                            ui.mouse_pos = ssxy!{
                                x,
                                y,
                            };

                            match modifiers {
                                m if m.is_empty() => {
                                    let cursor_icon = if wimp_render::should_show_text_cursor(
                                        ui.mouse_pos,
                                        r_s.view.menu_mode(),
                                        r_s.dimensions
                                    ) {
                                        glutin_wrapper::window::CursorIcon::Text
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
                            modifiers,
                            ..
                        } // allow things like Shift-Alt-Click
                        if (!modifiers).intersects(!CTRL) => {
                            r_s.ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;

                            let replace_or_add = if modifiers.ctrl() {
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
                            let ui = &mut r_s.ui;
                            ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;
                            last_click_x = ui.mouse_pos.x;
                            last_click_y = ui.mouse_pos.y;
                        },
                        _ => {}
                    }
                }
                Event::MainEventsCleared if running => {
                    perf_viz::start_record!("MainEventsCleared");
                    let index_state = r_s.view.index_state();
                    let buffer_status_map = &mut r_s.buffer_status_map;

                    for _ in 0..EVENTS_PER_FRAME {
                        match editor_out_source.try_recv() {
                            Ok(EditorThreadOutput::Rendered((v, c))) => {
                                r_s.view.update(v);
                                for (i, e_t) in r_s.view.edited_transitions() {
                                    transform_at(
                                        buffer_status_map,
                                        index_state,
                                        i,
                                        BufferStatusTransition::from(e_t)
                                    );
                                }
                                

                                r_s.cmds.push_back(c);
                            }
                            Ok(EditorThreadOutput::Pid(pid)) => {
                                r_s.pids.editor = pid;
                            }
                            _ => break,
                        };
                    }

                    for _ in 0..EVENTS_PER_FRAME {
                        match edited_files_out_source.try_recv() {
                            Ok((index, transition)) => {
                                transform_at(
                                    buffer_status_map,
                                    index_state,
                                    index,
                                    transition
                                );
                            }
                            _ => break,
                        };
                    }

                    buffer_status_map.migrate_all(index_state);

                    // Queue a RedrawRequested event so we draw the updated view quickly.
                    glutin_context.window().request_redraw();
                    perf_viz::end_record!("MainEventsCleared");
                }
                Event::RedrawRequested(_) => {
                    perf_viz::start_record!("frame");
                    r_s.stats = d!();

                    r_s.ui.frame_init();

                    let sswh!(width, height) = r_s.dimensions.window;

                    let ViewOutput { text_or_rects, action } =
                        wimp_render::view(
                            &mut r_s,
                            &r_c,
                            dt,
                        );

                    gl_layer::render(&mut gl_state, text_or_rects, width.get() as _, height.get() as _)
                        .expect("gl_layer::render didn't work");

                    perf_viz::start_record!("swap_buffers");
                    glutin_context
                        .swap_buffers()
                        .expect("swap_buffers didn't work!");
                    perf_viz::end_record!("swap_buffers");

                    perf_viz::start_record!("r_s.cmds");
                    for _ in 0..EVENTS_PER_FRAME {
                        if let Some(cmd) = r_s.cmds.pop_front() {
                            match cmd {
                                Cmd::SetClipboard(s) => {
                                    if let Err(err) = r_s.clipboard.set_contents(s) {
                                        handle_platform_error!(r_s, err);
                                    }
                                }
                                Cmd::LoadFile(path) => load_file!(path),
                                Cmd::None => {}
                            }
                        } else {
                            break;
                        }
                    }
                    perf_viz::end_record!("r_s.cmds");

                    match action {
                        ViewAction::Input(input) => {
                            perf_viz::start_record!("ViewAction::Input");
                            call_u_and_r!(input);
                            perf_viz::end_record!("ViewAction::Input");
                        }
                        ViewAction::Command(key) => {
                            perf_viz::start_record!("ViewAction::Command");
                            perform_command!(&key);
                            perf_viz::end_record!("ViewAction::Command");
                        }
                        ViewAction::None => {}
                    }

                    perf_viz::start_record!("report_rate");
                    if let Some(render_rate) = loop_helper.report_rate() {
                        macro_rules! ms_from_span {
                            ($span: expr) => {
                                $span
                                .duration_or_default().as_micros() as f32 / 1000.0
                            }
                        }
                        let wimp_stats = r_s.stats;

                        let view_function_ms = ms_from_span!(
                            wimp_stats.latest_view_function_time_span
                        );

                        let editor_stats = r_s.view.stats();
                        // TODO move the final string into editor_view, as a 
                        // secondary status line? Either this and the status
                        // line should both be in editor_view, or neither of
                        // them should be.

                        let editor_overall_ms = ms_from_span!(
                            editor_stats.latest_overall_time_span
                        );

                        let editor_update_ms = ms_from_span!(
                            editor_stats.latest_update_time_span
                        );

                        let editor_render_ms = ms_from_span!(
                            editor_stats.latest_render_time_span
                        );

                        let editor_buffer_render_ms = ms_from_span!(
                            editor_stats.latest_buffer_render_time_span
                        );

                        let parse_total = format!(
                            "{: >6.3} ms",
                            {
                                let mut total = 0.0;
                                for span in editor_stats.latest_parse_time_spans.iter() {
                                    use TimeSpan::*;
                                    match span {
                                        NotStarted | Started(_) => {},
                                        Ended(duration) => {
                                            total += duration.as_micros() as f32 / 1000.;
                                        },
                                    }
                                }
                                total
                            }
                        );

                        glutin_context.window().set_title(&format!(
                            "{}{} {:.0} FPS v{: >6.3} ms e{: >6.3} ms(e-u{: >6.3} ms e-r{: >6.3} ms(e-br{: >6.3} ms p{})) {:?} click {:?}",
                            title,
                            if cfg!(debug_assertions) {
                                " DEBUG"
                            } else {
                                ""
                            },
                            render_rate, // AKA FPS
                            view_function_ms,
                            editor_overall_ms,
                            editor_update_ms,
                            editor_render_ms,
                            editor_buffer_render_ms,
                            parse_total,
                            (r_s.ui.mouse_pos.x, r_s.ui.mouse_pos.y),
                            (last_click_x, last_click_y),
                        ));
                    }
                    perf_viz::end_record!("report_rate");

                    r_s.ui.frame_end();
                    perf_viz::end_record!("frame");
                    perf_viz::start_record!("sleepin'");
                    if cfg!(feature="no-spinning-sleep") {
                        loop_helper.loop_sleep_no_spin();
                    } else if r_s.ui.window_is_focused {
                        loop_helper.loop_sleep();
                    } else {
                        loop_helper.loop_sleep_no_spin();
                    }
                    perf_viz::end_record!("sleepin'");

                    perf_viz::end_record!("main loop");
                    perf_viz::start_record!("main loop");

                    // We want to track the time that the message loop takes too!
                    dt = loop_helper.loop_start();
                }
                Event::UserEvent(e) => match e {
                    CustomEvent::Pid(kind, pid) => {
                        match kind {
                            PidKind::PathMailbox => {
                                r_s.pids.path_mailbox = pid;
                            }
                        }
                    },
                    CustomEvent::OpenFile(p) => load_file!(p),
                    CustomEvent::SaveNewFile(ref p, index) => {
                        let r_s = &mut r_s;
                        // The fact we need to store the index and retreive it later, potentially
                        // across multiple updates, is why this thread needs to know about the
                        // generational indices.
                        if let Some(b) = r_s.view.get_buffer(index)
                        {
                            save_to_disk!(r_s, p, std::borrow::Cow::from(b.data.chars.clone()).as_ref(), index);
                        }
                    }
                    CustomEvent::SendBuffersToBeSaved => {
                        let view = &r_s.view;
                        let index_state = view.index_state();
                        let buffer_status_map = &mut r_s.buffer_status_map;
                        let _hope_it_gets_there = edited_files_in_sink.send(
                            EditedFilesThread::Buffers(
                                index_state,
                                view.buffer_iter().map(|(i, b)|
                                    (
                                        b.to_owned(), 
                                        buffer_status_map
                                            .get(index_state, i)
                                            .cloned()
                                            .unwrap_or_default()
                                    )
                                ).collect()
                            )
                        );
                    }
                    CustomEvent::EditedBufferError(e) => {
                        // TODO show warning dialog to user and ask if they want to continue
                        // without edited file saving or save and restart. If they do, then it
                        // should be made obvious visually that the feature is not working right
                        // now.
                        handle_platform_error!(r_s, e);
                    }
                },
                Event::NewEvents(StartCause::Init) => {
                    // At least try to measure the first frame accurately
                    perf_viz::start_record!("main loop");
                    dt = loop_helper.loop_start();
                }
                _ => {}
            }
        });
    }
}

fn canonical_or_same<P: AsRef<Path>>(p: P) -> PathBuf {
    let path = p.as_ref();

    path.canonicalize().unwrap_or_else(|_| path.into())
}