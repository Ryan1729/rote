// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

#![deny(unused)]

use std::{
    collections::VecDeque,
    io::Write,
    path::{PathBuf},
    time::Duration,
};
use wimp_render::{get_find_replace_info, FindReplaceInfo, get_go_to_position_info, GoToPositionInfo, ViewOutput, ViewAction};
use wimp_types::{ui, ui::{PhysicalButtonState, Navigation}, transform_at, BufferStatus, BufferStatusTransition, CustomEvent, get_clipboard, Dimensions, LabelledCommand, RunConsts, RunState, Pids, PidKind, EditorThreadInput, EditedFilesThread, ViewRunState, DebugMenuState, DpiFactor};
use macros::{d, dbg};
use platform_types::{screen_positioning::screen_to_text_box, *};
use shared::{Res};
use edited_storage::{canonicalize, load_tab, load_previous_tabs, LoadedTab};

/// # Errors
/// Returns an error if there is an error before the eventloop has started.
#[perf_viz::record]
pub fn run(
    editor_api: EditorAPI
) -> Res<()> {
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
                for arg in &accepted_args {
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
                    println!();
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

                data_dir = data_dir.map(|dd| dd.canonicalize().unwrap_or(dd));
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
                    window_layer::ScaleFactor::from_str(&s)
                        .ok()
                });
            }
            LICENSE => {
                println!("{} program by Ryan Wiedemann.", title);
                println!("Source and license available at:");
                println!("    https://github.com/Ryan1729/{}", title);
                println!();
                println!("License for the font:");
                println!("{}", window_layer::FONT_LICENSE);
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
                // whatever, so they can correct the file name. Hence, `?`.
                .map(PathBuf::from)?;

                // See comment above for why `?`.
                extra_paths.push(canonicalize(path)?);
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
    use atomically::Overwrites;
    let previous_instance_is_running = {
        let res = atomically::write(
            &running_lock_path,
            Overwrites::Disallow,
            |f| {
                // In some sense it doesn't matter what we write here, but it seems like it
                // would be nice for it to be different each time it is written.
                use std::time::{SystemTime, UNIX_EPOCH};
                let thing_to_write = match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(duration) => format!("{}\n", duration.as_nanos()),
                    Err(e) => format!("-{}\n", e.duration().as_nanos()),
                };
    
                f.write_all(thing_to_write.as_bytes())
            }
        );

        match res {
            Ok(()) => false,
            Err(io_error) if io_error.kind() == std::io::ErrorKind::AlreadyExists =>  {
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

                    // `read_to_string` apparently already reads the file metadata
                    // to figure out how much to reserve. TODO: confirm this.
                    let mut path_list = String::new();

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
                            writeln!(&mut path_list, "{}", p.to_string_lossy());
                    }

                    path_list
                };

                atomically::write(
                    &path_mailbox_path,
                    Overwrites::Allow,
                    |f| {
                        f.write_all(path_list.as_bytes())
                    }
                ).map_err(From::from)
            };

            let res = inner(path_mailbox_path);

            if let Ok(()) = res {
                println!("Seems like we were able to write to the mailbox.");
            } else {
                println!("Mailbox write failed.");
                return res;
            }
        }

        std::process::exit(0)
    }

    let edited_files_dir_buf = data_dir.join("edited_files_v1/");
    let edited_files_index_path_buf = data_dir.join("edited_files_v1_index.txt");

    use window_layer::ScaleFactor;
    const HIDPI_INCREMENT_BY: ScaleFactor = 1./12.;
    const HIDPI_MINIMUM: ScaleFactor = HIDPI_INCREMENT_BY;
    const HIDPI_DEFAULT: ScaleFactor = 1.;

    fn on_shutdown(running_lock_path: PathBuf) {
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
    }

    let window_state: window_layer::State<'_, CustomEvent> =
        match window_layer::init::<'_, '_, CustomEvent>(
            hidpi_factor_override.unwrap_or(HIDPI_DEFAULT),
            wimp_render::TEXT_BACKGROUND_COLOUR,
            title.into(),
        ) {
            Ok(s) => s,
            Err(e) => {
                on_shutdown(running_lock_path);
                return Err(e)
            },
        };

    let event_proxy = window_layer::create_event_proxy(&window_state);

    let startup_description = format!(
        "data_dir: \n{}", data_dir.to_string_lossy()
    );

    use std::sync::mpsc::{channel};


    let pids = Pids { window: std::process::id(), ..d!() };

    let (path_mailbox_in_sink, path_mailbox_join_handle)
        = path_mailbox::start_thread(
            path_mailbox::Paths {
                running_lock: running_lock_path,
                mailbox: path_mailbox_path,
            },
            event_proxy.clone(),
            on_shutdown,
        );
    let mut path_mailbox_join_handle = Some(path_mailbox_join_handle);

    // into the edited files thread
    let (edited_files_in_sink, edited_files_in_source) = channel();
    // out of the edited files thread
    let (edited_files_out_sink, edited_files_out_source) = channel();

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

    let loaded_tabs = {
        let mut previous_tabs = load_previous_tabs(&edited_files_dir_buf, &edited_files_index_path_buf);

        // We return early here for similar reasons to the reasons given in the
        // comment by the FILE argument parsing code above.
        for c_p in extra_paths {
            previous_tabs.push(load_tab(c_p)?);
        }

        previous_tabs
    };

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

    let (mut r_s, editor_in_sink, editor_out_source, mut editor_join_handle) = {
        let char_dims = window_layer::get_char_dims(
            &window_state,
            &wimp_render::TEXT_SIZES,
        );

        let current_hidpi_factor = HIDPI_DEFAULT;
        let resolved_hidpi_factor = hidpi_factor_override.unwrap_or(current_hidpi_factor);

        let dimensions = Dimensions {
            font: wimp_render::get_font_info(&char_dims),
            window: window_layer::dimensions(&window_state),
            hidpi_factor_override,
            current_hidpi_factor,
        };

        let (v, c) = (editor_api.update_and_render)(
            get_non_font_size_dependents_input!(d!(), dimensions)
        );

        let (editor_in_sink, editor_out_source, editor_join_handle)
            = editor_thread::start(
                editor_api,
                event_proxy.clone(),
                edited_files_in_sink.clone(),
            );

        let mut cmds = VecDeque::with_capacity(EVENTS_PER_FRAME);
        cmds.push_back(c);

        let mut ui: ui::State = d!();
        ui.window_is_focused = true;

        let expected_capacity_needed = (loaded_tabs.len() + 1) * 2;
        let buffer_status_map = g_i::Map::with_capacity(g_i::Length::or_max(expected_capacity_needed));

        let clipboard = get_clipboard();

        let editor_buffers_size_in_bytes = v.stats.editor_buffers_size_in_bytes;

        let mut view: wimp_types::View = d!();

        view.update(v, d!());

        (
            RunState {
                view_state: ViewRunState {
                    view,
                    ui,
                    buffer_status_map,
                    dimensions,
                    debug_menu_state: DebugMenuState {
                        preallocated_scratch: String::with_capacity(1024),
                        startup_description,
                        pids,
                        editor_buffers_size_in_bytes,
                        last_hidpi_factors: [
                            resolved_hidpi_factor,
                            0.,
                            0.,
                            0.,
                        ],
                        ..d!()
                    },
                    stats: d!(),
                },
                cmds,
                clipboard,
                editor_in_sink: editor_in_sink.clone(),
                event_proxy,
            },
            editor_in_sink,
            editor_out_source,
            Some(editor_join_handle)
        )
    };

    macro_rules! get_hidpi_factor {
        ($r_s: expr) => {
            v_s!($r_s).dimensions.hidpi_factor_override.unwrap_or(v_s!($r_s).dimensions.current_hidpi_factor)
        };
    }

    macro_rules! v_s {
        () => {
            r_s.view_state
        };
        ($r_s: expr) => {
            $r_s.view_state
        };
    }

    // If you didn't click on the same symbol, counting that as a double click seems like it
    // would be annoying.
    let mouse_epsilon_radius: abs::Length = {
        let char_dim = v_s!().dimensions.font.text_char_dim;
        let (w, h) = (char_dim.w, char_dim.h);

        (if w < h { w } else { h }).halve()
    };

    let (mut last_click_x, mut last_click_y) = (d!(), d!());

    const TARGET_RATE: window_layer::RatePerSecond = 128.0; //250.0);
    let mut dt = window_layer::initial_dt(TARGET_RATE.into());

    {
        macro_rules! mouse_within_radius {
            () => {{
                let mouse_pos = &v_s!().ui.mouse_pos;
                // TODO needing to add zero is suspect.
                let zero = abs::Pos::default();
                abs::Pos::abs(&(last_click_x - zero + mouse_pos.x)) <= mouse_epsilon_radius
                    && abs::Pos::abs(&(last_click_y - zero + mouse_pos.y)) <= mouse_epsilon_radius
            }};
        }

        use window_layer::{MouseButton, ModifiersState, KeyCode};

        macro_rules! call_u_and_r {
            ($input:expr) => {
                call_u_and_r!(r_s, $input)
            };
            ($vars: ident, $input:expr) => {{
                let editor_in_sink = &$vars.editor_in_sink;
                let input = $input;

                if cfg!(feature = "skip-updating-editor-thread") {
                    if let Input::Quit = &input {
                        let _hope_it_gets_there = editor_in_sink.send(
                            EditorThreadInput::Render(input)
                        );
                    }
                } else {
                    let ui: &mut ui::State = &mut v_s!($vars).ui;

                    ui.note_interaction();
                    let send_through = match ui.keyboard.hot {
                        ui::Id::TaggedListSelection(
                            ui::Tag::FileSwitcherResults
                            | ui::Tag::CommandMenu,
                            _
                        ) => {
                            match input {
                                Input::Insert('\n') => {
                                    ui.set_fresh_navigation(Navigation::Interact);
                                    false
                                },
                                Input::MoveAllCursors(Move::Up)
                                | Input::ExtendSelectionForAllCursors(Move::Up) => {
                                    ui.set_fresh_navigation(Navigation::Up);
                                    false
                                },
                                Input::MoveAllCursors(Move::Down)
                                | Input::ExtendSelectionForAllCursors(Move::Down) => {
                                    ui.set_fresh_navigation(Navigation::Down);
                                    false
                                },
                                _ => true,
                            }
                        },
                        _ => true,
                    };

                    if send_through {
                        let _hope_it_gets_there = editor_in_sink.send(
                            EditorThreadInput::Render(input)
                        );
                    }
                }
            }};
        }

        for (i, LoadedTab{ name, data, position }) in loaded_tabs.into_iter().enumerate() {
            let input = if let Some(position) = position {
                Input::AddOrSelectBufferThenGoTo(name, data, position)
            } else {
                Input::AddOrSelectBuffer(name, data)
            };
            call_u_and_r!(input);

            let index_state = v_s!().view.index_state();

            // if we bothered saving them before, they were clearly edited.
            v_s!().buffer_status_map.insert(
                index_state,
                index_state.new_index(g_i::IndexPart::or_max(i)),
                BufferStatus::EditedAndSaved,
            );
        }

        fn set_current_hidpi_factor(
            r_s: &mut RunState,
            fns: &mut window_layer::Fns,
            mut factor: DpiFactor
        ) {
            if !(factor >= HIDPI_MINIMUM) {
                // We negate so that NaN goes here
                factor = HIDPI_MINIMUM;
            }
            v_s!(r_s).dimensions.current_hidpi_factor = factor;
            let hidpi_factor = get_hidpi_factor!(r_s);

            v_s!(r_s).debug_menu_state.last_hidpi_factors.rotate_right(1);
            v_s!(r_s).debug_menu_state.last_hidpi_factors[0] = hidpi_factor;

            fns.set_dimensions(
                hidpi_factor as _,
                v_s!(r_s).dimensions.window,
            );

            let char_dims = fns.get_char_dims(
                &wimp_render::TEXT_SIZES,
            );

            v_s!(r_s).dimensions.font = wimp_render::get_font_info(&char_dims);
        }


        type CommandVars = RunState;

        let mut r_c: RunConsts = RunConsts {
            commands: std::collections::BTreeMap::new(),
        };

        macro_rules! register_command {
            ($modifiers: expr, $main_key: ident, $label: literal, $r_s: ident $code: block) => {{
                register_command!($modifiers, $main_key, $label, $r_s, _unused_identifier $code)
            }};
            ($modifiers: expr, $main_key: ident, $label: literal, $r_s: ident $(,)? $fns: ident $code: block) => {{
                fn command(
                    $r_s: &mut CommandVars,
                    $fns: &mut window_layer::Fns<'_, '_, '_, '_, '_, '_>,
                ) {
                    $code
                }
                let key = ($modifiers, KeyCode::$main_key);
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

        macro_rules! handle_platform_error {
            ($r_s: ident, $err: expr) => {
                handle_platform_error!(&mut $r_s.ui, &$r_s.editor_in_sink, &$r_s.view, $r_s.editor_api.load_buffer_view, $err)
            };
            ($ui: expr, $editor_in_sink: expr, $view: expr, $load_buffer_view: expr, $err: expr) => {
                let error = format!("{},{}: {}", file!(), line!(), $err);
                eprintln!("{}", error);

                let _hope_that_gets_there = $editor_in_sink.send(
                    EditorThreadInput::Render(Input::ShowError(error))
                );
            };
        }

        macro_rules! save_to_disk {
            ($r_s: ident, $path: expr, $label: expr, $buffer_index: expr) => {
                save_to_disk!(
                    &$r_s.editor_in_sink,
                    $path,
                    $label,
                    $buffer_index
                )
            };
            ($editor_in_sink: expr, $path: expr, $label: expr, $buffer_index: expr) => {
                let _hope_that_gets_there = $editor_in_sink.send(
                    EditorThreadInput::SaveToDisk($path, $label, $buffer_index)
                );
            };
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
            ($r_s: expr, $mode: expr) => {
                let r_s = $r_s;
                let mode: MenuMode = $mode;

                call_u_and_r!(r_s, Input::SetMenuMode(mode));

                call_u_and_r!(r_s, Input::SetSizeDependents(
                    Box::new(SizeDependents {
                        buffer_xywh: wimp_render::get_edit_buffer_xywh(
                            mode.into(),
                            v_s!(r_s).dimensions,
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

        let empty: ModifiersState = ModifiersState::empty();
        const LOGO: ModifiersState = ModifiersState::LOGO;
        const ALT: ModifiersState = ModifiersState::ALT;
        const CTRL: ModifiersState = ModifiersState::CTRL;
        const SHIFT: ModifiersState = ModifiersState::SHIFT;

        register_commands!{
            [empty, Apps, "Open/Close this menu.", r_s {
                v_s!(r_s).view.toggle_command_menu();
            }]
            [empty, Escape, "Close menus.", r_s {
                v_s!(r_s).view.close_menus();

                call_u_and_r!(r_s, Input::SetSizeDependents(
                    Box::new(SizeDependents {
                        buffer_xywh: wimp_render::get_edit_buffer_xywh(
                            d!(),
                            v_s!(r_s).dimensions
                        )
                        .into(),
                        find_xywh: None,
                        replace_xywh: None,
                        go_to_position_xywh: None,
                        font_info: None,
                    })
                ));
                call_u_and_r!(r_s, Input::Escape);
            }]
            [empty, F1, "Delete lines.", r_s {
                call_u_and_r!(r_s, Input::DeleteLines);
            }]
            [empty, F3, "Toggle Case", state {
                call_u_and_r!(state, Input::ToggleCase);
            }]
            [empty, F5, "Strip trailing whitepace", r_s {
                call_u_and_r!(r_s, Input::StripTrailingWhitespace);
            }]
            [empty, Back, "Backspace.", r_s {
                call_u_and_r!(r_s, Input::Delete);
            }]
            [empty, Up, "Move all cursors up.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::Up));
            }]
            [empty, Down, "Move all cursors down.", r_s {
                call_u_and_r!(r_s, Input::MoveAllCursors(Move::Down));
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
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToBufferStart));
            }]
            [CTRL, End, "Move cursors to end.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToBufferEnd));
            }]
            [CTRL, Left, "Move cursors to previous likely edit location.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToPreviousLikelyEditLocation));
            }]
            [CTRL, Right, "Move cursors to next likely edit location.", state {
                call_u_and_r!(state, Input::MoveAllCursors(Move::ToNextLikelyEditLocation));
            }]
            [CTRL, Key0, "Reset scroll (context sensitive).", r_s {
                let ui = &mut v_s!(r_s).ui;
                let font_info = v_s!(r_s).dimensions.font;
                if wimp_render::inside_tab_area(ui.mouse_pos, font_info) {
                    wimp_render::make_active_tab_visible(
                        ui,
                        &v_s!(r_s).view,
                        v_s!(r_s).dimensions
                    );
                } else {
                    call_u_and_r!(r_s, Input::ResetScroll);
                }
            }]
            [CTRL, A, "Select all.", state {
                call_u_and_r!(state, Input::SelectAll);
            }]
            [CTRL, C, "Copy.", state {
                call_u_and_r!(state, Input::Copy);
            }]
            [CTRL, D, "Extend selection with search.", state {
                call_u_and_r!(state, Input::ExtendSelectionWithSearch);
            }]
            [CTRL, E, "Toggle Single-Line Comments", state {
                call_u_and_r!(state, Input::ToggleSingleLineComments);
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
                    v_s!(r_s).view.current_path(),
                    p in CustomEvent::OpenFile(p)
                );
            }]
            [CTRL, P, "Switch files.", r_s {
                switch_menu_mode!(r_s, MenuMode::FileSwitcher);
            }]
            [CTRL, S, "Save.", r_s {
                let (i, label) = v_s!(r_s).view.current_text_index_and_buffer_label();
                match label.name.clone() {
                    BufferName::Scratch(_) => {
                        file_chooser_call!(
                            r_s.event_proxy,
                            save,
                            v_s!(r_s).view.current_path(),
                            p in CustomEvent::SaveNewFile(p, i)
                        );
                    }
                    BufferName::Path(p) => {
                        save_to_disk!(
                            r_s,
                            p,
                            label.clone(),
                            i
                        );
                    }
                }
            }]
            [CTRL, T, "New scratch buffer.", state {
                call_u_and_r!(state, Input::NewScratchBuffer(None));
            }]
            [CTRL, V, "Paste.", state {
                call_u_and_r!(state, Input::Paste(state.clipboard.get().ok()));
            }]
            [CTRL, W, "Close tab.", r_s {
                match v_s!(r_s).view.current_buffer_id() {
                    BufferId {
                        kind: BufferIdKind::Text,
                        index,
                        ..
                    } => {
                        call_u_and_r!(r_s, Input::CloseBuffer(index));
                    }
                    _ => {
                        call_u_and_r!(r_s, Input::Escape);
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
            [CTRL | ALT, D, "Duplicate Lines", state {
                call_u_and_r!(state, Input::DuplicateLines);
            }]
            [CTRL | ALT, L, "Switch document parsing to next language.", state {
                call_u_and_r!(state, Input::NextLanguage);
            }]
            [CTRL | ALT | SHIFT, D, "Extend selection maximally with search.", state {
                call_u_and_r!(state, Input::ExtendSelectionMaximallyWithSearch);
            }]
            [CTRL | ALT | SHIFT, L, "Switch document parsing to previous language.", state {
                call_u_and_r!(state, Input::PreviousLanguage);
            }]
            [CTRL | ALT, Equals /* AKA unshifted Plus */, "Increase DPI factor.", r_s, fns {
                set_current_hidpi_factor(r_s, fns, get_hidpi_factor!(r_s) + HIDPI_INCREMENT_BY);
            }]
            [CTRL | ALT, Minus, "Decrease DPI factor.", r_s, fns {
                set_current_hidpi_factor(r_s, fns, get_hidpi_factor!(r_s) - HIDPI_INCREMENT_BY);
            }]
            [CTRL | ALT, Key1, "Reset DPI factor.", r_s, fns {
                set_current_hidpi_factor(r_s, fns, HIDPI_DEFAULT);
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
                let i = v_s!(r_s).view.current_text_index();
                file_chooser_call!(
                    r_s.event_proxy,
                    save,
                    v_s!(r_s).view.current_path(),
                    p in CustomEvent::SaveNewFile(p, i)
                );
            }]
            [CTRL | SHIFT, Z, "Redo.", state {
                call_u_and_r!(state, Input::Redo);
            }]
            [CTRL | SHIFT, Slash, "Toggle debug menu.", r_s {
                v_s!(r_s).view.toggle_debug_menu();
            }]
            [SHIFT, Tab, "Indent out selection/line.", r_s {
                call_u_and_r!(r_s, Input::TabOut);
            }]
            [SHIFT, Up, "Extend selection(s) upward.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::Up));
            }]
            [SHIFT, Down, "Extend selection(s) downward.", r_s {
                call_u_and_r!(r_s, Input::ExtendSelectionForAllCursors(Move::Down));
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

        window_state.run(TARGET_RATE.into(), move |event, mut fns| {
            use window_layer::{Event, ElementState, MouseScrollDelta};

            macro_rules! quit {
                () => {{
                    perf_viz::end_record!("main loop");
                    call_u_and_r!(Input::Quit);
                    let _hope_it_gets_there = edited_files_in_sink.send(EditedFilesThread::Quit);
                    let _hope_it_gets_there = path_mailbox_in_sink.send(path_mailbox::Thread::Quit);

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

                    fns.quit();
                }};
            }

            if cfg!(feature = "print-raw-input") {
                if let Event::KeyboardInput { keycode, state, .. } = event {
                    println!(
                        "{:?}",
                        (
                            keycode,
                            state
                        )
                    );
                }
            }

            macro_rules! text_box_xy {
                () => {{
                    let view = &v_s!().view;
                    let xy = wimp_render::get_current_buffer_rect(
                        view.current_buffer_kind(),
                        view.menu_mode(),
                        v_s!().dimensions
                    )
                    .xy;

                    screen_to_text_box(v_s!().ui.mouse_pos, xy)
                }};
            }

            macro_rules! perform_command {
                ($key: expr) => {
                    if let Some(LabelledCommand{ label, command }) = r_c.commands.get($key) {
                        dbg!(label);
                        command(&mut r_s, &mut fns);
                    }
                }
            }

            macro_rules! load_file {
                ($path: expr) => {{
                    let unparsed_path = $path;
    
                    match canonicalize(&unparsed_path).and_then(|p| {
                        let last_path_tried = p.path.to_owned();
                        load_tab(p)
                            .map_err(|e|
                                edited_storage::CanonicalizeError::io(
                                    last_path_tried,
                                    e
                                )
                            )
                    }) {
                        Ok(LoadedTab{ name, data, position }) => {
                            let input = if let Some(position) = position {
                                Input::AddOrSelectBufferThenGoTo(name, data, position)
                            } else {
                                Input::AddOrSelectBuffer(name, data)
                            };
    
                            call_u_and_r!(input);
    
                            // Notify the user that the file loaded, if we are not
                            // already in focus.
                            use window_layer::UserAttentionType;
                            fns.request_user_attention(
                                Some(UserAttentionType::Informational)
                            );
                        }
                        Err(err) => {
                            handle_platform_error!(
                                r_s,
                                format!("{}\npath: {}", err, unparsed_path.to_string_lossy())
                            );
                        }
                    }
                }};
            }

            match event {
                Event::CloseRequested => quit!(),
                Event::ScaleFactorChanged(scale_factor) => {
                    set_current_hidpi_factor(
                        &mut r_s,
                        &mut fns,
                        scale_factor,
                    );
                }
                Event::Resized => {
                    v_s!().dimensions.window = fns.dimensions();

                    call_u_and_r!(
                        get_non_font_size_dependents_input!(
                            v_s!().view.menu_mode(),
                            v_s!().dimensions
                        )
                    );
                }
                Event::Focused(is_focused) => {
                    dbg!("set to ", is_focused);
                    v_s!().ui.window_is_focused = is_focused;
                    if is_focused {
                        // X11 requires us to explicitly unset the window
                        // attention.
                        fns.request_user_attention(
                            None
                        );
                    }
                }
                Event::ReceivedCharacter(mut c) => {
                    if c != '\u{1}'    // "start of heading" (sent with Ctrl-a)
                    && c != '\u{3}'    // "end of text" (sent with Ctrl-c)
                    && c != '\u{4}'    // "end of transmission" (sent with Ctrl-d)
                    && c != '\u{5}'    // "enquiry" (sent with Ctrl-e)
                    && c != '\u{6}'    // "acknowledge" (sent with Ctrl-f)
                    && c != '\u{7}'    // bell (sent with Ctrl-g)
                    && c != '\u{8}'    // backspace (sent with Ctrl-h)
                    && c != '\u{9}'    // horizontal tab (sent with Ctrl-i)
                    && c != '\u{c}'    // new page/form feed (sent with Ctrl-l)
                    && c != '\u{f}'    // "shift in" AKA use black ink apparently, (sent with Ctrl-o)
                    && c != '\u{10}'   // "data link escape" AKA interpret the following as raw data, (sent with Ctrl-p)
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
                            match v_s!().view.current_buffer_kind() {
                                None => {}
                                Text| FileSwitcher => {
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
                Event::KeyboardInput {
                    state: ElementState::Pressed,
                    keycode,
                    modifiers,
                    ..
                } => {
                    perform_command!(&(modifiers, keycode));
                }
                Event::MouseWheel {
                    delta: MouseScrollDelta::LineDelta(_, y),
                    modifiers,
                } if modifiers.is_empty() => {
                    let ui = &mut v_s!().ui;
                    let scroll_y = abs::Vector::from(y * wimp_render::SCROLL_MULTIPLIER);
                    if wimp_render::inside_tab_area(ui.mouse_pos, v_s!().dimensions.font) {
                        ui.tab_scroll -= scroll_y;
                    } else {
                        call_u_and_r!(Input::ScrollVertically(scroll_y));
                    }
                }
                Event::MouseWheel {
                    delta: MouseScrollDelta::LineDelta(_, y),
                    modifiers,
                } if modifiers == SHIFT => {
                    let ui = &mut v_s!().ui;
                    let scroll_y = abs::Vector::from(y * wimp_render::SCROLL_MULTIPLIER);
                    if wimp_render::inside_tab_area(ui.mouse_pos, v_s!().dimensions.font) {
                        ui.tab_scroll -= scroll_y;
                    } else {
                        call_u_and_r!(Input::ScrollHorizontally(scroll_y));
                    }
                }
                Event::CursorMoved {
                    position,
                    modifiers,
                } => {
                    let ui = &mut v_s!().ui;
                    ui.mouse_pos = position;

                    match modifiers {
                        m if m.is_empty() => {
                            let cursor_icon = if wimp_render::should_show_text_cursor(
                                ui.mouse_pos,
                                v_s!().view.menu_mode(),
                                v_s!().dimensions
                            ) {
                                window_layer::CursorIcon::Text
                            } else {
                                d!()
                            };

                            fns.set_cursor_icon(cursor_icon);

                            if ui.left_mouse_state.is_pressed() && !mouse_within_radius!() {
                                call_u_and_r!(Input::DragCursors(text_box_xy!()));
                            }
                        }
                        _ => {}
                    }
                }
                Event::MouseInput {
                    button: MouseButton::Left,
                    state: ElementState::Pressed,
                    modifiers,
                } // Allow things like Shift-Alt-Click as plain click, but disallow
                  // Super-Shift-Alt-Click.
                if (!modifiers).intersects(!CTRL) => {
                    v_s!().ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;

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
                },
                Event::MouseInput {
                    button: MouseButton::Left,
                    state: ElementState::Released,
                    ..
                } => {
                    let ui = &mut v_s!().ui;
                    ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;
                    last_click_x = ui.mouse_pos.x;
                    last_click_y = ui.mouse_pos.y;
                },
                Event::MainEventsCleared => {
                    perf_viz::start_record!("MainEventsCleared");
                    let index_state = v_s!().view.index_state();
                    let buffer_status_map = &mut v_s!().buffer_status_map;

                    for _ in 0..EVENTS_PER_FRAME {
                        match editor_out_source.try_recv() {
                            Ok(editor_thread::Output::Rendered((v, c1, c2, result))) => {
                                debug_assert!(result.is_ok());
                                v_s!().debug_menu_state.editor_buffers_size_in_bytes
                                    = v.stats.editor_buffers_size_in_bytes;

                                v_s!().view.update(v, result.map(|b| b.data).unwrap_or_default());
                                for (i, e_t) in v_s!().view.edited_transitions() {
                                    transform_at(
                                        buffer_status_map,
                                        index_state,
                                        i,
                                        BufferStatusTransition::from(e_t)
                                    );
                                }

                                if c1 != Cmd::None {
                                    r_s.cmds.push_back(c1);
                                }
                                if c2 != Cmd::None {
                                    r_s.cmds.push_back(c2);
                                }
                            }
                            Ok(editor_thread::Output::Pid(pid)) => {
                                v_s!().debug_menu_state.pids.editor = pid;
                            }
                            Err(_) => break,
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
                    fns.request_redraw();
                    perf_viz::end_record!("MainEventsCleared");
                }
                Event::Init => {
                    // At least try to measure the first frame accurately
                    perf_viz::start_record!("main loop");
                    dt = fns.loop_start();
                }
                Event::RedrawRequested => {
                    perf_viz::start_record!("frame");

                    let ViewOutput { text_or_rects, action } =
                        wimp_render::view(
                            &mut v_s!(),
                            &r_c,
                            dt,
                        );

                    // TODO Stop allocating new text_or_rects every frame.
                    fns.render(&text_or_rects)
                        .expect("fns didn't work");

                    perf_viz::start_record!("r_s.cmds");
                    for _ in 0..EVENTS_PER_FRAME {
                        if let Some(cmd) = r_s.cmds.pop_front() {
                            match cmd {
                                Cmd::SetClipboard(s) => {
                                    if let Err(err) = r_s.clipboard.set(s) {
                                        handle_platform_error!(r_s, err);
                                    }
                                }
                                Cmd::LoadFile(path) => load_file!(path),
                                Cmd::MakeActiveTabVisible => {
                                    wimp_render::make_active_tab_visible(
                                        &mut v_s!(r_s).ui,
                                        &v_s!(r_s).view,
                                        v_s!(r_s).dimensions
                                    );
                                },
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
                    if let Some(render_rate) = fns.report_rate() {
                        macro_rules! ms_from_span {
                            ($span: expr) => {
                                $span
                                .duration_or_default().as_micros() as f32 / 1000.0
                            }
                        }
                        let wimp_stats = v_s!().stats;

                        let view_function_ms = ms_from_span!(
                            wimp_stats.latest_view_function_time_span
                        );

                        let editor_stats = v_s!().view.stats();
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

                        let editor_status_line_ms = ms_from_span!(
                            editor_stats.latest_status_line_time_span
                        );

                        let editor_menu_render_ms = ms_from_span!(
                            editor_stats.latest_menu_render_time_span
                        );

                        let parse_total = format!(
                            "{: >6.3} ms",
                            {
                                let mut total = 0.0;

                                // For this display, we want some fewer decimal 
                                // places, and so are fine with some precision loss.
                                #[allow(clippy::cast_precision_loss)]

                                for span in &editor_stats.latest_parse_time_spans {
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

                        fns.set_title(&format!(
                            "{}{} {:.0} FPS v{: >6.3} ms e{: >6.3} ms(e-u{: >6.3} ms e-r{: >6.3} ms(e-br{: >6.3} ms p{}) e-st{: >6.3} ms e-mr{: >6.3} ms) {:?} click {:?}",
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
                            editor_status_line_ms,
                            editor_menu_render_ms,
                            (v_s!().ui.mouse_pos.x, v_s!().ui.mouse_pos.y),
                            (last_click_x, last_click_y),
                        ));
                    }
                    perf_viz::end_record!("report_rate");

                    perf_viz::end_record!("frame");

                    fns.loop_sleep();

                    perf_viz::end_record!("main loop");
                    perf_viz::start_record!("main loop");

                    // We want to track the time that the message loop takes too!
                    dt = fns.loop_start();
                }
                Event::UserEvent(e) => match e {
                    CustomEvent::Pid(kind, pid) => {
                        match kind {
                            PidKind::PathMailbox => {
                                v_s!().debug_menu_state.pids.path_mailbox = pid;
                            }
                        }
                    },
                    CustomEvent::OpenFile(p) => load_file!(p),
                    CustomEvent::SaveNewFile(p, index) => {
                        // TODO Stop receiving this `p` and `index` here and
                        // forwarding to the next thread, and instead send it
                        // there directly

                        let r_s = &mut r_s;
                        // The fact we need to store the index and retreive it later,
                        // potentially across multiple updates, is why this thread
                        // needs to know about the generational indices.
                        if let Some(label) = v_s!(r_s).view.get_buffer_label(index)
                        {
                            save_to_disk!(
                                r_s,
                                p,
                                label.clone(),
                                index
                            );
                        }
                    }
                    CustomEvent::MarkBufferStatusTransition(
                        path,
                        index,
                        transition,
                    ) => {
                        transform_at(
                            &mut v_s!().buffer_status_map,
                            // TODO should this be the index state before the save?
                            v_s!().view.index_state(),
                            index,
                            transition
                        );
                        call_u_and_r!(
                            r_s,
                            Input::SavedAs(index, path)
                        );
                    },
                    CustomEvent::SendBuffersToBeSaved => {
                        // TODO Can we send this directly from the edited files
                        // thread to the editor thread, without passing through here?
                        let view = &v_s!().view;
                        let index_state = view.index_state();
                        let buffer_status_map = &mut v_s!().buffer_status_map;

                        let mut names = Vec::with_capacity(32);
                        let mut statuses = Vec::with_capacity(32);

                        for (i, label) in view.buffers.buffer_iter() {
                            names.push(label.name.clone());
                            statuses.push(
                                buffer_status_map
                                    .get(index_state, i)
                                    .copied()
                                    .unwrap_or_default()
                            );
                        }

                        let _hope_it_gets_there = editor_in_sink.send(
                            EditorThreadInput::SaveBuffers(
                                index_state,
                                names,
                                statuses,
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
                _ => {}
            }
        });
    }
}