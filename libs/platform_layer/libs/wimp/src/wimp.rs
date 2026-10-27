// This was originally based on example code for https://github.com/alexheretic/glyph-brush
// the code is licensed under the Apache 2.0 license, as described in the license file in the
// opengl folder, to the extent that the code remains as it was
// (at commit 90e7c7c331e9f991e11de6404b2ca073c0a09e61)

#![deny(unused)]

use std::{
    collections::VecDeque,
    path::{PathBuf},
    time::Duration,
};
use wimp_render::{get_find_replace_info, FindReplaceInfo, get_go_to_position_info, GoToPositionInfo, ViewOutput, ViewAction};
use wimp_types::{ui, ui::{PhysicalButtonState, Navigation}, transform_at, BufferStatus, get_clipboard, Dimensions, LabelledCommand, RunConsts, RunState, Pids, EditorThreadInput, EditedFilesThread, ViewRunState, DebugMenuState, DpiFactor};
use macros::{d, dbg};
use platform_types::{screen_positioning::screen_to_text_box, *};
use platform_layer_types::{BufferStatusTransition, CustomEvent, EventSender, PidKind};
use shared::{Res, TITLE};
use edited_storage::{canonicalize, load_tab, load_previous_tabs, LoadedTab};
use wimp_types::StartConfig;

/// # Errors
/// Returns an error if there is an error before the eventloop has started.
#[perf_viz::record]
pub fn run(
    editor_api: EditorAPI,
    start_config: StartConfig,
) -> Res<()> {
    let running_lock_path = start_config.running_lock_path;
    let edited_files_index_path_buf = start_config.edited_files_index_path_buf;
    let edited_files_dir_buf = start_config.edited_files_dir_buf;
    let path_mailbox_path = start_config.path_mailbox_path;
    let hidpi_factor_override = start_config.hidpi_factor_override;
    let extra_paths = start_config.extra_paths;
    let startup_description = start_config.startup_description;

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

    use window_layer_types::{InitOutput, ScaleFactor};
    const HIDPI_INCREMENT_BY: ScaleFactor = 1./12.;
    const HIDPI_MINIMUM: ScaleFactor = HIDPI_INCREMENT_BY;
    const HIDPI_DEFAULT: ScaleFactor = 1.;

    let InitOutput {
        state: window_state,
        event_proxy,
        char_dims: initial_char_dims,
        dimensions: initial_dimensions,
    } = match window_layer::init(
            hidpi_factor_override.unwrap_or(HIDPI_DEFAULT),
            wimp_render::TEXT_BACKGROUND_COLOUR,
            TITLE.into(),
            &wimp_render::TEXT_SIZES,
        ) {
            Ok(s) => s,
            Err(e) => {
                on_shutdown(running_lock_path);
                return Err(e)
            },
        };

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

    use std::sync::mpsc::{channel};

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
                                            proxy.send(CustomEvent::EditedBufferError(
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

                    proxy.send(CustomEvent::SendBuffersToBeSaved);

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
        let char_dims = initial_char_dims;

        let current_hidpi_factor = HIDPI_DEFAULT;
        let resolved_hidpi_factor = hidpi_factor_override.unwrap_or(current_hidpi_factor);

        let dimensions = Dimensions {
            font: wimp_render::get_font_info(&char_dims),
            window: initial_dimensions,
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

    let mut dt = <_>::default();

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

        use window_layer_types::{ModifiersState, KeyCode, MouseButton};

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

            let char_dims = fns.set_scale_factor(
                hidpi_factor as _,
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
                    $fns: &mut window_layer::Fns,
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
            ($r_s: ident, $func: ident, $default_path: expr, $path: ident in $event: expr) => ({
                #[cfg(not(feature = "native-file-chooser"))]
                {
                    handle_platform_error!(
                        $r_s,
                        format!(
                            "This version of {TITLE} was built without the native file dialog.\nConsider using the --file command instead."
                        )
                    );
                }

                #[cfg(feature = "native-file-chooser")]
                {
                    let proxy =
                        std::sync::Arc::new(std::sync::Mutex::new($r_s.event_proxy.clone()));
                    let proxy = proxy.clone();
                    file_chooser::$func(
                        $default_path,
                        move |$path: PathBuf| {
                            proxy
                                .lock()
                                .expect("file_chooser thread private mutex locked!?")
                                .send($event);
                        }
                    );
                }
            });
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
            [empty, F2, "Auto-Indent Selection", state {
                call_u_and_r!(state, Input::AutoIndentSelection);
            }]
            [empty, F3, "Toggle Case", state {
                call_u_and_r!(state, Input::ToggleCase);
            }]
            [CTRL, F4, "Close tab.", r_s {
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
                    r_s,
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
                            r_s,
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
                #[cfg(feature = "native-file-chooser")]
                let i = v_s!(r_s).view.current_text_index();
                file_chooser_call!(
                    r_s,
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

        window_layer::run(window_state, move |event, mut fns| {
            use window_layer_types::{
                Event,
                ButtonState,
                KeyboardInput,
                MouseWheel,
                MouseInput,
                CursorMoved,
            };

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
                if let Event::KeyboardInput(KeyboardInput{ keycode, state, .. }) = event {
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
                            use window_layer_types::AttentionMode;
                            fns.request_attention_mode(
                                AttentionMode::Urgent,
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
                Event::Resized(dimensions) => {
                    v_s!().dimensions.window = dimensions;

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
                Event::KeyboardInput(KeyboardInput {
                    state: ButtonState::Pressed,
                    keycode,
                    modifiers,
                    ..
                }) => {
                    perform_command!(&(modifiers, keycode));
                }
                Event::KeyboardInput(KeyboardInput {
                    state: ButtonState::Released,
                    ..
                }) => {}
                Event::MouseWheel(MouseWheel{
                    delta: y,
                    modifiers,
                }) if modifiers.is_empty() => {
                    let ui = &mut v_s!().ui;
                    let scroll_y = abs::Vector::from(y * wimp_render::SCROLL_MULTIPLIER);
                    if wimp_render::inside_tab_area(ui.mouse_pos, v_s!().dimensions.font) {
                        ui.tab_scroll -= scroll_y;
                    } else {
                        call_u_and_r!(Input::ScrollVertically(scroll_y));
                    }
                }
                Event::MouseWheel(MouseWheel{
                    delta: y,
                    modifiers,
                }) if modifiers == SHIFT => {
                    let ui = &mut v_s!().ui;
                    let scroll_y = abs::Vector::from(y * wimp_render::SCROLL_MULTIPLIER);
                    if wimp_render::inside_tab_area(ui.mouse_pos, v_s!().dimensions.font) {
                        ui.tab_scroll -= scroll_y;
                    } else {
                        call_u_and_r!(Input::ScrollHorizontally(scroll_y));
                    }
                }
                Event::MouseWheel(_) => {}
                Event::CursorMoved(CursorMoved{
                    position,
                    modifiers: m,
                }) if m.is_empty() => {
                    let ui = &mut v_s!().ui;
                    ui.mouse_pos = position;

                    let cursor_icon = if wimp_render::should_show_text_cursor(
                        ui.mouse_pos,
                        v_s!().view.menu_mode(),
                        v_s!().dimensions
                    ) {
                        window_layer_types::CursorIcon::Text
                    } else {
                        d!()
                    };

                    fns.set_cursor_icon(cursor_icon);

                    if ui.left_mouse_state.is_pressed() && !mouse_within_radius!() {
                        call_u_and_r!(Input::DragCursors(text_box_xy!()));
                    }
                }
                Event::CursorMoved(_) => {}
                Event::MouseInput(MouseInput {
                    button: MouseButton::Left,
                    state: ButtonState::Pressed,
                    modifiers,
                })  // Allow things like Shift-Alt-Click as plain click, but disallow
                    // Super-Shift-Alt-Click.
                if (!modifiers).intersects(!CTRL) => {
                    v_s!().ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;

                    let replace_or_add = if modifiers.intersects(CTRL) {
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
                Event::MouseInput(MouseInput {
                    button: MouseButton::Left,
                    state: ButtonState::Released,
                    ..
                }) => {
                    let ui = &mut v_s!().ui;
                    ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;
                    last_click_x = ui.mouse_pos.x;
                    last_click_y = ui.mouse_pos.y;
                },
                Event::MouseInput(_) => {},
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

                    window_layer::render(&mut fns, &text_or_rects)
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
                            TITLE,
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
            }
        });
    }
}