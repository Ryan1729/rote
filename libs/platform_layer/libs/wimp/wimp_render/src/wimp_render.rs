#![deny(bindings_with_variant_name, unused)]
use gl_layer::{ColouredText, MulticolourTextSpec, TextLayout, TextOrRect, TextSpec, VisualSpec};
use wimp_types::{CommandsMap, LocalMenuView, View, WimpMenuMode, MenuView, WimpMenuView, FindReplaceMode, ui_id, ui, ui::{ButtonState}, BufferStatus, CommandKey, Dimensions, RunConsts, ViewRunState, ui::{ListSelection, ListSelectionWindowSize}, command_keys};
use macros::{c, d, dbg, invariant_assert, u};
use platform_types::{
    *,
    screen_positioning::*,
    g_i,
    GoToPositionView,
    FindReplaceView,
    FileSwitcherView,
    BufferViewData,
    BufferIdKind,
    BufferId,
    b_id,
    CursorState,
    Highlight,
    HighlightKind,
    tbxy,
    tbxywh,
    Input,
    sswh,
    ssr,
    Spans,
};


type Colour = [f32; 4];

const COLOUR_DELTA: f32 = 1.0 / 128.0;

// TODO clamp `lighten` and `darken`?
macro_rules! lighten {
    ($colour: expr) => {{
        let colour = $colour;
        [
            colour[0] + COLOUR_DELTA,
            colour[1] + COLOUR_DELTA,
            colour[2] + COLOUR_DELTA,
            colour[3],
        ]
    }};
}
macro_rules! darken {
    ($colour: expr) => {{
        let colour = $colour;
        [
            colour[0] - COLOUR_DELTA,
            colour[1] - COLOUR_DELTA,
            colour[2] - COLOUR_DELTA,
            colour[3],
        ]
    }};
}

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

macro_rules! palette {
  (black $($tokens:tt)*) => {   c![0x22 as f32 / 255.0, 0x22 as f32 / 255.0, 0x22 as f32 / 255.0 $($tokens)*] };
  (red $($tokens:tt)*) => {     c![0xde as f32 / 255.0, 0x49 as f32 / 255.0, 0x49 as f32 / 255.0 $($tokens)*] };
  (green $($tokens:tt)*) => {   c![0x30 as f32 / 255.0, 0xb0 as f32 / 255.0, 0x6e as f32 / 255.0 $($tokens)*] };
  (yellow $($tokens:tt)*) => {  c![0xff as f32 / 255.0, 0xb9 as f32 / 255.0, 0x37 as f32 / 255.0 $($tokens)*] };
  (blue $($tokens:tt)*) => {    c![0x33 as f32 / 255.0, 0x52 as f32 / 255.0, 0xe1 as f32 / 255.0 $($tokens)*] };
  (magenta $($tokens:tt)*) => { c![0x53 as f32 / 255.0, 0x33 as f32 / 255.0, 0x54 as f32 / 255.0 $($tokens)*] };
  (cyan $($tokens:tt)*) => {    c![0x5a as f32 / 255.0, 0x7d as f32 / 255.0, 0x8b as f32 / 255.0 $($tokens)*] };
  (white $($tokens:tt)*) => {   c![0xee as f32 / 255.0, 0xee as f32 / 255.0, 0xee as f32 / 255.0 $($tokens)*] };

  (alt black $($tokens:tt)*) => {   c![0x66 as f32 / 255.0, 0x66 as f32 / 255.0, 0x66 as f32 / 255.0 $($tokens)*] };
  (alt red $($tokens:tt)*) => {     c![0x49 as f32 / 255.0, 0x0b as f32 / 255.0, 0x0b as f32 / 255.0 $($tokens)*] };
  (alt green $($tokens:tt)*) => {   c![0x16 as f32 / 255.0, 0x4f as f32 / 255.0, 0x31 as f32 / 255.0 $($tokens)*] };
  (alt yellow $($tokens:tt)*) => {  c![0xff as f32 / 255.0, 0xff as f32 / 255.0, 0x00 as f32 / 255.0 $($tokens)*] };
  (alt blue $($tokens:tt)*) => {    c![0x00 as f32 / 255.0, 0x37 as f32 / 255.0, 0xff as f32 / 255.0 $($tokens)*] };
  (alt magenta $($tokens:tt)*) => { c![0xa9 as f32 / 255.0, 0x68 as f32 / 255.0, 0xab as f32 / 255.0 $($tokens)*] };
  (alt cyan $($tokens:tt)*) => {    c![0x48 as f32 / 255.0, 0x91 as f32 / 255.0, 0xae as f32 / 255.0 $($tokens)*] };
  (alt white $($tokens:tt)*) => {   c![0xff as f32 / 255.0, 0xff as f32 / 255.0, 0xff as f32 / 255.0 $($tokens)*] };
}

pub const TEXT_BACKGROUND_COLOUR: Colour = palette![black];
pub const TEXT_HOVER_BACKGROUND_COLOUR: Colour = lighten!(palette![black]);
pub const TEXT_PRESSED_BACKGROUND_COLOUR: Colour = darken!(palette![black]);

const HIGHLIGHT_ALPHA: f32 = 0.6;
const USER_HIGHLIGHT_COLOUR: Colour = palette![alt black, HIGHLIGHT_ALPHA];
const RESULT_HIGHLIGHT_COLOUR: Colour = palette![yellow, HIGHLIGHT_ALPHA];
const CURRENT_RESULT_HIGHLIGHT_COLOUR: Colour = palette![green, HIGHLIGHT_ALPHA];

fn highlight_kind_colour(kind: HighlightKind) -> Colour {
    u!{HighlightKind}
    match kind {
        User => USER_HIGHLIGHT_COLOUR,
        Result => RESULT_HIGHLIGHT_COLOUR,
        CurrentResult => CURRENT_RESULT_HIGHLIGHT_COLOUR,
    }
}

const CHROME_BACKGROUND_COLOUR: Colour = palette![alt green];
const CHROME_TEXT_COLOUR: Colour = palette![alt cyan];
const TAB_BAR_BACKGROUND_COLOUR: Colour = palette![alt cyan];
const TAB_BACKGROUND_COLOUR: Colour = palette![cyan];
const TAB_TEXT_COLOUR: Colour = palette![white];

#[derive(Clone, Debug, PartialEq)]
pub enum ViewAction {
    None,
    Input(Input),
    Command(CommandKey),
}
d!{for ViewAction: ViewAction::None}

impl ViewAction {
    pub fn or(self, action: ViewAction) -> ViewAction {
        match self {
            ViewAction::None => action,
            _ => self,
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, ViewAction::None)
    }
}

fn into_action(opt: Option<Input>) -> ViewAction {
    opt.into()
}

impl From<Option<Input>> for ViewAction {
    fn from(op: Option<Input>) -> Self {
        u!{ViewAction}
        match op {
            Some(input) => Input(input),
            Option::None => None
        }
    }
}

impl From<Option<CommandKey>> for ViewAction {
    fn from(op: Option<CommandKey>) -> Self {
        u!{ViewAction}
        match op {
            Some(key) => Command(key),
            Option::None => None
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct ViewOutput<'view> {
    pub text_or_rects: Vec<TextOrRect<'view>>,
    pub action: ViewAction,
}

#[perf_viz::record]
pub fn view<'view>(
    ViewRunState {
        ref mut ui,
        ref mut view,
        ref mut buffer_status_map,
        dimensions,
        ref startup_description,
        ref pids,
        ref mut pid_string,
        ref mut stats,
        ..
    }: &'view mut ViewRunState,
    RunConsts {
        commands
    }: &RunConsts,
    dt: std::time::Duration,
) -> ViewOutput<'view> {
    *stats = d!();
    ui.frame_init(&view);

    stats.latest_view_function_time_span = TimeSpan::start();
    if cfg!(feature = "extra-prints") {
        if_changed::dbg!(&view);
    }

    let dimensions = *dimensions;
    let sswh!(width, height) = dimensions.window;
    let FontInfo {
        ref text_char_dim,
        ref status_char_dim,
        ref tab_char_dim,
        ref find_replace_char_dim,
        ..
    } = dimensions.font;

    const PER_BUFFER_TEXT_OR_RECT_ESTIMATE: usize =
        4   // 2-4 per tab, usually 2
    ;
    ui.add_dt(dt);

    let buffer_count: usize = view.buffers_count().into();

    let mut text_or_rects = Vec::with_capacity(buffer_count * PER_BUFFER_TEXT_OR_RECT_ESTIMATE);

    let mut action = ViewAction::None;

    let UpperPositionInfo {
        edit_y,
        ..
    } = upper_position_info(&tab_char_dim);

    perf_viz::start_record!("fill text_or_rects");

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: ssr!(_, ssxy!(width, edit_y)),
        colour: TAB_BAR_BACKGROUND_COLOUR,
        z: TAB_BACKGROUND_Z,
    }));

    //
    // Tabs
    //
    perf_viz::start_record!("render Tabs");
    let selected_index = view.current_text_index();

    let tab_count = buffer_count;
    for (i, (index, label))
    in view.buffers.buffer_iter().enumerate() {
        let SpacedRect {
            padding,
            margin,
            rect,
        } = get_tab_spaced_rect(&ui, *tab_char_dim, i, tab_count, width);
        if rect.min.x > width {
            break;
        }

        if do_outline_button(
            ui,
            ui_id!(i),
            &mut text_or_rects,
            OutlineButtonSpec {
                text: &label.name_string,
                size: TAB_SIZE,
                char_dim: *tab_char_dim,
                layout: TextLayout::Unbounded,
                margin,
                rect,
                underline: if index == selected_index {
                    Some(LineSpec {
                        colour: palette![blue],
                        thickness: padding.into_ltrb().b,
                    })
                } else {
                    None
                },
                overline: match buffer_status_map
                    .get(
                        view.index_state(),
                        index,
                    )
                    .cloned()
                    .unwrap_or_default()
                {
                    BufferStatus::Unedited => None,
                    BufferStatus::EditedAndUnSaved => Some(LineSpec {
                        colour: palette![red],
                        thickness: padding.into_ltrb().t,
                    }),
                    BufferStatus::EditedAndSaved => Some(LineSpec {
                        colour: palette![alt black],
                        thickness: padding.into_ltrb().t,
                    }),
                },
                z: TAB_Z,
            },
        ) {
            action = ViewAction::Input(Input::SelectBuffer(b_id!(
                BufferIdKind::Text,
                index
            )))
        }
    }
    perf_viz::end_record!("render Tabs");

    perf_viz::start_record!("render BufferIdKind::Text");
    let (index, _label) = view.current_text_index_and_buffer_label();
    let data: &BufferViewData = &view.scratch.buffer_view_data;

    // let text = {
    //     chars
    //     // perf_viz::record_guard!("map unprinatbles to symbols for themselves");
    //     // let s = chars
    //     //     .chars()
    //     //     .map(|c| {
    //     //         // map unprinatbles to symbols for themselves
    //     //         if c < 0x20 as char {
    //     //             std::char::from_u32(c as u32 | 0x2400u32).unwrap_or(c)
    //     //         } else {
    //     //             c
    //     //         }
    //     //     })
    //     //     .collect::<String>();
    //     // s
    // };
    let edit_buffer_text_rect = get_edit_buffer_xywh(view.menu_mode(), dimensions);

    let edit_buffer_text_rect: ScreenSpaceRect = edit_buffer_text_rect.into();

    action = into_action(text_box(
        ui,
        &mut text_or_rects,
        edit_buffer_text_rect,
        d!(),
        *text_char_dim,
        TEXT_SIZE,
        TextBoxColour::FromSpans,
        data,
        b_id!(BufferIdKind::Text, index),
        EDIT_Z,
        view.current_buffer_id(),
    ))
    .or(action);
    perf_viz::end_record!("render BufferIdKind::Text");

    perf_viz::start_record!("render view.menu()");
    match view.menu() {
        WimpMenuView { local_menu: &None, platform_menu } => {
            match platform_menu {
                MenuView::None => {}
                MenuView::FindReplace(FindReplaceView {
                    mode,
                    find,
                    replace,
                    result_count,
                }) => {
                    //
                    //    Find/Replace
                    //
                    let FindReplaceInfo {
                        padding,
                        top_y,
                        bottom_y,
                        label_rect,
                        find_outer_rect,
                        replace_outer_rect,
                        ..
                    } = get_find_replace_info(dimensions);
                    match mode.into() {
                        FindReplaceMode::CurrentFile => {
                            let outer_rect = get_full_width_ssr(top_y, width, bottom_y);
                            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                                rect: outer_rect,
                                colour: CHROME_BACKGROUND_COLOUR,
                                z: FIND_REPLACE_BACKGROUND_Z,
                            }));
                            text_or_rects.push(TextOrRect::Text(TextSpec {
                                text: if find.chars.is_empty() {
                                    "In current file"
                                } else {
                                    // cheap hack to avoid lifetime issues
                                    match result_count {
                                        0 => "In current file (0 results)",
                                        1 => "In current file (1 result)",
                                        2 => "In current file (2 results)",
                                        3 => "In current file (3 results)",
                                        4 => "In current file (4 results)",
                                        5 => "In current file (5 results)",
                                        6 => "In current file (6 results)",
                                        7 => "In current file (7 results)",
                                        8 => "In current file (8 results)",
                                        _ => "In current file (9+ results)",
                                    }
                                },
                                size: FIND_REPLACE_SIZE,
                                layout: TextLayout::Unbounded,
                                spec: VisualSpec {
                                    rect: label_rect,
                                    colour: CHROME_TEXT_COLOUR,
                                    z: FIND_REPLACE_Z,
                                },
                            }));
                            macro_rules! spaced_input_box {
                                ($data: expr, $input: expr, $outer_rect: expr) => {{
                                    action = into_action(text_box(
                                        ui,
                                        &mut text_or_rects,
                                        $outer_rect,
                                        padding,
                                        *find_replace_char_dim,
                                        FIND_REPLACE_SIZE,
                                        TextBoxColour::Single(CHROME_TEXT_COLOUR),
                                        $data,
                                        $input,
                                        FIND_REPLACE_Z,
                                        view.current_buffer_id(),
                                    ))
                                    .or(action);
                                }};
                            }

                            spaced_input_box!(find, b_id!(BufferIdKind::Find, index), find_outer_rect);
                            spaced_input_box!(
                                replace,
                                b_id!(BufferIdKind::Replace, index),
                                replace_outer_rect
                            );
                        }
                    }
                }
                MenuView::FileSwitcher(ref fs_view) => {
                    render_file_switcher_menu(
                        index,
                        fs_view,
                        ui,
                        view.current_buffer_id(),
                        dimensions,
                        &mut text_or_rects,
                        &mut action,
                    );

                    if action.is_none() {
                        if cfg!(feature="extra-prints") {
                            print!(".");
                        }
                    } else {
                        dbg!(&action);
                    }
                }
                MenuView::GoToPosition(GoToPositionView {
                    ref go_to_position,
                }) => {
                    let GoToPositionInfo {
                        padding,
                        top_y,
                        bottom_y,
                        label_rect,
                        input_outer_rect,
                        ..
                    } = get_go_to_position_info(dimensions);
                    let outer_rect = get_full_width_ssr(top_y, width, bottom_y);
                    text_or_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: outer_rect,
                        colour: CHROME_BACKGROUND_COLOUR,
                        z: FIND_REPLACE_BACKGROUND_Z,
                    }));

                    text_or_rects.push(TextOrRect::Text(TextSpec {
                        text: "Go to position",
                        size: FIND_REPLACE_SIZE,
                        layout: TextLayout::Unbounded,
                        spec: VisualSpec {
                            rect: label_rect,
                            colour: CHROME_TEXT_COLOUR,
                            z: FIND_REPLACE_Z,
                        },
                    }));

                    action = into_action(text_box(
                        ui,
                        &mut text_or_rects,
                        input_outer_rect,
                        padding,
                        *find_replace_char_dim,
                        FIND_REPLACE_SIZE,
                        TextBoxColour::Single(CHROME_TEXT_COLOUR),
                        go_to_position,
                        b_id!(BufferIdKind::GoToPosition, index),
                        FIND_REPLACE_Z,
                        view.current_buffer_id(),
                    )).or(action);
                }
            }
        }
        WimpMenuView { local_menu: Some(local_menu), .. } => {
            match local_menu {
                LocalMenuView::Command => {
                    let CommandMenuInfo {
                        list_margin,
                        first_button_rect,
                        outer_rect,
                        ..
                    } = get_command_menu_info(dimensions);
                    text_or_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: outer_rect,
                        colour: CHROME_BACKGROUND_COLOUR,
                        z: FIND_REPLACE_BACKGROUND_Z,
                    }));

                    let text_or_rects = &mut text_or_rects;

                    let mut current_rect = first_button_rect;
                    let vertical_shift = first_button_rect.height()
                        + list_margin.into_ltrb().b;

                    let mut navigated_result = None;
                    let results: Vec<CommandKey> = commands.keys().cloned().collect();

                    if action.is_none() {
                        u!{ui::Navigation}

                        match ui.navigation {
                            None => {}
                            Up => {
                                ui.file_switcher_pos.index = if ui.file_switcher_pos.index == 0 {
                                    results.len().clone().saturating_sub(1)
                                } else {
                                    ui.file_switcher_pos.index - 1
                                };
                            }
                            Down => {
                                ui.file_switcher_pos.index = (ui.file_switcher_pos.index + 1) % results.len();
                            }
                            Interact => {
                                action = results
                                    .get(ui.file_switcher_pos.index)
                                    .cloned()
                                    .into();
                            }
                        }

                        navigated_result = Some(ui.file_switcher_pos.index);
                    }

                    for (result_index, result) in results.iter().enumerate() {
                        let result_id = ui_id!(result_index);

                        match navigated_result {
                            Some(i) if i == result_index => {
                                dbg!(&mut ui.keyboard).set_next_hot(result_id);
                            }
                            _ => {}
                        };

                        command_button(
                            ui,
                            ui_id!(format!("{:p}", result)),
                            text_or_rects,
                            current_rect,
                            dimensions,
                            &commands,
                            result,
                            &mut action,
                        );

                        current_rect.min.y += vertical_shift;
                        current_rect.max.y += vertical_shift;
                    }
                }
                LocalMenuView::Debug => {
                    let DebugMenuInfo {
                        outer_rect,
                        first_button_rect,
                        list_margin,
                        ..
                    } = get_debug_menu_info(dimensions);
                    text_or_rects.push(TextOrRect::Rect(VisualSpec {
                        rect: outer_rect,
                        colour: CHROME_BACKGROUND_COLOUR,
                        z: FIND_REPLACE_BACKGROUND_Z,
                    }));

                    // TODO render a bar chart of the last N view renders,
                    // where the x axis is  the Input variant, and the y axis is
                    // duration statisics like maximum, mean, median and mode.

                    let vertical_shift = first_button_rect.height()
                            + list_margin.into_ltrb().b;

                    let mut y = first_button_rect.min.y;
                    command_button(
                        ui,
                        ui_id!(),
                        &mut text_or_rects,
                        first_button_rect,
                        dimensions,
                        &commands,
                        &command_keys::add_run_state_snapshot(),
                        &mut action,
                    );

                    y += vertical_shift;

                    let line_shift: abs::Length = vertical_shift.halve();

                    let mut pid_bottom_y = y;

                    pid_string.clear();

                    macro_rules! push_pid_line {
                        ($field_name: ident) => {{
                            pid_bottom_y += line_shift;

                            let field_name = stringify!($field_name);
                            for _ in 0..(16usize.saturating_sub(field_name.len())) {
                                pid_string.push(' ');
                            }

                            pid_string.push_str(field_name);
                            pid_string.push_str(" PID: ");
                            pid_string.push_str(&format!("{}", pids.$field_name));
                            pid_string.push('\n');
                        }}
                    }

                    push_pid_line!(window);
                    push_pid_line!(editor);
                    push_pid_line!(path_mailbox);

                    text_or_rects.push(TextOrRect::Text(
                        TextSpec {
                            text: pid_string,
                            size: FIND_REPLACE_SIZE,
                            layout: TextLayout::Unbounded,
                            spec: VisualSpec {
                                rect: ssr!(
                                    first_button_rect.min.x, y,
                                    first_button_rect.max.x, pid_bottom_y
                                ),
                                colour: CHROME_TEXT_COLOUR,
                                z: FIND_REPLACE_BACKGROUND_Z,
                            }
                        }
                    ));

                    y = pid_bottom_y;

                    text_or_rects.push(TextOrRect::Text(TextSpec {
                        text: startup_description,
                        size: FIND_REPLACE_SIZE,
                        layout: TextLayout::Unbounded,
                        spec: VisualSpec {
                            rect: ssr!(
                                first_button_rect.min.x, y,
                                first_button_rect.max.x, y + vertical_shift,
                            ),
                            colour: CHROME_TEXT_COLOUR,
                            z: FIND_REPLACE_BACKGROUND_Z,
                        }
                    }));
                }
            }
        }
    }
    perf_viz::end_record!("render view.menu()");

    //
    //    Status line
    //

    perf_viz::start_record!("Status line");
    let status_line_y = get_status_line_y(*status_char_dim, height);

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: get_full_width_ssr(
            status_line_y,
            width,
            status_line_y + SEPARATOR_LINE_THICKNESS
        ),
        colour: TAB_BAR_BACKGROUND_COLOUR,
        z: STATUS_BACKGROUND_Z,
    }));

    let rect = get_full_width_ssr(
        status_line_y + SEPARATOR_LINE_THICKNESS,
        width,
        height
    );

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect,
        colour: CHROME_BACKGROUND_COLOUR,
        z: STATUS_BACKGROUND_Z,
    }));

    text_or_rects.push(TextOrRect::Text(TextSpec {
        text: &view.status_line().chars,
        size: STATUS_SIZE,
        layout: TextLayout::Unbounded,
        spec: VisualSpec {
            rect: rect.with_min_y(status_line_y + abs::Ratio::TWO * SEPARATOR_LINE_THICKNESS),
            colour: CHROME_TEXT_COLOUR,
            z: STATUS_Z,
        },
    }));

    let far_right_button_rect = ssr!(
        rect.max.x - (abs::Length::from(status_char_dim.w.get()) + SEPARATOR_LINE_THICKNESS),
        status_line_y + SEPARATOR_LINE_THICKNESS,
        rect.max.x - SEPARATOR_LINE_THICKNESS,
        rect.max.y - SEPARATOR_LINE_THICKNESS
    );

    let second_button_min_x = far_right_button_rect.min.x
        - (
            abs::Length::from(status_char_dim.w.get())
            + abs::Ratio::TWO
            * SEPARATOR_LINE_THICKNESS
        );

    let second_button_rect = far_right_button_rect
    .with_min_x(second_button_min_x)
    .with_max_x(
        second_button_min_x
        + (far_right_button_rect.width())
    );

    if do_outline_button(
        ui,
        ui_id!(),
        &mut text_or_rects,
        OutlineButtonSpec {
            text: "?", // 0x3F
            size: STATUS_SIZE,
            char_dim: *status_char_dim,
            layout: TextLayout::Unbounded,
            margin: Spacing::All(SEPARATOR_LINE_THICKNESS),
            rect: second_button_rect,
            z: STATUS_Z,
            ..d!()
        },
    ) {
        action = Some(command_keys::debug_menu()).into()
    }


    if do_outline_button(
        ui,
        ui_id!(),
        &mut text_or_rects,
        OutlineButtonSpec {
            text: "≡", // U+2261
            size: STATUS_SIZE,
            char_dim: *status_char_dim,
            layout: TextLayout::Unbounded,
            margin: Spacing::All(SEPARATOR_LINE_THICKNESS),
            rect: far_right_button_rect,
            z: STATUS_Z,
            ..d!()
        },
    ) {
        action = Some(command_keys::command_menu()).into()
    }
    perf_viz::end_record!("Status line");

    //
    //    Recolouring
    //
    perf_viz::start_record!("Recolouring");
    if !ui.window_is_focused {
        for t_or_r in text_or_rects.iter_mut() {
            u!{TextOrRect}
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
    perf_viz::end_record!("Recolouring");
    perf_viz::end_record!("fill text_or_rects");

    ui.frame_end();

    stats.latest_view_function_time_span = stats.latest_view_function_time_span.end_if_started();

    ViewOutput {
        text_or_rects,
        action,
    }
}

// This is a private function so having a slightly awkward API is tolerable
#[allow(clippy::too_many_arguments)]
fn command_button<'view> (
    ui: &mut ui::State,
    id: ui::Id,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    rect: ScreenSpaceRect,
    dimensions: Dimensions,
    commands: &CommandsMap,
    command_key: &CommandKey,
    action: &mut ViewAction,
) {
    let cmd_option = commands.get(command_key);
    invariant_assert!(cmd_option.is_some(), "{:?} has no command associated with it!", command_key);

    if let Some(cmd) = cmd_option {
        let Dimensions {
            window: sswh!(_w, height),
            ..
        } = dimensions;
        let SpacingAllSpec { margin, .. } = get_menu_spacing(height);

        if do_outline_button(
            ui,
            id,
            text_or_rects,
            OutlineButtonSpec {
                text: cmd.label,
                size: TAB_SIZE,
                char_dim: dimensions.font.tab_char_dim,
                layout: TextLayout::Unbounded,
                margin: Spacing::All(margin * LIST_MARGIN_TO_PADDING_RATIO),
                rect,
                z: TAB_Z,
                ..d!()
            },
        ) {
            *action = ViewAction::Command(*command_key);
        }
    }
}

// TODO add arguments and calculate how many will fit based on screen size etc.
fn calculate_window_size() -> ListSelectionWindowSize {
    ListSelectionWindowSize::new(5).unwrap()
}

fn render_file_switcher_menu<'view>(
    buffer_index: g_i::Index,
    FileSwitcherView { search, results }: &'view FileSwitcherView,
    ui: &mut ui::State,
    current_buffer_id: BufferId,
    dimensions: Dimensions,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    action: &mut ViewAction,
) {
    let FontInfo {
        ref tab_char_dim,
        ref find_replace_char_dim,
        ..
    } = dimensions.font;

    let FileSwitcherInfo {
        padding,
        margin,
        list_margin,
        top_y,
        bottom_y,
        label_rect,
        search_outer_rect,
        search_text_xywh,
        ..
    } = get_file_switcher_info(dimensions);
    let outer_rect = get_full_width_ssr(top_y, dimensions.window.w, bottom_y);
    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: outer_rect,
        colour: CHROME_BACKGROUND_COLOUR,
        z: FIND_REPLACE_BACKGROUND_Z,
    }));

    text_or_rects.push(TextOrRect::Text(TextSpec {
        text: if search.chars.is_empty() {
            "Find File"
        } else {
            // cheap hack to avoid lifetime issues
            match results.len() {
                0 => "Find File (0 results)",
                1 => "Find File (1 result)",
                2 => "Find File (2 results)",
                3 => "Find File (3 results)",
                4 => "Find File (4 results)",
                5 => "Find File (5 results)",
                6 => "Find File (6 results)",
                7 => "Find File (7 results)",
                8 => "Find File (8 results)",
                _ => "Find File (9+ results)",
            }
        },
        size: FIND_REPLACE_SIZE,
        layout: TextLayout::Unbounded,
        spec: VisualSpec {
            rect: label_rect,
            colour: CHROME_TEXT_COLOUR,
            z: FIND_REPLACE_Z,
        },
    }));

    let mut current_rect = search_outer_rect;
    let list_bottom_margin = list_margin.into_ltrb().b;
    let vertical_shift =
        search_text_xywh.wh.h + margin.into_ltrb().b - list_bottom_margin;

    let search_buffer_id = b_id!(BufferIdKind::FileSwitcher, buffer_index);

    let window_size = calculate_window_size();

    let mut navigated_result = None;

    if action.is_none() {
        u!{ui::Navigation}

        match ui.navigation {
            None => {
                if let ui::Id::TaggedListSelection(
                    ui::Tag::FileSwitcherResults,
                    selection
                ) = ui.keyboard.hot
                {
                    navigated_result = Some(selection);
                }
            }
            Up => {
                if let ui::Id::TaggedListSelection(
                    ui::Tag::FileSwitcherResults,
                    selection,
                ) = ui.keyboard.hot
                {
                    if selection.index == 0 {
                        *action = ViewAction::Input(Input::SelectBuffer(search_buffer_id));

                        ui.keyboard.set_not_hot();
                    } else {
                        navigated_result = Some(selection.move_up());
                    }
                }
            }
            Down => {
                if let ui::Id::TaggedListSelection(
                    ui::Tag::FileSwitcherResults,
                    selection,
                ) = ui.keyboard.hot
                {
                    navigated_result = Some(selection.move_down(window_size, results.len()));
                } else if !results.is_empty() {
                    navigated_result = Some(d!());
                    *action = ViewAction::Input(
                        Input::SelectBuffer(b_id!(BufferIdKind::None, buffer_index))
                    );
                }
            }
            Interact => {
                if let ui::Id::TaggedListSelection(
                    ui::Tag::FileSwitcherResults,
                    selection,
                ) = ui.keyboard.hot
                {
                    *action = results
                        .get(selection.index)
                        .cloned()
                        .map(Input::OpenOrSelectBuffer)
                        .into();
                }
            }
        }
    }

    macro_rules! spaced_input_box {
        ($data: expr, $input: expr, $outer_rect: expr) => {{
            *action = into_action(text_box(
                ui,
                text_or_rects,
                $outer_rect,
                padding,
                *find_replace_char_dim,
                FIND_REPLACE_SIZE,
                TextBoxColour::Single(CHROME_TEXT_COLOUR),
                $data,
                $input,
                FIND_REPLACE_Z,
                current_buffer_id,
            ))
            .or(action.clone());
        }};
    }

    spaced_input_box!(search, search_buffer_id, current_rect);
    // We add the extra `list_bottom_margin` here but not in the loop so that the
    // spacing bewteen the textbox and the first button is the same as the spacing
    // between subsequent buttons. If we added it in both, there would be a double
    // margin between buttons.
    current_rect.min.y += vertical_shift;
    current_rect.min.y += list_bottom_margin;
    current_rect.max.y += vertical_shift;
    current_rect.max.y += list_bottom_margin;

    fn get_result_id(selection: ListSelection) -> ui::Id {
        ui::Id::TaggedListSelection(
            ui::Tag::FileSwitcherResults,
            selection
        )
    }

    let selection = navigated_result.unwrap_or_default();

    // TODO Is the reset bug caused by not calling set_next_hot on the skipped ones?
    for (result_index, result) in results.iter()
        .enumerate()
        .skip(selection.window_start)
        .take(window_size.get())
    {
        let path_text = result.to_str().unwrap_or("Non-UTF8 Path");
        let rect = shrink_by(current_rect, list_margin);

        let result_id = match navigated_result {
            Some(selection) if selection.index == result_index => {
                let result_id = get_result_id(selection);
                ui.keyboard.set_next_hot(result_id);
                result_id
            }
            _ => {
                get_result_id(ListSelection{
                    index: result_index,
                    ..selection
                })
            }
        };

        if do_outline_button(
            ui,
            result_id,
            text_or_rects,
            OutlineButtonSpec {
                text: &path_text,
                size: TAB_SIZE,
                char_dim: *tab_char_dim,
                layout: TextLayout::Unbounded,
                margin: list_margin,
                rect,
                z: TAB_Z,
                ..d!()
            },
        ) {
            *action = ViewAction::Input(Input::OpenOrSelectBuffer(result.to_owned()));
        }
        current_rect.min.y += vertical_shift;
        current_rect.max.y += vertical_shift;
    }
}

enum TextBoxColour {
    FromSpans,
    Single(Colour),
}
d!(for TextBoxColour: TextBoxColour::FromSpans);

#[perf_viz::record]
fn colourize<'text>(to_colourize: &'text str, spans: &Spans) -> Vec<ColouredText<'text>> {
    spans
        .labelled_slices(to_colourize)
        .map(move |l_s| ColouredText {
            text: std::borrow::Cow::Borrowed(l_s.slice),
            colour: match l_s.kind.get_byte() & 0b111 {
                1 => palette![cyan],
                2 => palette![green],
                3 => palette![yellow],
                4 => palette![magenta],
                _ => palette![blue]
            },
        }).collect()
}

// These are private functions so having a slightly awkward API is tolerable
#[allow(clippy::too_many_arguments)]
fn text_box<'view>(
    ui: &mut ui::State,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    outer_rect: ScreenSpaceRect,
    padding: Spacing,
    char_dim: CharDim,
    size: f32,
    text_colour: TextBoxColour,
    buffer_view_data: impl Into<&'view BufferViewData>,
    buffer_id: BufferId,
    z: u16,
    current_buffer_id: BufferId,
) -> Option<Input> {
    let mut input = None;

    let (clicked, button_state) = ui::do_button_logic(ui, ui_id!(format!("{:?}", buffer_id)), outer_rect);
    if clicked {
        input = Some(Input::SelectBuffer(buffer_id));
    }

    let (background_colour, cursor_alpha) = if current_buffer_id == buffer_id {
        (TEXT_BACKGROUND_COLOUR, ui.get_fade_alpha())
    } else {
        (
            match button_state {
                ButtonState::Usual => TEXT_BACKGROUND_COLOUR,
                ButtonState::Hover(_) => TEXT_HOVER_BACKGROUND_COLOUR,
                ButtonState::Pressed(_) => TEXT_PRESSED_BACKGROUND_COLOUR,
            },
            1.0,
        )
    };

    text_box_view(
        text_or_rects,
        outer_rect,
        padding,
        char_dim,
        size,
        text_colour,
        buffer_view_data.into(),
        background_colour,
        cursor_alpha,
        z,
    );

    input
}

#[allow(clippy::too_many_arguments)]
fn text_box_view<'view>(
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    outer_rect: ScreenSpaceRect,
    padding: Spacing,
    char_dim: CharDim,
    size: f32,
    text_colour: TextBoxColour,
    BufferViewData {
        highlights,
        cursors,
        scroll,
        chars,
        spans,
        ..
    }: &'view BufferViewData,
    background_colour: Colour,
    cursor_alpha: f32,
    z: u16,
) {
    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: outer_rect,
        colour: background_colour,
        z: z.saturating_sub(2),
    }));

    let scroll = *scroll;

    let text_box_pos = tbxy!{
        outer_rect.min.x,
        outer_rect.min.y,
    };
    let scroll_offset = text_box_to_screen(
        text_to_text_box(TextSpaceXY::default(), scroll),
        text_box_pos,
    );

    let offset_text_rect = shrink_by(ssr!(scroll_offset, outer_rect.max), padding);

    text_or_rects.push(TextOrRect::MulticolourText(MulticolourTextSpec {
        text: {
            perf_viz::record_guard!("de-roping for colourization");
            match text_colour {
                TextBoxColour::FromSpans => colourize(
                    &chars,
                    spans
                ),
                TextBoxColour::Single(colour) => {
                    perf_viz::record_guard!("Single colour chars.into()");
                    vec![ColouredText{ colour, text: chars.into() }]
                },
            }
        },
        size,
        layout: TextLayout::UnboundedLayoutClipped(
            ssr!(
                text_box_pos.x,
                text_box_pos.y,
                outer_rect.max.x,
                outer_rect.max.y,
            ),
            scroll
        ),
        rect: offset_text_rect,
        z,
    }));

    for c in cursors.iter() {
        let screen_xy = position_to_screen_space(c.position, char_dim, scroll, text_box_pos);
        let cursor_rect = shrink_by(ssr!(screen_xy, outer_rect.max), padding);
        text_or_rects.push(TextOrRect::Text(TextSpec {
            text: "▏",
            size,
            layout: TextLayout::Unbounded{},
            spec: VisualSpec {
                rect: cursor_rect,
                colour: match c.state {
                    CursorState::None => palette![red, cursor_alpha],
                    CursorState::PressedAgainstWall(_) => palette![yellow, cursor_alpha],
                },
                z: z.saturating_add(3),
            },
        }));
    }

    let ssxy!(x y) = offset_text_rect.min;
    let CharDim { w, h } = char_dim;
    text_or_rects.extend(
        highlights
            .iter()
            .filter_map(|Highlight { min, max, kind, .. }| {
                let mut rect = ssr!(
                    abs::Ratio::from(min.offset.0) * w + x,
                    abs::Ratio::from(min.line) * h + y,
                    abs::Ratio::from(max.offset.0) * w + x,
                    abs::Ratio::from(max.line + 1) * h + y
                );

                clamp_within(&mut rect, outer_rect);

                if rect.has_any_area() {
                    Some(TextOrRect::Rect(VisualSpec {
                        rect,
                        colour: highlight_kind_colour(*kind),
                        z: z.saturating_add(4),
                    }))
                } else {
                    None
                }
            }),
    );
}

pub fn make_active_tab_visible<'view>(
    ui: &mut ui::State,
    view: &'view View,
    Dimensions {
        font: FontInfo { tab_char_dim, .. },
        window: sswh!(window_width, _h)
    }: Dimensions,
) -> Option<()> {
    let target_index_or_max: usize = view.current_text_index().into();
    let tab_count = view.buffers_count().into();
    let tab_layout = get_tab_spaced_rect(&ui, tab_char_dim, 0, tab_count, window_width);
    let tab_width = tab_layout.width();

    make_nth_tab_visible_if_present(ui, target_index_or_max, tab_width, window_width);

    Some(())
}

fn unscrolled_tab_left_edge(
    target_index: usize,
    tab_width: abs::Length,
) -> abs::Pos {
    abs::Pos::from(
        abs::Ratio::from(target_index) * tab_width
    )
}

fn unscrolled_tab_right_edge(
    target_index: usize,
    tab_width: abs::Length,
) -> abs::Pos {
    unscrolled_tab_left_edge(target_index + 1, tab_width)
}

fn make_nth_tab_visible_if_present(
    ui: &mut ui::State,
    target_index: usize,
    tab_width: abs::Length,
    screen_width: abs::Length,
) {
    // This was written to fix a suddenly failing proptest. Maybe a simpler
    // overall solution without this special case exists?
    if tab_width >= screen_width {
        let to_make_visible = unscrolled_tab_left_edge(target_index, tab_width);

        ui.tab_scroll = to_make_visible;
        return
    }

    let min_pos: abs::Pos = d!();

    let to_make_visible =
        unscrolled_tab_left_edge(target_index, tab_width)
        + tab_width.halve();

    attempt_to_make_line_space_pos_visible(
        &mut ui.tab_scroll,
        (min_pos, screen_width),
        F32_0_1::MAX,
        F32_0_1::MAX,
        min_pos + to_make_visible,
    );
}

struct LineSpec {
    colour: Colour,
    thickness: abs::Length,
}

struct OutlineButtonSpec<'text> {
    text: &'text str,
    size: f32,
    char_dim: CharDim,
    layout: TextLayout,
    margin: Spacing,
    rect: ScreenSpaceRect,
    z: u16,
    underline: Option<LineSpec>,
    overline: Option<LineSpec>,
}
d!(for OutlineButtonSpec<'static>: OutlineButtonSpec {
    text: "OutlineButtonSpec default",
    size: 16.0,
    char_dim: char_dim!(16.0 16.0),
    layout: TextLayout::Unbounded,
    margin: d!(),
    rect: d!(),
    z: gl_layer::DEFAULT_Z,
    underline: d!(),
    overline: d!(),
});

fn do_outline_button<'view>(
    ui_state: &mut ui::State,
    id: ui::Id,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    spec: OutlineButtonSpec<'view>,
) -> bool {
    let (clicked, state) = ui::do_button_logic(ui_state, id, spec.rect);

    render_outline_button(text_or_rects, spec, state, ui_state.get_fade_alpha());

    clicked
}

fn render_outline_button<'view>(
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    OutlineButtonSpec {
        text,
        size,
        char_dim,
        layout,
        margin,
        rect,
        underline,
        overline,
        z,
    }: OutlineButtonSpec<'view>,
    state: ButtonState,
    fade_alpha: f32,
) {
    u!{ButtonState}

    const BACKGROUND_COLOUR: Colour = TAB_BACKGROUND_COLOUR;
    const TEXT_COLOUR: Colour = TAB_TEXT_COLOUR;

    let text_z = z.saturating_add(3);
    let overline_z = z.saturating_add(2);
    let underline_z = z.saturating_add(1);
    //z is the rect z
    let outline_z = z.saturating_sub(1);

    macro_rules! highlight_colour {
        ($input_type: expr) => {{
            u!{ui::InputType}
            match $input_type {
                Mouse => palette![yellow, fade_alpha],
                Keyboard => palette![blue, fade_alpha],
                Both => palette![green, fade_alpha],
            }
        }};
    }

    macro_rules! push_text {
        () => {
            text_or_rects.push(TextOrRect::Text(TextSpec {
                text,
                size,
                layout,
                spec: VisualSpec {
                    rect: get_inner_text_rect(text, char_dim, rect),
                    colour: TEXT_COLOUR,
                    z: text_z,
                },
            }));
        };
    }

    match state {
        Pressed(input_type) => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                colour: highlight_colour!(input_type),
                z,
            }));
            push_text!();
        }
        Hover(input_type) => {
            // outline
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect: enlarge_by(rect, margin),
                colour: highlight_colour!(input_type),
                z: outline_z,
            }));

            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                colour: BACKGROUND_COLOUR,
                z,
            }));
            push_text!();
        }
        Usual => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                colour: BACKGROUND_COLOUR,
                z,
            }));
            push_text!();
        }
    }

    if let Some(LineSpec { colour, thickness }) = overline {
        text_or_rects.push(TextOrRect::Rect(VisualSpec {
            rect: rect.with_max_y(rect.min.y + thickness),
            colour,
            z: overline_z,
        }));
    }

    if let Some(LineSpec { colour, thickness }) = underline {
        text_or_rects.push(TextOrRect::Rect(VisualSpec {
            rect: rect.with_min_y(rect.max.y - thickness),
            colour,
            z: underline_z,
        }));
    }
}

pub const TEXT_SIZE: f32 = 32.0;
pub const FIND_REPLACE_SIZE: f32 = 26.0;
pub const STATUS_SIZE: f32 = 22.0;
pub const TAB_SIZE: f32 = 16.0;

pub const SEPARATOR_LINE_THICKNESS: abs::Length = abs::Length::TWO;

pub const TEXT_SIZES: [f32; 4] = [TEXT_SIZE, STATUS_SIZE, TAB_SIZE, FIND_REPLACE_SIZE];
pub const SCROLL_MULTIPLIER: f32 = TEXT_SIZE * 3.0;

pub fn get_font_info(char_dims: &[CharDim]) -> FontInfo {
    debug_assert!(
        char_dims.len() >= TEXT_SIZES.len(),
        "get_font_info didn't receive enough char_dims"
    );

    FontInfo {
        text_char_dim: char_dims[0],
        status_char_dim: char_dims[1],
        tab_char_dim: char_dims[2],
        find_replace_char_dim: char_dims[3],
    }
}

/// You can use any u8 as a base, and this function will make a z that allows UI widgets to use
/// some more layers for other stuff by adding small numbers to it. Say 1, 2, 3 etc.
const fn z_from_base(base: u8) -> u16 {
    (base as u16) << 8
}

// Reminder: smaller means farther away.
pub const EDIT_Z: u16 = z_from_base(32);
pub const FIND_REPLACE_BACKGROUND_Z: u16 = z_from_base(32 + 8);
pub const FIND_REPLACE_Z: u16 = z_from_base(32 + 16);
pub const STATUS_BACKGROUND_Z: u16 = z_from_base(64);
pub const TAB_BACKGROUND_Z: u16 = STATUS_BACKGROUND_Z;
pub const STATUS_Z: u16 = z_from_base(128);
pub const TAB_Z: u16 = STATUS_Z;

/// Ratios to tab width
const TAB_MARGIN_RATIO: abs::Ratio = abs::Ratio::THIRTY_SECONDTH;
const TAB_PADDING_RATIO: abs::Ratio = abs::Ratio::SIXTY_FOURTH;
const TAB_MIN_W: abs::Length = abs::Length::ONE_TWENTY_EIGHT;
const TAB_MIN_PADDING: abs::Length = abs::Length::TWO;//TAB_MIN_W * TAB_PADDING_RATIO;
const TAB_MIN_MARGIN: abs::Length = abs::Length::FOUR;//TAB_MIN_W * TAB_MARGIN_RATIO;

#[derive(Clone, Copy)]
pub enum Spacing {
    All(abs::Length),
    Horizontal(abs::Length),
    Vertical(abs::Length),
    Axis(abs::Length, abs::Length),
    LeftTopRightBottom(abs::Length, abs::Length, abs::Length, abs::Length),
}
d!(for Spacing: Spacing::All(d!()));

/// LRTB is short for `LeftTopRightBottom`. This represents what the values of a spacing would be
/// if the spacing was the `LeftTopRightBottom` variant.
struct LRTB {
    l: abs::Length,
    r: abs::Length,
    t: abs::Length,
    b: abs::Length,
}

impl Spacing {
    fn into_ltrb(self) -> LRTB {
        u!{Spacing}
        let (l, t, r, b) = match self {
            All(n) => (n, n, n, n),
            Horizontal(n) => (n, d!(), n, d!()),
            Vertical(n) => (d!(), n, d!(), n),
            Axis(x, y) => (x, y, x, y),
            LeftTopRightBottom(l, t, r, b) => (l, t, r, b),
        };

        LRTB { l, t, r, b }
    }
}
struct SpacedRect {
    rect: ScreenSpaceRect,
    padding: Spacing,
    margin: Spacing,
}

impl SpacedRect {
    fn width(&self) -> abs::Length {
        enlarge_by(self.rect, self.margin).width()
    }
}

fn get_tab_spaced_rect(
    ui: &ui::State,
    tab_char_dim: CharDim,
    tab_index: usize,
    tab_count: usize,
    width: abs::Length,
) -> SpacedRect {
    let UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        edit_y
    } = upper_position_info(&tab_char_dim);
    let tab_w = width / tab_count.into();
    let tab_w = if tab_w > TAB_MIN_W {
        tab_w
    } else {
        // NaN ends up here
        TAB_MIN_W
    };
    let tab_padding = tab_w * TAB_PADDING_RATIO;
    let tab_margin = tab_w * TAB_MARGIN_RATIO;

    let min_x: abs::Pos = unscrolled_tab_left_edge(tab_index, tab_w) + tab_padding - ui.tab_scroll;
    let max_x: abs::Pos = unscrolled_tab_right_edge(tab_index, tab_w) - tab_padding - ui.tab_scroll;

    SpacedRect {
        padding: Spacing::Axis(tab_padding, tab_v_padding),
        margin: Spacing::Axis(tab_margin, tab_v_margin),
        rect: ssr!(min_x, tab_y, max_x, edit_y - tab_y),
    }
}

fn enlarge_by(
    ssr!(min_x, min_y, max_x, max_y): ScreenSpaceRect,
    enlarge_amount: Spacing,
) -> ScreenSpaceRect {
    let LRTB {
        l: min_x_e,
        t: min_y_e,
        r: max_x_e,
        b: max_y_e,
    } = enlarge_amount.into_ltrb();
    ssr!(
        min_x - min_x_e,
        min_y - min_y_e,
        max_x + max_x_e,
        max_y + max_y_e
    )
}

fn shrink_by(
    ssr!(min_x, min_y, max_x, max_y): ScreenSpaceRect,
    shrink_amount: Spacing,
) -> ScreenSpaceRect {
    let LRTB {
        l: min_x_s,
        t: min_y_s,
        r: max_x_s,
        b: max_y_s,
    } = shrink_amount.into_ltrb();
    ssr!(
        min_x + min_x_s,
        min_y + min_y_s,
        max_x - max_x_s,
        max_y - max_y_s
    )
}

pub fn get_inner_text_rect(text: &str, char_dim: CharDim, rect: ScreenSpaceRect) -> ScreenSpaceRect {
    let text_w = abs::Ratio::from(text.chars().count()) * char_dim.w;

    center_within((text_w.get().into(), char_dim.h.get().into()), rect)
}

/// returns a rectangle with the passed width and height centered inside the passed rectangle.
fn center_within(
    (w, h): (abs::Length, abs::Length),
    rect: ScreenSpaceRect
) -> ScreenSpaceRect {
    let (middle_x, middle_y) = rect.middle();
    let min = ssxy!(middle_x - (w.halve()), middle_y - (h.halve()));
    ssr!(min.x, min.y, min.x + w, min.y + h)
}

struct UpperPositionInfo {
    tab_v_padding: abs::Length,
    tab_v_margin: abs::Length,
    tab_y: abs::Pos,
    edit_y: abs::Pos,
}

#[perf_viz::record]
fn upper_position_info(tab_char_dim: &CharDim) -> UpperPositionInfo {
    let tab_v_padding = TAB_MIN_PADDING;
    let tab_v_margin = TAB_MIN_MARGIN;
    let tab_y = tab_v_margin.into();
    let edit_y = tab_y + tab_v_padding + tab_char_dim.h + tab_v_padding + tab_y;

    UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        edit_y,
    }
}

/// A specification for spacing around something, containing values suitable for passing to `Spacing::All`.
struct SpacingAllSpec {
    margin: abs::Length,
    padding: abs::Length,
}

fn get_menu_spacing(height: abs::Length) -> SpacingAllSpec {
    /// Ratios to screen height
    const MARGIN_RATIO: abs::Ratio = abs::Ratio::SIXTEENTH;
    const PADDING_RATIO: abs::Ratio = abs::Ratio::THIRTY_SECONDTH;

    let min_margin: abs::Length = MARGIN_RATIO * abs::Length::TWO_FIFTY_SIX;
    let min_padding: abs::Length = PADDING_RATIO * abs::Length::TWO_FIFTY_SIX;

    let mut margin = MARGIN_RATIO * height;
    margin = if margin > min_margin {
        margin
    } else {
        //NaN ends up here
        min_margin
    };
    let mut padding = PADDING_RATIO * height;
    padding = if padding > min_padding {
        padding
    } else {
        //NaN ends up here
        min_padding
    };

    SpacingAllSpec {
        margin,
        padding
    }
}

pub struct FindReplaceInfo {
    pub margin: Spacing,
    pub padding: Spacing,
    pub top_y: abs::Pos,
    pub bottom_y: abs::Pos,
    pub label_rect: ScreenSpaceRect,
    pub find_outer_rect: ScreenSpaceRect,
    pub find_text_xywh: TextBoxXYWH,
    pub replace_outer_rect: ScreenSpaceRect,
    pub replace_text_xywh: TextBoxXYWH,
}

pub fn get_find_replace_info(
    Dimensions {
        font: FontInfo {
            status_char_dim,
            find_replace_char_dim,
            ..
        },
        window: sswh!(width, height),
    }: Dimensions,
) -> FindReplaceInfo {
    let SpacingAllSpec { margin, padding } = get_menu_spacing(height);

    let bottom_y = get_status_line_y(status_char_dim, height);
    // assuming that there are two text buffers and a heading, each with the same
    // margin and padding, without the margins being duplicated
    let top_y = bottom_y - (
        margin * abs::Ratio::FOUR
        + padding * abs::Ratio::SIX
        + find_replace_char_dim.h * abs::Ratio::THREE
    );

    let mut current_y = top_y + margin;
    let text_height = abs::Ratio::TWO * padding + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - (margin + $h_padding).double(),
                text_height - padding.double()
            )
        };
    }

    let label_rect = text_rect!(abs::Length::ZERO).into();

    current_y += text_height + margin;
    let find_outer_rect = ssr!(
        margin,
        current_y,
        width - margin,
        current_y + text_height
    );
    let find_text_xywh = text_rect!();

    current_y += text_height + margin;
    let replace_outer_rect = ssr!(
        margin,
        current_y,
        width - margin,
        current_y + text_height
    );
    let replace_text_xywh = text_rect!();

    FindReplaceInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        top_y,
        bottom_y,
        label_rect,
        find_outer_rect,
        find_text_xywh,
        replace_outer_rect,
        replace_text_xywh,
    }
}

pub struct FileSwitcherInfo {
    pub margin: Spacing,
    pub padding: Spacing,
    pub list_margin: Spacing,
    pub top_y: abs::Pos,
    pub bottom_y: abs::Pos,
    pub label_rect: ScreenSpaceRect,
    pub search_outer_rect: ScreenSpaceRect,
    pub search_text_xywh: TextBoxXYWH,
}

const LIST_MARGIN_TO_PADDING_RATIO: abs::Ratio = abs::Ratio::EIGHTH;

pub fn get_file_switcher_info(
    Dimensions {
        font: FontInfo {
            status_char_dim,
            find_replace_char_dim,
            tab_char_dim,
            ..
        },
        window: sswh!(width, height),
    }: Dimensions,
) -> FileSwitcherInfo {
    let SpacingAllSpec { margin, padding } = get_menu_spacing(height);

    let bottom_y = get_status_line_y(status_char_dim, height);
    let top_y = upper_position_info(&tab_char_dim).edit_y;

    let mut current_y = top_y + margin;
    let text_height = padding.double() + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - (margin + $h_padding).double(),
                text_height - padding.double()
            )
        };
    }

    let label_rect = text_rect!(abs::Length::ZERO).into();

    current_y += text_height + margin;
    let search_outer_rect = ssr!(
        margin,
        current_y,
        width - margin,
        current_y + text_height
    );
    let search_text_xywh = text_rect!();

    FileSwitcherInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        list_margin: Spacing::All(
            margin * LIST_MARGIN_TO_PADDING_RATIO
        ),
        top_y,
        bottom_y,
        label_rect,
        search_outer_rect,
        search_text_xywh,
    }
}

pub struct GoToPositionInfo {
    pub margin: Spacing,
    pub padding: Spacing,
    pub top_y: abs::Pos,
    pub bottom_y: abs::Pos,
    pub label_rect: ScreenSpaceRect,
    pub input_outer_rect: ScreenSpaceRect,
    pub input_text_xywh: TextBoxXYWH,
}

pub fn get_go_to_position_info(
    Dimensions {
        font: FontInfo {
            find_replace_char_dim,
            tab_char_dim,
            ..
        },
        window: sswh!(width, height),
    }: Dimensions,
) -> GoToPositionInfo {
    let SpacingAllSpec { margin, padding } = get_menu_spacing(height);

    let top_y = upper_position_info(&tab_char_dim).edit_y;

    let mut current_y = top_y + margin;
    let text_height = padding.double() + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - (margin + $h_padding).double(),
                text_height - padding.double()
            )
        };
    }

    let label_rect = text_rect!(abs::Length::ZERO).into();

    current_y += text_height + margin;
    let input_outer_rect = ssr!(
        margin,
        current_y,
        width - margin,
        current_y + text_height
    );
    let input_text_xywh = text_rect!();

    let bottom_y = current_y + text_height + padding + margin;

    GoToPositionInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        top_y,
        bottom_y,
        label_rect,
        input_outer_rect,
        input_text_xywh,
    }
}

pub struct CommandMenuInfo {
    pub margin: Spacing,
    pub padding: Spacing,
    pub top_y: abs::Pos,
    pub bottom_y: abs::Pos,
    pub outer_rect: ScreenSpaceRect,
    pub first_button_rect: ScreenSpaceRect,
    pub list_margin: Spacing,
}

pub fn get_command_menu_info(
    dimensions: Dimensions,
) -> CommandMenuInfo {
    let Dimensions {
        font: FontInfo {
            tab_char_dim,
            ..
        },
        window: sswh!(width, _),
    } = dimensions;

    let CoverTextAreaInfo {
        margin,
        padding,
        top_y,
        bottom_y,
        outer_rect,
    } = cover_text_area_info(dimensions);

    let list_margin = margin * LIST_MARGIN_TO_PADDING_RATIO;

    let first_button_rect = shrink_by(
        ssr!(
            _,
            _,
            width,
            top_y + padding.double() + tab_char_dim.h
        ),
        Spacing::Horizontal(margin)
    ).with_min_y(top_y + list_margin);

    CommandMenuInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        top_y,
        bottom_y,
        outer_rect,
        list_margin: Spacing::All(list_margin),
        first_button_rect,
    }
}

// If we end up with a third menu like this and CommandMenu, merge the infos.
pub struct DebugMenuInfo {
    pub margin: Spacing,
    pub padding: Spacing,
    pub top_y: abs::Pos,
    pub bottom_y: abs::Pos,
    pub outer_rect: ScreenSpaceRect,
    pub first_button_rect: ScreenSpaceRect,
    pub list_margin: Spacing,
}

pub fn get_debug_menu_info(
    dimensions: Dimensions,
) -> DebugMenuInfo {
    let Dimensions {
        font: FontInfo {
            tab_char_dim,
            ..
        },
        window: sswh!(width, _),
    } = dimensions;

    let CoverTextAreaInfo {
        margin,
        padding,
        top_y,
        bottom_y,
        outer_rect,
    } = cover_text_area_info(dimensions);

    let list_margin = margin * LIST_MARGIN_TO_PADDING_RATIO;

    let first_button_rect = shrink_by(
        ssr!(
            _,
            _,
            width,
            top_y + padding.double() + tab_char_dim.h
        ),
        Spacing::Horizontal(margin)
    ).with_min_y(top_y + list_margin);

    DebugMenuInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        top_y,
        bottom_y,
        outer_rect,
        list_margin: Spacing::All(list_margin),
        first_button_rect,
    }
}

/// Info for making a rect that completely covers the text area.
pub struct CoverTextAreaInfo {
    pub margin: abs::Length,
    pub padding: abs::Length,
    pub top_y: abs::Pos,
    pub bottom_y: abs::Pos,
    pub outer_rect: ScreenSpaceRect,
}

pub fn cover_text_area_info(
    Dimensions {
        font: FontInfo {
            status_char_dim,
            tab_char_dim,
            ..
        },
        window: sswh!(width, height),
    }: Dimensions,
) -> CoverTextAreaInfo {
    let SpacingAllSpec { margin, padding } = get_menu_spacing(height);

    let top_y = upper_position_info(&tab_char_dim).edit_y;
    let bottom_y = get_status_line_y(status_char_dim, height);

    let outer_rect = get_full_width_ssr(top_y, width, bottom_y);

    CoverTextAreaInfo {
        margin,
        padding,
        top_y,
        bottom_y,
        outer_rect,
    }
}

fn get_status_line_y(status_char_dim: CharDim, height: abs::Length) -> abs::Pos {
    abs::Pos::from(height) - (status_char_dim.h + SEPARATOR_LINE_THICKNESS.double())
}

fn get_full_width_ssr<
    F32_0: Into<f32>,
    F32_1: Into<f32>,
    F32_2: Into<f32>,
>(
    top_y: F32_0,
    width: F32_1,
    bottom_y: F32_2
) -> ScreenSpaceRect {
    ssr!(0.0, top_y.into(), width.into(), bottom_y.into())
}

pub fn get_edit_buffer_xywh(
    mode: WimpMenuMode,
    dimensions: Dimensions,
) -> TextBoxXYWH {
    let Dimensions {
        font: FontInfo {
            status_char_dim,
            ref tab_char_dim,
            ..
        },
        window: sswh!(width, height),
    } = dimensions;
    u!{WimpMenuMode}
    let max_y = match mode {
        Hidden | GoToPosition => get_status_line_y(status_char_dim, height),
        FindReplace(_) => get_find_replace_info(dimensions).top_y,
        FileSwitcher | Command | Debug => height.into(),
    };
    let y = upper_position_info(tab_char_dim).edit_y;
    TextBoxXYWH {
        xy: tbxy!{ abs::Pos::ZERO, y },
        wh: ScreenSpaceWH {
            w: width,
            h: abs::Length::from(max_y - y),
        },
    }
}

pub fn get_current_buffer_rect(
    current_buffer_kind: BufferIdKind,
    mode: WimpMenuMode,
    dimensions: Dimensions,
) -> TextBoxXYWH {
    u!{BufferIdKind}
    match current_buffer_kind {
        None => d!(),
        Text => get_edit_buffer_xywh(mode, dimensions),
        Find => get_find_replace_info(dimensions).find_text_xywh,
        Replace => get_find_replace_info(dimensions).replace_text_xywh,
        FileSwitcher => get_file_switcher_info(dimensions).search_text_xywh,
        GoToPosition => get_go_to_position_info(dimensions).input_text_xywh,
    }
}

/// This function determines whether the mouse cursor should use the text selection icon or not.
pub fn should_show_text_cursor(
    xy: ScreenSpaceXY,
    mode: WimpMenuMode,
    dimensions: Dimensions,
) -> bool {
    u!{WimpMenuMode}

    let inside_edit_buffer = inside_rect(xy, get_edit_buffer_xywh(mode, dimensions).into());

    inside_edit_buffer || match mode {
        Hidden | Command | Debug => false,
        FindReplace(_) => {
            let FindReplaceInfo {
                find_outer_rect,
                replace_outer_rect,
                ..
            } = get_find_replace_info(dimensions);

            inside_rect(xy, find_outer_rect)
            || inside_rect(xy, replace_outer_rect)
        }
        FileSwitcher => {
            let FileSwitcherInfo {
                search_outer_rect,
                ..
            } = get_file_switcher_info(dimensions);

            inside_rect(xy, search_outer_rect)
        }
        GoToPosition => {
            let GoToPositionInfo {
                input_outer_rect,
                ..
            } = get_go_to_position_info(dimensions);

            inside_rect(xy, input_outer_rect)
        }

    }
}

pub fn inside_tab_area(
    ScreenSpaceXY { x: _, y }: ScreenSpaceXY,
    FontInfo {
        ref tab_char_dim, ..
    }: FontInfo,
) -> bool {
    y < upper_position_info(tab_char_dim).edit_y
}

#[cfg(test)]
mod wimp_render_tests;
