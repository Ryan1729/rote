use gl_layer::{ColouredText, MulticolourTextSpec, TextLayout, TextOrRect, TextSpec, VisualSpec};
use wimp_types::{ui_id, ui, ui::{ButtonState}, BufferStatus, ClipboardProvider, Dimensions, LocalMenu, RunConsts, RunState, advance_local_menu};
use macros::{c, d};
use platform_types::{screen_positioning::*, *};
use std::cmp::max;

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
    use HighlightKind::*;
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

#[derive(Clone, Default, Debug)]
pub struct ViewOutput<'view> {
    pub text_or_rects: Vec<TextOrRect<'view>>,
    pub input: Option<Input>,
}

pub fn view<'view>(
    RunState {
        ref mut ui,
        ref view,
        ref buffer_status_map,
        ref mut local_menu,
        dimensions,
        ..
    }: &'view mut RunState,
    RunConsts {
        commands
    }: &RunConsts,
    dt: std::time::Duration,
) -> ViewOutput<'view> {
    ui::begin_view(ui, view);

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

    let mut view_output = ViewOutput {
        text_or_rects: Vec::with_capacity(view.buffers.len() * PER_BUFFER_TEXT_OR_RECT_ESTIMATE),
        input: None,
    };

    let UpperPositionInfo {
        edit_y,
        ..
    } = upper_position_info(&tab_char_dim);

    view_output.text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: ssr!(0.0, 0.0, width, edit_y),
        color: TAB_BAR_BACKGROUND_COLOUR,
        z: TAB_BACKGROUND_Z,
    }));

    //
    // Tabs
    //

    let selected_index = view.get_visible_index_or_max();

    let tab_count = view.buffers.len();
    for (i, BufferView { name_string, .. }) in view.buffers.iter().enumerate() {
        let SpacedRect {
            padding,
            margin,
            rect,
        } = get_tab_spaced_rect(&ui, *tab_char_dim, i, tab_count, width);
        if rect.min.0 > width {
            break;
        }

        if do_outline_button(
            ui,
            ui_id!(i),
            &mut view_output.text_or_rects,
            OutlineButtonSpec {
                text: &name_string,
                size: TAB_SIZE,
                char_dim: *tab_char_dim,
                layout: TextLayout::SingleLine,
                margin,
                rect,
                underline: if i == selected_index {
                    Some(LineSpec {
                        colour: palette![blue],
                        thickness: padding.into_ltrb().b,
                    })
                } else {
                    None
                },
                overline: match buffer_status_map
                    .get(
                        view.index_state,
                        view.index_state.new_index(g_i::IndexPart::or_max(i)),
                    )
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
            view_output.input = Some(Input::SelectBuffer(b_id!(
                BufferIdKind::Text,
                view.index_state.new_index(g_i::IndexPart::or_max(i))
            )))
        }
    }

    if let Some((index, BufferView { data, .. })) = view.get_visible_index_and_buffer() {
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

        let edit_buffer_text_rect = get_edit_buffer_xywh(view.menu.get_mode(), dimensions);

        let edit_buffer_text_rect: ScreenSpaceRect = edit_buffer_text_rect.into();
        view_output.input = text_box(
            ui,
            &mut view_output.text_or_rects,
            edit_buffer_text_rect,
            d!(),
            *text_char_dim,
            TEXT_SIZE,
            TextBoxColour::FromSpans,
            &data,
            b_id!(BufferIdKind::Text, index),
            EDIT_Z,
            &view.current_buffer_id,
        )
        .or(view_output.input);

        macro_rules! handle_view_menu {
            () => {
                match &view.menu {
                    MenuView::None => {}
                    MenuView::FindReplace(FindReplaceView {
                        mode,
                        find,
                        replace,
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
                        match mode {
                            FindReplaceMode::CurrentFile => {
                                let outer_rect = ScreenSpaceRect {
                                    min: (0.0, top_y),
                                    max: (width, bottom_y),
                                };
                                view_output.text_or_rects.push(TextOrRect::Rect(VisualSpec {
                                    rect: outer_rect,
                                    color: CHROME_BACKGROUND_COLOUR,
                                    z: FIND_REPLACE_BACKGROUND_Z,
                                }));
                                view_output.text_or_rects.push(TextOrRect::Text(TextSpec {
                                    text: "In current file",
                                    size: FIND_REPLACE_SIZE,
                                    layout: TextLayout::SingleLine,
                                    spec: VisualSpec {
                                        rect: label_rect,
                                        color: CHROME_TEXT_COLOUR,
                                        z: FIND_REPLACE_Z,
                                    },
                                }));
                                macro_rules! spaced_input_box {
                                    ($data: expr, $input: expr, $outer_rect: expr) => {{
                                        view_output.input = text_box(
                                            ui,
                                            &mut view_output.text_or_rects,
                                            $outer_rect,
                                            padding,
                                            *find_replace_char_dim,
                                            FIND_REPLACE_SIZE,
                                            TextBoxColour::Single(CHROME_TEXT_COLOUR),
                                            &$data,
                                            $input,
                                            FIND_REPLACE_Z,
                                            &view.current_buffer_id,
                                        )
                                        .or(view_output.input);
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
                    MenuView::FileSwitcher(fs_view) => {
                        render_file_switcher_menu(
                            index,
                            fs_view,
                            ui,
                            view.current_buffer_id,
                            dimensions,
                            &mut view_output,
                        );
        
                        if view_output.input.is_none() {
                            print!(".");
                        } else {
                            dbg!(&view_output.input);
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
                        let outer_rect = ScreenSpaceRect {
                            min: (0.0, top_y),
                            max: (width, bottom_y),
                        };
                        view_output.text_or_rects.push(TextOrRect::Rect(VisualSpec {
                            rect: outer_rect,
                            color: CHROME_BACKGROUND_COLOUR,
                            z: FIND_REPLACE_BACKGROUND_Z,
                        }));
                        
                        view_output.text_or_rects.push(TextOrRect::Text(TextSpec {
                            text: "Go to position",
                            size: FIND_REPLACE_SIZE,
                            layout: TextLayout::SingleLine,
                            spec: VisualSpec {
                                rect: label_rect,
                                color: CHROME_TEXT_COLOUR,
                                z: FIND_REPLACE_Z,
                            },
                        }));
        
                        view_output.input = text_box(
                            ui,
                            &mut view_output.text_or_rects,
                            input_outer_rect,
                            padding,
                            *find_replace_char_dim,
                            FIND_REPLACE_SIZE,
                            TextBoxColour::Single(CHROME_TEXT_COLOUR),
                            go_to_position,
                            b_id!(BufferIdKind::GoToPosition, index),
                            FIND_REPLACE_Z,
                            &view.current_buffer_id,
                        ).or(view_output.input);
                    }
                }
            }
        }

        use LocalMenu::*;
        match *local_menu {
            Some(Command) => {
                let CommandMenuInfo {
                    top_y,
                    bottom_y,
                    label_rect,
                    ..
                } = get_command_menu_info(dimensions);
                let outer_rect = ScreenSpaceRect {
                    min: (0.0, top_y),
                    max: (dimensions.window.w, bottom_y),
                };
                view_output.text_or_rects.push(TextOrRect::Rect(VisualSpec {
                    rect: outer_rect,
                    color: CHROME_BACKGROUND_COLOUR,
                    z: FIND_REPLACE_BACKGROUND_Z,
                }));
                view_output.text_or_rects.push(TextOrRect::Text(TextSpec {
                    text: "TODO command menu",
                    size: FIND_REPLACE_SIZE,
                    layout: TextLayout::SingleLine,
                    spec: VisualSpec {
                        rect: label_rect,
                        color: CHROME_TEXT_COLOUR,
                        z: FIND_REPLACE_Z,
                    },
                }));
            }
            None => {
                handle_view_menu!()
            }
        }
    }

    //
    //    Status line
    //

    let status_line_y = get_status_line_y(*status_char_dim, height);

    view_output.text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: ScreenSpaceRect {
            min: (0.0, status_line_y),
            max: (width, status_line_y + SEPARATOR_LINE_THICKNESS),
        },
        color: TAB_BAR_BACKGROUND_COLOUR,
        z: STATUS_BACKGROUND_Z,
    }));

    let rect = ScreenSpaceRect {
        min: (0.0, status_line_y + SEPARATOR_LINE_THICKNESS),
        max: (width, height),
    };

    view_output.text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect,
        color: CHROME_BACKGROUND_COLOUR,
        z: STATUS_BACKGROUND_Z,
    }));

    view_output.text_or_rects.push(TextOrRect::Text(TextSpec {
        text: &view.status_line.chars,
        size: STATUS_SIZE,
        layout: TextLayout::SingleLine,
        spec: VisualSpec {
            rect: rect.with_min_y(status_line_y + 2.0 * SEPARATOR_LINE_THICKNESS),
            color: CHROME_TEXT_COLOUR,
            z: STATUS_Z,
        },
    }));

    if do_outline_button(
        ui,
        ui_id!(),
        &mut view_output.text_or_rects,
        OutlineButtonSpec {
            text: "≡", // U+2261
            size: STATUS_SIZE,
            char_dim: *status_char_dim,
            layout: TextLayout::SingleLine,
            margin: Spacing::All(SEPARATOR_LINE_THICKNESS),
            rect: ssr!(
                rect.max.0 - (status_char_dim.w + SEPARATOR_LINE_THICKNESS),
                status_line_y + SEPARATOR_LINE_THICKNESS,
                rect.max.0 - SEPARATOR_LINE_THICKNESS,
                rect.max.1 - SEPARATOR_LINE_THICKNESS
            ),
            z: STATUS_Z,
            ..d!()
        },
    ) {
        advance_local_menu(local_menu);
    }
      

    //
    //    Recolouring
    //    

    if !ui.window_is_focused {
        for t_or_r in view_output.text_or_rects.iter_mut() {
            use TextOrRect::*;
            match t_or_r {
                Rect(ref mut spec) => {
                    spec.color = grey_scale_dim!(spec.color);
                },
                Text(ref mut spec) => {
                    spec.spec.color = grey_scale_bright!(spec.spec.color);
                },
                MulticolourText(ref mut spec) => {
                    for ColouredText { ref mut color, .. } in spec.text.iter_mut() {
                        *color = grey_scale_bright!(*color)
                    }
                }
            }
        }
    }

    ui::end_view(ui);

    view_output
}

fn render_file_switcher_menu<'view>(
    buffer_index: g_i::Index,
    FileSwitcherView { search, results }: &'view FileSwitcherView,
    ui: &mut ui::State,
    current_buffer_id: BufferId,
    dimensions: Dimensions,
    view_output: &mut ViewOutput<'view>,
) {
    let text_or_rects = &mut view_output.text_or_rects;
    let input = &mut view_output.input;

    let FontInfo {
        ref tab_char_dim,
        ref find_replace_char_dim,
        ..
    } = dimensions.font;

    let FileSwitcherInfo {
        padding,
        margin,
        list_padding,
        list_margin,
        top_y,
        bottom_y,
        label_rect,
        search_outer_rect,
        search_text_xywh,
        ..
    } = get_file_switcher_info(dimensions);
    let outer_rect = ScreenSpaceRect {
        min: (0.0, top_y),
        max: (dimensions.window.w, bottom_y),
    };
    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: outer_rect,
        color: CHROME_BACKGROUND_COLOUR,
        z: FIND_REPLACE_BACKGROUND_Z,
    }));
    text_or_rects.push(TextOrRect::Text(TextSpec {
        text: if search.chars.len() == 0 {
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
        layout: TextLayout::SingleLine,
        spec: VisualSpec {
            rect: label_rect,
            color: CHROME_TEXT_COLOUR,
            z: FIND_REPLACE_Z,
        },
    }));

    let mut current_rect = search_outer_rect;
    let list_bottom_margin = list_margin.into_ltrb().b;
    let vertical_shift =
        search_text_xywh.wh.h + margin.into_ltrb().b - list_bottom_margin;

    let search_buffer_id = b_id!(BufferIdKind::FileSwitcher, buffer_index);

    fn get_result_id(index: usize) -> ui::Id {
        ui::Id::TaggedUsize(ui::Tag::FileSwitcherResults, index)
    }

    let mut navigated_result = None;

    if input.is_none() {
        use ui::Navigation::*;
        match if_changed::dbg!(ui.navigation) {
            None => {
                if current_buffer_id.kind != BufferIdKind::None {
                    // do nothing
                } else if let ui::Id::TaggedUsize(
                    ui::Tag::FileSwitcherResults,
                    result_index,
                ) = ui.keyboard.hot
                {
                    navigated_result = Some(result_index);
                }
            }
            Up => {
                dbg!("Up", ui.keyboard.hot);
                if let ui::Id::TaggedUsize(
                    ui::Tag::FileSwitcherResults,
                    result_index,
                ) = ui.keyboard.hot
                {
                    if result_index == 0 {
                        *input = Some(Input::SelectBuffer(search_buffer_id));
                    } else {
                        navigated_result = Some(result_index - 1);
                    }
                }
            }
            Down => {
                if current_buffer_id == search_buffer_id {
                    if results.len() > 0 {
                        navigated_result = Some(0);
                        *input =
                            Some(Input::SelectBuffer(b_id!(BufferIdKind::None, buffer_index)));
                    }
                } else if current_buffer_id.kind != BufferIdKind::None {
                    // do nothing
                } else if let ui::Id::TaggedUsize(
                    ui::Tag::FileSwitcherResults,
                    result_index,
                ) = ui.keyboard.hot
                {
                    navigated_result = Some((result_index + 1) % results.len());
                }
            }
            Interact => {
                if current_buffer_id.kind != BufferIdKind::None {
                    // do nothing
                } else if let ui::Id::TaggedUsize(
                    ui::Tag::FileSwitcherResults,
                    result_index,
                ) = ui.keyboard.hot
                {
                    *input = results
                        .get(result_index)
                        .cloned()
                        .map(Input::OpenOrSelectBuffer);
                }
            }
        }
    }

    macro_rules! spaced_input_box {
        ($data: expr, $input: expr, $outer_rect: expr) => {{
            *input = text_box(
                ui,
                &mut view_output.text_or_rects,
                $outer_rect,
                padding,
                *find_replace_char_dim,
                FIND_REPLACE_SIZE,
                TextBoxColour::Single(CHROME_TEXT_COLOUR),
                &$data,
                $input,
                FIND_REPLACE_Z,
                &current_buffer_id,
            )
            .or(input.clone());
        }};
    }

    spaced_input_box!(search, search_buffer_id, current_rect);
    // We add the extra `list_bottom_margin` here but not in the loop so that the
    // spacing bewteen the textbox and the first button is the same as the spacing
    // between subsequent buttons. If we added it in both, there would be a double
    // margin between buttons.
    current_rect.min.1 += vertical_shift;
    current_rect.min.1 += list_bottom_margin;
    current_rect.max.1 += vertical_shift;
    current_rect.max.1 += list_bottom_margin;

    for (result_index, result) in results.iter().enumerate() {
        let path_text = result.to_str().unwrap_or("Non-UTF8 Path");
        let rect = enlarge_by(shrink_by(current_rect, margin), list_padding);

        let result_id = get_result_id(result_index);

        match navigated_result {
            Some(i) if i == result_index => {
                ui.keyboard.set_next_hot(result_id);
            }
            _ => {}
        };

        if do_outline_button(
            ui,
            result_id,
            &mut view_output.text_or_rects,
            OutlineButtonSpec {
                text: &path_text,
                size: TAB_SIZE,
                char_dim: *tab_char_dim,
                layout: TextLayout::SingleLine,
                margin: list_margin,
                rect,
                z: TAB_Z,
                ..d!()
            },
        ) {
            *input = Some(Input::OpenOrSelectBuffer(result.to_owned()));
        }
        current_rect.min.1 += vertical_shift;
        current_rect.max.1 += vertical_shift;
    }
}

enum TextBoxColour {
    FromSpans,
    Single(Colour),
}
d!(for TextBoxColour: TextBoxColour::FromSpans);

fn colourize<'text>(text: &'text str, spans: &[SpanView]) -> Vec<ColouredText<'text>> {
    let mut prev_byte_index = 0;
    spans.iter().map(|s| {
        let text = &text[prev_byte_index..s.end_byte_index];
        let text = text.trim_end();
        let output = ColouredText {
            text,
            color: match s.kind.get_byte() & 0b111 {
                1 => palette![cyan],
                2 => palette![green],
                3 => palette![yellow],
                4 => palette![magenta],
                _ => palette![blue]
            },
        };
        prev_byte_index = s.end_byte_index;
        output
    }).collect()
}

fn text_box<'view>(
    ui: &mut ui::State,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    outer_rect: ScreenSpaceRect,
    padding: Spacing,
    char_dim: CharDim,
    size: f32,
    text_color: TextBoxColour,
    BufferViewData {
        highlights,
        cursors,
        scroll,
        chars,
        spans,
        ..
    }: &'view BufferViewData,
    buffer_id: BufferId,
    z: u16,
    current_buffer_id: &BufferId,
) -> Option<Input> {
    let mut input = None;

    let (clicked, button_state) = ui::do_button_logic(ui, ui_id!(format!("{:?}", buffer_id)), outer_rect);
    if clicked {
        input = Some(Input::SelectBuffer(buffer_id));
    }

    let (color, cursor_alpha) = if *current_buffer_id == buffer_id {
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

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: outer_rect,
        color,
        z: z.saturating_sub(1),
    }));

    let scroll = *scroll;
    let text_box_pos = TextBoxXY {
        x: outer_rect.min.0,
        y: outer_rect.min.1,
    };
    let scroll_offset = text_box_to_screen(
        text_to_text_box(TextSpaceXY::default(), scroll),
        text_box_pos,
    );
    let offset_text_rect = shrink_by(ssr!(scroll_offset.into(), outer_rect.max), padding);
    
    text_or_rects.push(TextOrRect::MulticolourText(MulticolourTextSpec {
        text: match text_color {
            TextBoxColour::FromSpans => colourize(&chars, spans),
            TextBoxColour::Single(color) => vec![ColouredText{ color, text: &chars }],
        },
        size,
        layout: TextLayout::Unbounded, //WrapInRect(outer_rect),
        rect: offset_text_rect,
        z,
    }));

    for c in cursors.iter() {
        let screen_xy = position_to_screen_space(c.position, char_dim, scroll, text_box_pos);
        let cursor_rect = shrink_by(ssr!(screen_xy.into(), outer_rect.max), padding);
        text_or_rects.push(TextOrRect::Text(TextSpec {
            text: "▏",
            size,
            layout: TextLayout::WrapInRect(outer_rect),
            spec: VisualSpec {
                rect: cursor_rect,
                color: match c.state {
                    CursorState::None => palette![red, cursor_alpha],
                    CursorState::PressedAgainstWall(_) => palette![yellow, cursor_alpha],
                },
                z: z.saturating_add(3),
            },
        }));
    }

    let (x, y) = offset_text_rect.min;
    let CharDim { w, h } = char_dim;
    text_or_rects.extend(
        highlights
            .iter()
            .filter_map(|Highlight { min, max, kind, .. }| {
                let mut rect = ssr!(
                    min.offset.0 as f32 * w + x,
                    min.line as f32 * h + y,
                    max.offset.0 as f32 * w + x,
                    (max.line + 1) as f32 * h + y
                );

                clamp_within(&mut rect, outer_rect);

                if rect.has_any_area() {
                    Some(TextOrRect::Rect(VisualSpec {
                        rect,
                        color: highlight_kind_colour(*kind),
                        z: z.saturating_add(4),
                    }))
                } else {
                    None
                }
            }),
    );

    input
}

pub fn make_active_tab_visible<'view>(
    ui: &mut ui::State,
    view: &'view View,
    Dimensions {
        font: FontInfo { tab_char_dim, .. },
        window: sswh!(window_width, _h)
    }: Dimensions,
) -> Option<()> {
    let target_index_or_max = view.get_visible_index_or_max();
    let tab_count = view.buffers.len();
    let tab_layout = get_tab_spaced_rect(&ui, tab_char_dim, 0, tab_count, window_width);
    let tab_width = tab_layout.width();

    make_nth_tab_visible_if_present(ui, target_index_or_max, tab_count, tab_width);

    Some(())
}

fn make_nth_tab_visible_if_present(
    ui: &mut ui::State,
    target_index: usize,
    tab_count: usize,
    tab_width: f32,
) {
    if target_index < tab_count {
        ui.tab_scroll = -(target_index as f32 * tab_width);
    }
}
struct LineSpec {
    colour: Colour,
    thickness: f32,
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
    char_dim: CharDim { w: 16.0, h: 16.0 },
    layout: TextLayout::SingleLine,
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
    use ButtonState::*;

    const BACKGROUND_COLOUR: Colour = TAB_BACKGROUND_COLOUR;
    const TEXT_COLOUR: Colour = TAB_TEXT_COLOUR;

    let text_z = z.saturating_add(3);
    let overline_z = z.saturating_add(2);
    let underline_z = z.saturating_add(1);
    //z is the rect z
    let outline_z = z.saturating_sub(1);

    macro_rules! highlight_colour {
        ($input_type: expr) => {{
            use ui::InputType::*;
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
                    color: TEXT_COLOUR,
                    z: text_z,
                },
            }));
        };
    }

    match state {
        Pressed(input_type) => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                color: highlight_colour!(input_type),
                z,
            }));
            push_text!();
        }
        Hover(input_type) => {
            // outline
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect: enlarge_by(rect, margin),
                color: highlight_colour!(input_type),
                z: outline_z,
            }));

            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                color: BACKGROUND_COLOUR,
                z,
            }));
            push_text!();
        }
        Usual => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect,
                color: BACKGROUND_COLOUR,
                z,
            }));
            push_text!();
        }
    }

    if let Some(LineSpec { colour, thickness }) = overline {
        text_or_rects.push(TextOrRect::Rect(VisualSpec {
            rect: rect.with_max_y(rect.min.1 + thickness),
            color: colour,
            z: overline_z,
        }));
    }

    if let Some(LineSpec { colour, thickness }) = underline {
        text_or_rects.push(TextOrRect::Rect(VisualSpec {
            rect: rect.with_min_y(rect.max.1 - thickness),
            color: colour,
            z: underline_z,
        }));
    }
}

pub const TEXT_SIZE: f32 = 32.0;
pub const FIND_REPLACE_SIZE: f32 = 26.0;
pub const STATUS_SIZE: f32 = 22.0;
pub const TAB_SIZE: f32 = 16.0;

pub const SEPARATOR_LINE_THICKNESS: f32 = 2.0;

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
const TAB_MARGIN_RATIO: f32 = 1.0 / 32.0;
const TAB_PADDING_RATIO: f32 = 1.0 / 64.0;
const TAB_MIN_W: f32 = 128.0;
const TAB_MIN_PADDING: f32 = TAB_MIN_W * TAB_PADDING_RATIO;
const TAB_MIN_MARGIN: f32 = TAB_MIN_W * TAB_MARGIN_RATIO;

#[derive(Clone, Copy)]
pub enum Spacing {
    All(f32),
    Horizontal(f32),
    Vertical(f32),
    Axis(f32, f32),
    LeftTopRightBottom(f32, f32, f32, f32),
}
d!(for Spacing: Spacing::All(0.0));

/// LRTB is short for `LeftTopRightBottom`. This represents what the values of a spacing would be
/// if the spacing was the `LeftTopRightBottom` variant.
struct LRTB {
    l: f32,
    r: f32,
    t: f32,
    b: f32,
}

impl Spacing {
    fn into_ltrb(self) -> LRTB {
        use Spacing::*;
        let (l, t, r, b) = match self {
            All(n) => (n, n, n, n),
            Horizontal(n) => (n, 0.0, n, 0.0),
            Vertical(n) => (0.0, n, 0.0, n),
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
    fn width(&self) -> f32 {
        enlarge_by(self.rect, self.margin).width()
    }
}

fn get_tab_spaced_rect(
    ui: &ui::State,
    tab_char_dim: CharDim,
    tab_index: usize,
    tab_count: usize,
    width: f32,
) -> SpacedRect {
    let UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        edit_y
    } = upper_position_info(&tab_char_dim);
    let tab_count: f32 = usize_to_f32_or_65536(max(tab_count, 1));
    let tab_w = width / tab_count;
    let tab_w = if tab_w > TAB_MIN_W {
        tab_w
    } else {
        // NaN ends up here
        TAB_MIN_W
    };
    let tab_padding = tab_w * TAB_PADDING_RATIO;
    let tab_margin = tab_w * TAB_MARGIN_RATIO;

    let min_x = usize_to_f32_or_65536(tab_index) * tab_w + tab_padding + ui.tab_scroll;
    let max_x = usize_to_f32_or_65536(tab_index + 1) * tab_w - tab_padding + ui.tab_scroll;

    SpacedRect {
        padding: Spacing::Axis(tab_padding, tab_v_padding),
        margin: Spacing::Axis(tab_margin, tab_v_margin),
        rect: ssr!((min_x, tab_y), (max_x, edit_y - tab_y)),
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
    let text_w = usize_to_f32_or_65536(text.chars().count()) * char_dim.w;
    
    center_within((text_w, char_dim.h), rect)
}

/// returns a rectangle with the passed width and height centered inside the passed rectangle.
fn center_within((w, h): (f32, f32), rect: ScreenSpaceRect) -> ScreenSpaceRect {
    let (middle_x, middle_y) = rect.middle();
    let min = (middle_x - (w / 2.0), middle_y - (h / 2.0));
    ssr!(min, (min.0 + w, min.1 + h))
}

fn usize_to_f32_or_65536(n: usize) -> f32 {
    use std::convert::TryFrom;
    u16::try_from(n).unwrap_or(u16::max_value()).into()
}

struct UpperPositionInfo {
    tab_v_padding: f32,
    tab_v_margin: f32,
    tab_y: f32,
    edit_y: f32,
}

fn upper_position_info(tab_char_dim: &CharDim) -> UpperPositionInfo {
    let tab_v_padding = TAB_MIN_PADDING;
    let tab_v_margin = TAB_MIN_MARGIN;
    let tab_y = tab_v_margin;
    let edit_y = tab_y + tab_v_padding + tab_char_dim.h + tab_v_padding + tab_y;

    UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        edit_y,
    }
}

/// A specification for spacing aorund something, contiaing values suitable for passing to `Spacing::All`.
struct SpacingAllSpec {
    margin: f32,
    padding: f32,
}

fn get_menu_spacing(height: f32) -> SpacingAllSpec {
    /// Ratios to screen height
    const MARGIN_RATIO: f32 = 1.0 / 16.0;
    const PADDING_RATIO: f32 = 1.0 / 32.0;

    const MIN_MARGIN: f32 = MARGIN_RATIO * 256.0;
    const MIN_PADDING: f32 = PADDING_RATIO * 256.0;

    let mut margin = MARGIN_RATIO * height;
    margin = if margin > MIN_MARGIN {
        margin
    } else {
        //NaN ends up here
        MIN_MARGIN
    };
    let mut padding = PADDING_RATIO * height;
    padding = if padding > MIN_PADDING {
        padding
    } else {
        //NaN ends up here
        MIN_PADDING
    };

    SpacingAllSpec {
        margin,
        padding
    }
}

pub struct FindReplaceInfo {
    pub margin: Spacing,
    pub padding: Spacing,
    pub top_y: f32,
    pub bottom_y: f32,
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
    // assuming that there are two text buffers and a heading, each with the same margin and
    //padding, without the margins being duplicated
    let top_y = bottom_y - (margin * 4.0 + padding * 6.0 + find_replace_char_dim.h * 3.0);

    let mut current_y = top_y + margin;
    let text_height = 2.0 * padding + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - 2.0 * (margin + $h_padding),
                text_height - 2.0 * padding
            )
        };
    }

    let label_rect = text_rect!(0.0).into();

    current_y += text_height + margin;
    let find_outer_rect = ssr!(
        (margin, current_y),
        (width - margin, current_y + text_height)
    );
    let find_text_xywh = text_rect!();

    current_y += text_height + margin;
    let replace_outer_rect = ssr!(
        (margin, current_y),
        (width - margin, current_y + text_height)
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
    pub list_padding: Spacing,
    pub top_y: f32,
    pub bottom_y: f32,
    pub label_rect: ScreenSpaceRect,
    pub search_outer_rect: ScreenSpaceRect,
    pub search_text_xywh: TextBoxXYWH,
}

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
    let text_height = 2.0 * padding + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - 2.0 * (margin + $h_padding),
                text_height - 2.0 * padding
            )
        };
    }

    let label_rect = text_rect!(0.0).into();

    current_y += text_height + margin;
    let search_outer_rect = ssr!(
        (margin, current_y),
        (width - margin, current_y + text_height)
    );
    let search_text_xywh = text_rect!();

    const LIST_MARGIN_TO_PADDING_RATIO: f32 = 1.0 / 8.0;

    FileSwitcherInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        list_margin: Spacing::All(margin * LIST_MARGIN_TO_PADDING_RATIO),
        list_padding: Spacing::All(margin * (1.0 - LIST_MARGIN_TO_PADDING_RATIO)),
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
    pub top_y: f32,
    pub bottom_y: f32,
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
    let text_height = 2.0 * padding + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - 2.0 * (margin + $h_padding),
                text_height - 2.0 * padding
            )
        };
    }

    let label_rect = text_rect!(0.0).into();

    current_y += text_height + margin;
    let input_outer_rect = ssr!(
        (margin, current_y),
        (width - margin, current_y + text_height)
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
    pub top_y: f32,
    pub bottom_y: f32,
    pub label_rect: ScreenSpaceRect,
}

pub fn get_command_menu_info(
    Dimensions {
        font: FontInfo {
            find_replace_char_dim,
            tab_char_dim,
            ..
        },
        window: sswh!(width, height),
    }: Dimensions,
) -> CommandMenuInfo {
    let SpacingAllSpec { margin, padding } = get_menu_spacing(height);

    let top_y = upper_position_info(&tab_char_dim).edit_y;

    let mut current_y = top_y + margin;
    let text_height = 2.0 * padding + find_replace_char_dim.h;

    macro_rules! text_rect {
        () => {
            text_rect!(padding)
        };
        ($h_padding: expr) => {
            tbxywh!(
                margin + $h_padding,
                current_y + padding,
                width - 2.0 * (margin + $h_padding),
                text_height - 2.0 * padding
            )
        };
    }

    let label_rect = text_rect!(0.0).into();

    current_y += text_height + margin;

    let bottom_y = current_y + text_height + padding + margin;

    CommandMenuInfo {
        margin: Spacing::All(margin),
        padding: Spacing::All(padding),
        top_y,
        bottom_y,
        label_rect,
    }
}

fn get_status_line_y(status_char_dim: CharDim, height: f32) -> f32 {
    height - (status_char_dim.h + 2.0 * SEPARATOR_LINE_THICKNESS)
}

pub fn get_edit_buffer_xywh(
    mode: MenuMode,
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
    let max_y = match mode {
        MenuMode::Hidden | MenuMode::GoToPosition => get_status_line_y(status_char_dim, height),
        MenuMode::FindReplace(_) => get_find_replace_info(dimensions).top_y,
        MenuMode::FileSwitcher => height,
    };
    let y = upper_position_info(tab_char_dim).edit_y;
    TextBoxXYWH {
        xy: TextBoxXY { x: 0.0, y },
        wh: ScreenSpaceWH {
            w: width,
            h: max_y - y,
        },
    }
}

pub fn get_current_buffer_rect(
    current_buffer_id: BufferId,
    mode: MenuMode,
    dimensions: Dimensions,
) -> TextBoxXYWH {
    use BufferIdKind::*;
    match current_buffer_id.kind {
        None => d!(),
        Text => get_edit_buffer_xywh(mode, dimensions),
        Find => get_find_replace_info(dimensions).find_text_xywh,
        Replace => get_find_replace_info(dimensions).replace_text_xywh,
        FileSwitcher => get_file_switcher_info(dimensions).search_text_xywh,
        GoToPosition => get_go_to_position_info(dimensions).input_text_xywh,
    }
}

/// This function determines whether the mouse cursor should use the text selection icon ot not.
pub fn should_show_text_cursor(
    xy: ScreenSpaceXY,
    mode: MenuMode,
    dimensions: Dimensions,
) -> bool {
    let inside_edit_buffer = inside_rect(xy, get_edit_buffer_xywh(mode, dimensions).into());
    inside_edit_buffer || match mode {
        MenuMode::Hidden => false,
        MenuMode::FindReplace(_) => {
            let FindReplaceInfo {
                find_outer_rect,
                replace_outer_rect,
                ..
            } = get_find_replace_info(dimensions);

            inside_rect(xy, find_outer_rect.into())
            || inside_rect(xy, replace_outer_rect.into())
        }
        MenuMode::FileSwitcher => {
            let FileSwitcherInfo {
                search_outer_rect,
                ..
            } = get_file_switcher_info(dimensions);

            inside_rect(xy, search_outer_rect.into())
        }
        MenuMode::GoToPosition => {
            let GoToPositionInfo {
                input_outer_rect,
                ..
            } = get_go_to_position_info(dimensions);

            inside_rect(xy, input_outer_rect.into())
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
