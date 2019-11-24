use gl_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use macros::{c, d, fmt_debug, ord};
use platform_types::{screen_positioning::*, *};
use std::cmp::max;

// This is probably excessive. We can make this smaller if there is a measuarable perf impact
// but given this goes on the stack, that seems unlikely?
#[derive(Clone, Copy)]
pub struct UIId([u64; 32]);
fmt_debug!(for UIId: UIId(id) in "{}", {
    let mut s = String::with_capacity(32 * 4);
    'outer: for n in id.iter() {
        let bytes = n.to_be_bytes();
        for &byte in bytes.iter() {
            if byte == 0 {
                break 'outer;
            }
            s.push(byte as char);
        }
    }
    s
});
ord!(and friends for UIId: id, other in {
    id.0.cmp(&other.0)
});

impl UIId {
    pub const fn new(id: [u64; 32]) -> Self {
        UIId(id)
    }
}

const UI_ID_ZERO: UIId = UIId::new([0; 32]);

/// This macro creates a UIId based on the expression passed in and the location of the invocation
/// in the file. This implies it may assign the same id to multiple `id` invocations inside another
/// macro. A suggested fix for that is to pass down the needed ids from outside that macro.
macro_rules! id {
    ($thing: expr) => {{
        let mut id = [0; 32];
        // TODO is the compilier smart enough to avoid the allocation here?
        let s = format!(
            "{},{}",
            $thing,
            concat!(column!(), ",", line!(), ",", file!())
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

        // use new so we can use this macro outside this package.
        UIId::new(id)
    }};
}

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
const TEXT_COLOUR: Colour = palette![blue];

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
const TAB_HIGHLIGHT_COLOUR: Colour = palette![yellow];

const TEXT_SIZE: f32 = 60.0;
const FIND_REPLACE_SIZE: f32 = 26.0;
const STATUS_SIZE: f32 = 22.0;
const TAB_SIZE: f32 = 16.0;

const SEPARATOR_LINE_THICKNESS: f32 = 2.0;

pub const TEXT_SIZES: [f32; 4] = [TEXT_SIZE, STATUS_SIZE, TAB_SIZE, FIND_REPLACE_SIZE];

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
const EDIT_Z: u16 = z_from_base(32);
const FIND_REPLACE_BACKGROUND_Z: u16 = z_from_base(32 + 8);
const FIND_REPLACE_Z: u16 = z_from_base(32 + 16);
const STATUS_BACKGROUND_Z: u16 = z_from_base(64);
const TAB_BACKGROUND_Z: u16 = STATUS_BACKGROUND_Z;
const STATUS_Z: u16 = z_from_base(128);
const TAB_Z: u16 = STATUS_Z;

/// Ratios to tab width
const TAB_MARGIN_RATIO: f32 = 1.0 / 128.0;
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
    ui: &UIState,
    tab_char_dim: CharDim,
    tab_index: usize,
    tab_count: usize,
    width: f32,
) -> SpacedRect {
    let UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        ..
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
        rect: ssr!((min_x, tab_y), (max_x, tab_y + tab_char_dim.h)),
    }
}

pub fn view<'view>(
    ui: &mut UIState,
    view: &'view View,
    font_info: &FontInfo,
    wh: ScreenSpaceWH,
) -> (Vec<TextOrRect<'view>>, Option<Input>) {
    let sswh!(width, height) = wh;
    let FontInfo {
        text_char_dim,
        status_char_dim,
        tab_char_dim,
        find_replace_char_dim,
        ..
    } = font_info;
    const PER_BUFFER_TEXT_OR_RECT_ESTIMATE: usize =
        4   // 2-4 per tab, usually 2
    ;
    let mut text_or_rects =
        Vec::with_capacity(view.buffers.len() * PER_BUFFER_TEXT_OR_RECT_ESTIMATE);
    let mut input = None;

    let edit_buffer_text_rect = get_edit_buffer_xywh(view.menu.get_mode(), *font_info, wh);

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: ssr!(0.0, 0.0, width, edit_buffer_text_rect.xy.y),
        color: TAB_BAR_BACKGROUND_COLOUR,
        z: TAB_BACKGROUND_Z,
    }));

    let visible_index_or_max = view.get_visible_index_or_max();
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
            id!(i),
            &mut text_or_rects,
            OutlineButtonSpec {
                text: &name_string,
                size: TAB_SIZE,
                char_dim: *tab_char_dim,
                layout: TextLayout::SingleLine,
                margin,
                rect,
                background_colour: TAB_BACKGROUND_COLOUR,
                text_colour: TAB_TEXT_COLOUR,
                highlight_colour: TAB_HIGHLIGHT_COLOUR,
                extra_highlight: if i == visible_index_or_max {
                    ExtraHighlight::Underline(TEXT_COLOUR, padding.into_ltrb().b)
                } else {
                    ExtraHighlight::None
                },
                z: TAB_Z,
            },
        ) {
            input = Some(Input::SelectBuffer(b_id!(
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

        let edit_buffer_text_rect: ScreenSpaceRect = edit_buffer_text_rect.into();
        input = text_box(
            ui,
            &mut text_or_rects,
            edit_buffer_text_rect,
            d!(),
            *text_char_dim,
            TEXT_SIZE,
            TEXT_COLOUR,
            &data,
            b_id!(BufferIdKind::Text, index),
            EDIT_Z,
            &view.current_buffer_id,
        )
        .or(input);

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
                } = get_find_replace_info(*font_info, wh);
                match mode {
                    FindReplaceMode::CurrentFile => {
                        let outer_rect = ScreenSpaceRect {
                            min: (0.0, top_y),
                            max: (width, bottom_y),
                        };
                        text_or_rects.push(TextOrRect::Rect(VisualSpec {
                            rect: outer_rect,
                            color: CHROME_BACKGROUND_COLOUR,
                            z: FIND_REPLACE_BACKGROUND_Z,
                        }));
                        text_or_rects.push(TextOrRect::Text(TextSpec {
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
                                input = text_box(
                                    ui,
                                    &mut text_or_rects,
                                    $outer_rect,
                                    padding,
                                    *find_replace_char_dim,
                                    FIND_REPLACE_SIZE,
                                    CHROME_TEXT_COLOUR,
                                    $data,
                                    $input,
                                    FIND_REPLACE_Z,
                                    &view.current_buffer_id,
                                )
                                .or(input);
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
            MenuView::FileSwitcher(FileSwitcherView { search, results }) => {
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
                } = get_file_switcher_info(*font_info, wh);
                let outer_rect = ScreenSpaceRect {
                    min: (0.0, top_y),
                    max: (width, bottom_y),
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
                macro_rules! spaced_input_box {
                    ($data: expr, $input: expr, $outer_rect: expr) => {{
                        input = text_box(
                            ui,
                            &mut text_or_rects,
                            $outer_rect,
                            padding,
                            *find_replace_char_dim,
                            FIND_REPLACE_SIZE,
                            CHROME_TEXT_COLOUR,
                            $data,
                            $input,
                            FIND_REPLACE_Z,
                            &view.current_buffer_id,
                        )
                        .or(input);
                    }};
                }

                let mut current_rect = search_outer_rect;
                let list_bottom_margin = list_margin.into_ltrb().b;
                let vertical_shift =
                    search_text_xywh.wh.h + margin.into_ltrb().b - list_bottom_margin;

                spaced_input_box!(
                    search,
                    b_id!(BufferIdKind::FileSwitcher, index),
                    current_rect
                );
                // We add the extra `list_bottom_margin` here but not in the loop so that the
                // spacing bewteen the textbox and the first button is the same as the spacing
                // between subsequent buttons. If we added it in both there would be a double
                // magin between buttons.
                current_rect.min.1 += vertical_shift;
                current_rect.min.1 += list_bottom_margin;
                current_rect.max.1 += vertical_shift;
                current_rect.max.1 += list_bottom_margin;

                for (i, result) in results.iter().enumerate() {
                    let path_text = result.to_str().unwrap_or("Non-UTF8 Path");
                    let rect = enlarge_by(shrink_by(current_rect, margin), list_padding);
                    if do_outline_button(
                        ui,
                        id!(i),
                        &mut text_or_rects,
                        OutlineButtonSpec {
                            text: &path_text,
                            size: TAB_SIZE,
                            char_dim: *tab_char_dim,
                            layout: TextLayout::SingleLine,
                            margin: list_margin,
                            rect,
                            background_colour: TAB_BACKGROUND_COLOUR,
                            text_colour: TAB_TEXT_COLOUR,
                            highlight_colour: TAB_HIGHLIGHT_COLOUR,
                            extra_highlight: ExtraHighlight::None,
                            z: TAB_Z,
                        },
                    ) {
                        input = Some(Input::OpenOrSelectBuffer(result.to_owned()));
                    }
                    current_rect.min.1 += vertical_shift;
                    current_rect.max.1 += vertical_shift;
                }
            }
        }
    }

    //
    //    Status line
    //

    let status_line_y = get_status_line_y(*status_char_dim, height);

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
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
            rect: rect.with_min_y(status_line_y + 2.0 * SEPARATOR_LINE_THICKNESS),
            color: CHROME_TEXT_COLOUR,
            z: STATUS_Z,
        },
    }));

    (text_or_rects, input)
}

fn text_box<'view>(
    ui: &mut UIState,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    outer_rect: ScreenSpaceRect,
    padding: Spacing,
    char_dim: CharDim,
    size: f32,
    // TODO if we need more colour customization we should probably make a struct for it
    text_color: Colour,
    BufferViewData {
        highlights,
        cursors,
        scroll,
        chars,
    }: &'view BufferViewData,
    buffer_id: BufferId,
    z: u16,
    current_buffer_id: &BufferId,
) -> Option<Input> {
    let mut input = None;

    let (clicked, button_state) = do_button_logic(ui, id!(format!("{:?}", buffer_id)), outer_rect);
    if clicked {
        input = Some(Input::SelectBuffer(buffer_id));
    }

    let color = if *current_buffer_id == buffer_id {
        TEXT_BACKGROUND_COLOUR
    } else {
        match button_state {
            ButtonState::Usual => TEXT_BACKGROUND_COLOUR,
            ButtonState::Hover => TEXT_HOVER_BACKGROUND_COLOUR,
            ButtonState::Pressed => TEXT_PRESSED_BACKGROUND_COLOUR,
        }
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
    text_or_rects.push(TextOrRect::Text(TextSpec {
        text: &chars,
        size,
        layout: TextLayout::Unbounded, //WrapInRect(outer_rect),
        spec: VisualSpec {
            rect: offset_text_rect,
            color: text_color,
            z,
        },
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
                    CursorState::None => c![0.9, 0.3, 0.3],
                    CursorState::PressedAgainstWall => c![0.9, 0.9, 0.3],
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
    ui: &mut UIState,
    view: &'view View,
    FontInfo { tab_char_dim, .. }: &FontInfo,
    (screen_width, _): (f32, f32),
) -> Option<()> {
    let target_index_or_max = view.get_visible_index_or_max();
    let tab_count = view.buffers.len();
    let tab_layout = get_tab_spaced_rect(&ui, *tab_char_dim, 0, tab_count, screen_width);
    let tab_width = tab_layout.width();

    make_nth_tab_visible_if_present(ui, target_index_or_max, tab_count, tab_width);

    Some(())
}

fn make_nth_tab_visible_if_present(
    ui: &mut UIState,
    target_index: usize,
    tab_count: usize,
    tab_width: f32,
) {
    if target_index < tab_count {
        ui.tab_scroll = -(target_index as f32 * tab_width);
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PhysicalButtonState {
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

    pub fn is_pressed(&self) -> bool {
        match *self {
            Self::ReleasedThisFrame | Self::Released => false,
            Self::PressedThisFrame | Self::Pressed => true,
        }
    }
}

#[derive(Debug)]
pub struct UIState {
    pub mouse_pos: ScreenSpaceXY,
    pub left_mouse_state: PhysicalButtonState,
    pub tab_scroll: f32,
    active: UIId,
    hot: UIId,
    next_hot: UIId,
}

d!(for UIState: UIState {
    mouse_pos: d!(),
    left_mouse_state: d!(),
    tab_scroll: d!(),
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

fn clamp_within(rect: &mut ScreenSpaceRect, ScreenSpaceRect { min, max }: ScreenSpaceRect) {
    if rect.min.0 < min.0 {
        rect.min.0 = min.0
    } else {
        // NaN ends up here
    };
    if rect.min.1 < min.1 {
        rect.min.1 = min.1
    } else {
        // NaN ends up here
    };

    if rect.max.0 > max.0 {
        rect.max.0 = max.0
    } else {
        // NaN ends up here
    };
    if rect.max.1 > max.1 {
        rect.max.1 = max.1
    } else {
        // NaN ends up here
    };
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
            ui.set_not_active();
        }
    } else if ui.hot == id {
        if mouse_state == PhysicalButtonState::PressedThisFrame {
            ui.set_active(id);
        }
    }

    if inside {
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

enum ExtraHighlight {
    None,
    /// The f32 is height
    Underline(Colour, f32),
}
d!(for ExtraHighlight: ExtraHighlight::None);

struct OutlineButtonSpec<'text> {
    text: &'text str,
    size: f32,
    char_dim: CharDim,
    layout: TextLayout,
    margin: Spacing,
    rect: ScreenSpaceRect,
    background_colour: Colour,
    text_colour: Colour,
    highlight_colour: Colour,
    extra_highlight: ExtraHighlight,
    z: u16,
}
d!(for OutlineButtonSpec<'static>: OutlineButtonSpec {
    text: "OutlineButtonSpec default",
    size: 16.0,
    char_dim: CharDim { w: 16.0, h: 16.0 },
    layout: TextLayout::SingleLine,
    margin: d!(),
    rect: d!(),
    background_colour: c![1.0, 0.0, 0.0],
    text_colour: c![0.0, 1.0, 0.0],
    highlight_colour: c![0.0, 0.0, 1.0],
    extra_highlight: d!(),
    z: gl_layer::DEFAULT_Z,
});

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

fn do_outline_button<'view>(
    ui: &mut UIState,
    id: UIId,
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    spec: OutlineButtonSpec<'view>,
) -> bool {
    let (clicked, state) = do_button_logic(ui, id, spec.rect);

    render_outline_button(text_or_rects, spec, state);

    clicked
}

/// returns a rectangle with the passed width and height centered inside the passed rectangle.
fn center_within((w, h): (f32, f32), rect: ScreenSpaceRect) -> ScreenSpaceRect {
    let (middle_x, middle_y) = rect.middle();
    let min = (middle_x - (w / 2.0), middle_y - (h / 2.0));
    ssr!(min, (min.0 + w, min.1 + h))
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
        background_colour,
        text_colour,
        highlight_colour,
        extra_highlight,
        z,
    }: OutlineButtonSpec<'view>,
    state: ButtonState,
) {
    use ButtonState::*;

    let text_w = usize_to_f32_or_65536(text.chars().count()) * char_dim.w;
    let text_rect = center_within((text_w, char_dim.h), rect);

    macro_rules! push_text {
        () => {
            text_or_rects.push(TextOrRect::Text(TextSpec {
                text,
                size,
                layout,
                spec: VisualSpec {
                    rect: text_rect,
                    color: text_colour,
                    z: z.saturating_add(1),
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
                z: z.saturating_sub(1),
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

    match extra_highlight {
        ExtraHighlight::Underline(color, height) => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect: rect.with_min_y(rect.max.1 - height),
                color,
                z: z.saturating_add(2),
            }));
        }
        _ => {}
    }
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
    let tab_y = tab_v_margin + tab_v_padding;
    let edit_y = tab_y + tab_char_dim.h + tab_v_padding + tab_v_margin;

    UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        edit_y,
    }
}

/// Ratios to screen height
const FIND_REPLACE_MARGIN_RATIO: f32 = 1.0 / 16.0;
const FIND_REPLACE_PADDING_RATIO: f32 = 1.0 / 32.0;

const FIND_REPLACE_MIN_MARGIN: f32 = FIND_REPLACE_MARGIN_RATIO * 256.0;
const FIND_REPLACE_MIN_PADDING: f32 = FIND_REPLACE_PADDING_RATIO * 256.0;

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
    FontInfo {
        status_char_dim,
        find_replace_char_dim,
        ..
    }: FontInfo,
    sswh!(width, height): ScreenSpaceWH,
) -> FindReplaceInfo {
    let mut margin = FIND_REPLACE_MARGIN_RATIO * height;
    margin = if margin > FIND_REPLACE_MIN_MARGIN {
        margin
    } else {
        //NaN ends up here
        FIND_REPLACE_MIN_MARGIN
    };
    let mut padding = FIND_REPLACE_PADDING_RATIO * height;
    padding = if padding > FIND_REPLACE_MIN_PADDING {
        padding
    } else {
        //NaN ends up here
        FIND_REPLACE_MIN_PADDING
    };

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
    FontInfo {
        status_char_dim,
        find_replace_char_dim,
        tab_char_dim,
        ..
    }: FontInfo,
    sswh!(width, height): ScreenSpaceWH,
) -> FileSwitcherInfo {
    // TODO should these be separate constants with different values?
    let mut margin = FIND_REPLACE_MARGIN_RATIO * height;
    margin = if margin > FIND_REPLACE_MIN_MARGIN {
        margin
    } else {
        //NaN ends up here
        FIND_REPLACE_MIN_MARGIN
    };
    let mut padding = FIND_REPLACE_PADDING_RATIO * height;
    padding = if padding > FIND_REPLACE_MIN_PADDING {
        padding
    } else {
        //NaN ends up here
        FIND_REPLACE_MIN_PADDING
    };

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

fn get_status_line_y(status_char_dim: CharDim, height: f32) -> f32 {
    height - (status_char_dim.h + 2.0 * SEPARATOR_LINE_THICKNESS)
}

pub fn get_edit_buffer_xywh(mode: MenuMode, font_info: FontInfo, wh: ScreenSpaceWH) -> TextBoxXYWH {
    let sswh!(width, height) = wh;
    let FontInfo {
        status_char_dim,
        ref tab_char_dim,
        ..
    } = font_info;
    let max_y = match mode {
        MenuMode::Hidden => get_status_line_y(status_char_dim, height),
        MenuMode::FindReplace => get_find_replace_info(font_info, wh).top_y,
        MenuMode::FileSwitcher => wh.h,
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
    font_info: FontInfo,
    wh: ScreenSpaceWH,
) -> TextBoxXYWH {
    use BufferIdKind::*;
    match current_buffer_id.kind {
        Text => get_edit_buffer_xywh(mode, font_info, wh),
        Find => get_find_replace_info(font_info, wh).find_text_xywh,
        Replace => get_find_replace_info(font_info, wh).replace_text_xywh,
        FileSwitcher => get_file_switcher_info(font_info, wh).search_text_xywh,
    }
}

pub fn should_show_text_cursor(
    xy: ScreenSpaceXY,
    current_buffer_id: BufferId,
    mode: MenuMode,
    font_info: FontInfo,
    wh: ScreenSpaceWH,
) -> bool {
    match mode {
        MenuMode::Hidden => inside_rect(
            xy,
            get_current_buffer_rect(current_buffer_id, mode, font_info, wh).into(),
        ),
        MenuMode::FindReplace => {
            inside_rect(xy, get_edit_buffer_xywh(mode, font_info, wh).into()) || {
                let FindReplaceInfo {
                    find_outer_rect,
                    replace_outer_rect,
                    ..
                } = get_find_replace_info(font_info, wh);

                inside_rect(xy, find_outer_rect.into())
                    || inside_rect(xy, replace_outer_rect.into())
            }
        }
        MenuMode::FileSwitcher => {
            let FileSwitcherInfo {
                search_outer_rect, ..
            } = get_file_switcher_info(font_info, wh);

            inside_rect(xy, search_outer_rect.into())
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
