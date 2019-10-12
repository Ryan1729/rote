use gl_layer::{TextLayout, TextOrRect, TextSpec, VisualSpec};
use macros::{c, d, fmt_debug, ord};
use platform_types::*;
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

const CHROME_BACKGROUND_COLOUR: Colour = c![7.0 / 256.0, 7.0 / 256.0, 7.0 / 256.0];
const TEXT_COLOUR: Colour = c![0.3, 0.3, 0.9];
const TAB_BACKGROUND_COLOUR: Colour = c![3.0 / 256.0, 3.0 / 256.0, 3.0 / 256.0];

const TEXT_SIZE: f32 = 60.0;
const STATUS_SIZE: f32 = 22.0;
const TAB_SIZE: f32 = 16.0;

pub const TEXT_SIZES: [f32; 3] = [TEXT_SIZE, STATUS_SIZE, TAB_SIZE];

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
const TAB_BACKGROUND_Z: u16 = STATUS_BACKGROUND_Z;
const STATUS_Z: u16 = z_from_base(1 << 4);
const TAB_Z: u16 = STATUS_Z;

const TAB_MARGIN_RATIO: f32 = 1.0 / 128.0;
const TAB_PADDING_RATIO: f32 = 1.0 / 64.0;
const TAB_MIN_W: f32 = 128.0;
const TAB_MIN_PADDING: f32 = TAB_MIN_W * TAB_PADDING_RATIO;
const TAB_MIN_MARGIN: f32 = TAB_MIN_W * TAB_MARGIN_RATIO;

pub fn get_font_info(char_dims: &[CharDim]) -> FontInfo {
    debug_assert!(
        char_dims.len() >= TEXT_SIZES.len(),
        "get_font_info didn't receive enough char_dims"
    );

    FontInfo {
        text_char_dim: char_dims[0],
        status_char_dim: char_dims[1],
        tab_char_dim: char_dims[2],
    }
}

#[derive(Clone, Copy)]
enum Spacing {
    All(f32),
    Horizontal(f32),
    Vertical(f32),
    Axis(f32, f32),
}
d!(for Spacing: Spacing::All(0.0));

impl Spacing {
    /// This rect represents the space that the spacing would take up if it were around nothing.
    fn into_rect(self) -> ScreenSpaceRect {
        use Spacing::*;
        let (min_x, min_y, max_x, max_y) = match self {
            All(n) => (n, n, n, n),
            Horizontal(n) => (n, 0.0, n, 0.0),
            Vertical(n) => (0.0, n, 0.0, n),
            Axis(x, y) => (x, y, x, y),
        };

        ScreenSpaceRect {
            min: (min_x, min_y),
            max: (max_x, max_y),
        }
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
    &UpperPositionInfo {
        tab_v_padding,
        tab_v_margin,
        tab_y,
        ..
    }: &UpperPositionInfo,
    tab_char_dim: CharDim,
    tab_index: usize,
    tab_count: usize,
    width: f32,
) -> SpacedRect {
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
    FontInfo {
        text_char_dim,
        status_char_dim,
        tab_char_dim,
    }: &FontInfo,
    (width, height): (f32, f32),
) -> (Vec<TextOrRect<'view>>, Option<Input>) {
    const PER_BUFFER_TEXT_OR_RECT_ESTIMATE: usize =
        4   // 2-4 per tab, usually 2
    ;
    let mut text_or_rects =
        Vec::with_capacity(view.buffers.len() * PER_BUFFER_TEXT_OR_RECT_ESTIMATE);
    let mut input = None;

    let u_pos_info = upper_position_info(tab_char_dim);
    let edit_y = u_pos_info.edit_y;
    let mouse_in_tab_area = ui.mouse_pos.y < edit_y;

    text_or_rects.push(TextOrRect::Rect(VisualSpec {
        rect: ssr!(0.0, 0.0, width, edit_y),
        color: if mouse_in_tab_area {
            c![
                TAB_BACKGROUND_COLOUR[0],
                TAB_BACKGROUND_COLOUR[1],
                CHROME_BACKGROUND_COLOUR[2]
            ]
        } else {
            TAB_BACKGROUND_COLOUR
        },
        z: TAB_BACKGROUND_Z,
    }));

    let visible_index_or_max = view.visible_buffers[0].unwrap_or(usize::max_value());
    perf_viz::start_record!("for &BufferView");
    let tab_count = view.buffers.len();
    for (i, BufferView { name, .. }) in view.buffers.iter().enumerate() {
        let SpacedRect {
            padding,
            margin,
            rect,
        } = get_tab_spaced_rect(&ui, &u_pos_info, *tab_char_dim, i, tab_count, width);
        if rect.min.0 > width {
            break;
        }
        if do_outline_button(
            ui,
            id!(i),
            &mut text_or_rects,
            OutlineButtonSpec {
                text: &name,
                size: TAB_SIZE,
                char_dim: *tab_char_dim,
                layout: TextLayout::SingleLine,
                padding,
                margin,
                rect,
                background_colour: CHROME_BACKGROUND_COLOUR,
                text_colour: c![0.6, 0.6, 0.6],
                highlight_colour: c![0.6, 0.6, 0.0],
                extra_highlight: if i == visible_index_or_max {
                    ExtraHighlight::Underline(TEXT_COLOUR)
                } else {
                    ExtraHighlight::None
                },
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
                rect: ssr!((x, y)),
                color: TEXT_COLOUR,
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

pub fn make_active_tab_visible<'view>(
    ui: &mut UIState,
    view: &'view View,
    FontInfo { tab_char_dim, .. }: &FontInfo,
    (screen_width, _): (f32, f32),
    buffer_side: usize,
) {
    let target_index_or_max = view.visible_buffers[buffer_side].unwrap_or(usize::max_value());
    let tab_count = view.buffers.len();
    let tab_layout = get_tab_spaced_rect(
        &ui,
        &upper_position_info(tab_char_dim),
        *tab_char_dim,
        0,
        tab_count,
        screen_width,
    );
    let tab_width = tab_layout.width();

    make_nth_tab_visible_if_present(ui, target_index_or_max, tab_count, tab_width, screen_width);
}

fn make_nth_tab_visible_if_present(
    ui: &mut UIState,
    target_index: usize,
    tab_count: usize,
    tab_width: f32,
    screen_width: f32,
) {
    let mut max_x_so_far = 0.0;
    for i in 0..tab_count {
        max_x_so_far += tab_width;

        if i == target_index {
            if max_x_so_far + tab_width > screen_width {
                ui.tab_scroll -= max_x_so_far - (screen_width - tab_width);
            }
            break;
        }
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
    Underline(Colour),
}
d!(for ExtraHighlight: ExtraHighlight::None);

struct OutlineButtonSpec<'text> {
    text: &'text str,
    size: f32,
    char_dim: CharDim,
    layout: TextLayout,
    padding: Spacing,
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
    padding: d!(),
    margin: d!(),
    rect: d!(),
    background_colour: c![1.0, 0.0, 0.0],
    text_colour: c![0.0, 1.0, 0.0],
    highlight_colour: c![0.0, 0.0, 1.0],
    extra_highlight: d!(),
    z: gl_layer::DEFAULT_Z,
});

fn enlarge_by(
    ScreenSpaceRect {
        min: (min_x, min_y),
        max: (max_x, max_y),
    }: ScreenSpaceRect,
    enlarge_amount: Spacing,
) -> ScreenSpaceRect {
    let ScreenSpaceRect {
        min: (min_x_e, min_y_e),
        max: (max_x_e, max_y_e),
    } = enlarge_amount.into_rect();
    ScreenSpaceRect {
        min: (min_x - min_x_e, min_y - min_y_e),
        max: (max_x + max_x_e, max_y + max_y_e),
    }
}

fn shrink_by(
    ScreenSpaceRect {
        min: (min_x, min_y),
        max: (max_x, max_y),
    }: ScreenSpaceRect,
    shrink_amount: Spacing,
) -> ScreenSpaceRect {
    let ScreenSpaceRect {
        min: (min_x_s, min_y_s),
        max: (max_x_s, max_y_s),
    } = shrink_amount.into_rect();
    ScreenSpaceRect {
        min: (min_x + min_x_s, min_y + min_y_s),
        max: (max_x - max_x_s, max_y - max_y_s),
    }
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
    ScreenSpaceRect {
        min,
        max: (min.0 + w, min.1 + h),
    }
}

fn render_outline_button<'view>(
    text_or_rects: &mut Vec<TextOrRect<'view>>,
    OutlineButtonSpec {
        text,
        size,
        char_dim,
        layout,
        padding,
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
    let shrunk_rect = shrink_by(rect, padding);
    let text_rect = center_within((text_w, char_dim.h), shrunk_rect);

    macro_rules! push_text {
        () => {
            text_or_rects.push(TextOrRect::Text(TextSpec {
                text,
                size,
                layout,
                spec: VisualSpec {
                    rect: text_rect,
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

    match extra_highlight {
        ExtraHighlight::Underline(color) => {
            text_or_rects.push(TextOrRect::Rect(VisualSpec {
                rect: ScreenSpaceRect {
                    min: (rect.min.0, shrunk_rect.max.1),
                    ..rect
                },
                color,
                z: z.saturating_sub(2),
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

fn status_line_y(status_char_dim: CharDim, height: f32) -> f32 {
    height - status_char_dim.h
}

pub fn inside_edit_area(
    ScreenSpaceXY { x: _, y }: ScreenSpaceXY,
    FontInfo {
        status_char_dim,
        ref tab_char_dim,
        ..
    }: FontInfo,
    ScreenSpaceWH { w: _, h }: ScreenSpaceWH,
) -> bool {
    y < status_line_y(status_char_dim, h) && y > upper_position_info(tab_char_dim).edit_y
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
mod tests {
    use super::*;

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
        assert_eq!(do_button_logic(&mut ui, id, rect), (false, Usual));
        ui.frame_end();
        ui.frame_init();
        assert_eq!(do_button_logic(&mut ui, id, rect), (false, Hover));
        ui.frame_end();
        ui.left_mouse_state = PhysicalButtonState::PressedThisFrame;
        for i in 0..16 {
            ui.frame_init();
            assert_eq!(
                do_button_logic(&mut ui, id, rect),
                (false, Pressed),
                "iteration {}",
                i
            );
            ui.frame_end();
        }
        ui.left_mouse_state = PhysicalButtonState::ReleasedThisFrame;
        ui.frame_init();
        assert_eq!(do_button_logic(&mut ui, id, rect), (true, Hover));
        ui.frame_end();
        for i in 0..16 {
            ui.frame_init();
            assert_eq!(
                do_button_logic(&mut ui, id, rect),
                (false, Hover),
                "iteration {}",
                i
            );
            ui.frame_end();
        }
    }

    #[test]
    fn render_outline_button_centers_this_example_properly() {
        let mut text_or_rects = Vec::new();

        let x_padding = 4.0;
        let y_padding = 6.0;

        let x_margin = 5.0;
        let y_margin = 7.0;

        let rect = ScreenSpaceRect {
            min: (8.0, 3.0),
            max: (128.0, 17.0),
        };

        let char_dim = CharDim { w: 4.0, h: 8.0 };

        let text = "test";

        let text_length = text.chars().count();

        let spec = OutlineButtonSpec {
            text: "test",
            size: 8.0,
            char_dim,
            padding: Spacing::Axis(x_padding, y_padding),
            margin: Spacing::Axis(x_margin, y_margin),
            rect,
            background_colour: c![0.0, 0.0, 0.0],
            text_colour: c![0.6, 0.6, 0.6],
            highlight_colour: c![0.6, 0.6, 0.0],
            z: TAB_Z,
            ..d!()
        };
        render_outline_button(&mut text_or_rects, spec, ButtonState::Usual);

        let background_rect = text_or_rects
            .iter()
            .find_map(|e| match e {
                TextOrRect::Rect(r) => Some(r),
                _ => None,
            })
            .unwrap();

        let text_spec = text_or_rects
            .iter()
            .find_map(|e| match e {
                TextOrRect::Text(t) => Some(t),
                _ => None,
            })
            .unwrap();

        let b_rect = background_rect.rect;
        let b_middle_x = (b_rect.min.0 + b_rect.max.0) / 2.0;
        let b_middle_y = (b_rect.min.1 + b_rect.max.1) / 2.0;

        let t_rect = text_spec.spec.rect;
        let t_middle_x = (t_rect.min.0 + t_rect.max.0) / 2.0;
        let t_middle_y = (t_rect.min.1 + t_rect.max.1) / 2.0;

        assert_eq!(
            t_middle_x, b_middle_x,
            "t_rect: {:?} b_rect: {:?}",
            t_rect, b_rect
        );
        assert_eq!(
            t_middle_y, b_middle_y,
            "t_rect: {:?} b_rect: {:?}",
            t_rect, b_rect
        );

        let text_w = char_dim.w * text_length as f32;
        assert_eq!(t_rect.width(), text_w);
    }

    #[test]
    fn center_within_centers_this_no_edge_cases_example_properly() {
        let rect = center_within(
            (13.0, 17.0),
            ScreenSpaceRect {
                min: (10.0, 20.0),
                max: (25.0, 40.0),
            },
        );

        assert_eq!(
            rect,
            ScreenSpaceRect {
                min: (11.0, 21.5),
                max: (11.0 + 13.0, 21.5 + 17.0),
            }
        );
    }
}
