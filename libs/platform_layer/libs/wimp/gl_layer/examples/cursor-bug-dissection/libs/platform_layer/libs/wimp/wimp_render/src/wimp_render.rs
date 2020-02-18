use gl_layer::{ColouredText, MulticolourTextSpec, TextLayout, TextOrRect, TextSpec, VisualSpec};
use macros::{c, d, ord};
use platform_types::{screen_positioning::*, *};
use shared::{BufferStatus, BufferStatusMap};
use std::cmp::max;

mod ui_id {
    use macros::{d, fmt_debug, ord};
    /// The varaints here represent sections of code that want to be able to store information in the
    /// ids. For example, so that the ui state can change differently based on which part of soem
    /// dynamically generated UI is selected.
    #[derive(Clone, Copy, Debug)]
    pub enum Tag {
        FileSwitcherResults,
    }

    /// 31 to leave space for the enum variant tag.
    pub const DATA_LEN: usize = 31;

    /// The payload of the `UUId::Data` varaint
    type Data = [u64; DATA_LEN];

    // This is probably excessive size-wise. We can make this smaller if there is a measuarable
    // perf impact but given this goes on the stack, that seems unlikely?
    #[derive(Clone, Copy)]
    pub enum UIId {
        /// The generic data variant. Used when the data's sizes are not known ahead of time
        Data(Data),
        TaggedUsize(Tag, usize),
    }
    d!(for UIId: UIId::Data(d!()));

    fmt_debug!(for UIId: id in "{}", {
        match id {
            UIId::Data(data) => {
                let mut s = String::with_capacity(31 * std::mem::size_of::<u64>());

                'outer: for n in data.iter() {
                    let bytes = n.to_be_bytes();
                    for &byte in bytes.iter() {
                        if byte == 0 {
                            break 'outer;
                        }
                        s.push(byte as char);
                    }
                }

                s
            },
            UIId::TaggedUsize(tag, payload) => {
                format!("TaggedUsize{:?}", (tag, payload))
            }
        }
    });
    ord!(and friends for UIId: id, other in {
        use UIId::*;
        use std::cmp::Ordering::*;
        match (id, other) {
            (Data(_), TaggedUsize(_, _)) => {
                Less
            },
            (TaggedUsize(_, _), Data(_)) => {
                Greater
            },
            (Data(d1), Data(d2)) => {
                d1.cmp(&d2)
            }
            (TaggedUsize(Tag::FileSwitcherResults, payload1), TaggedUsize(Tag::FileSwitcherResults, payload2)) => {
                payload1.cmp(&payload2)
            }
        }
    });

    impl UIId {
        pub const fn new(id: Data) -> Self {
            UIId::Data(id)
        }
    }

    /// This macro creates a UIId based on the expression passed in and the location of the invocation
    /// in the file. This implies it may assign the same id to multiple `id` invocations inside another
    /// macro. A suggested fix for that is to pass down the needed ids from outside that macro.
    #[macro_export]
    macro_rules! id {
        ($thing: expr) => {{
            let mut id = [0; ui_id::DATA_LEN];
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
}
use ui_id::UIId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Navigation {
    None,
    Up,
    Down,
    Interact,
}
d!(for Navigation: Navigation::None);

fn navigation_from_cursors(cursors: &Vec<CursorView>) -> Navigation {
    let mut output = d!();

    for c in cursors.iter() {
        match c.state {
            CursorState::None => {}
            CursorState::PressedAgainstWall(dir) => match dir {
                Move::Up => {
                    output = Navigation::Up;
                    break;
                }
                Move::Down => {
                    output = Navigation::Down;
                    break;
                }
                _ => {}
            },
        }
    }

    output
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

const TEXT_SIZE: f32 = 32.0;
const FIND_REPLACE_SIZE: f32 = 26.0;
const STATUS_SIZE: f32 = 22.0;
const TAB_SIZE: f32 = 16.0;

const SEPARATOR_LINE_THICKNESS: f32 = 2.0;

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
const EDIT_Z: u16 = z_from_base(32);
const FIND_REPLACE_BACKGROUND_Z: u16 = z_from_base(32 + 8);
const FIND_REPLACE_Z: u16 = z_from_base(32 + 16);
const STATUS_BACKGROUND_Z: u16 = z_from_base(64);
const TAB_BACKGROUND_Z: u16 = STATUS_BACKGROUND_Z;
const STATUS_Z: u16 = z_from_base(128);
const TAB_Z: u16 = STATUS_Z;

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

pub fn view<'view>(
    ui: &mut UIState,
    view: &'view View,
    font_info: &FontInfo,
    wh: ScreenSpaceWH,
    dt: std::time::Duration,
) -> (Vec<TextOrRect<'view>>, Option<Input>) {
        let offset = ((dt.as_millis() as u64 as f32) / 1000.0) * 1.5;

        ui.fade_solid_override_accumulator = 0.0;
        if ui.fade_solid_override_accumulator > 0.0 {
            ui.fade_solid_override_accumulator -= offset;
            if ui.fade_solid_override_accumulator < 0.0 {
                ui.fade_solid_override_accumulator = 0.0;
                // Make sure when the override ends that we don't jump to a random point in the
                // blink
                ui.fade_alpha_accumulator = 1.0;
            }
        } else {
            ui.fade_alpha_accumulator += offset;
            ui.fade_alpha_accumulator = ui.fade_alpha_accumulator.rem_euclid(2.0);
        }

    let fade_alpha = if ui.fade_solid_override_accumulator > 0.0 {
            1.0
        } else if ui.fade_alpha_accumulator > 1.0 {
            2.0 - ui.fade_alpha_accumulator
        } else {
            ui.fade_alpha_accumulator
        };

    let mut text_or_rects =
        Vec::with_capacity(1);

    {
        let edit_buffer_text_rect = get_edit_buffer_xywh(view.menu.get_mode(), *font_info, wh);

        let edit_buffer_text_rect: ScreenSpaceRect = edit_buffer_text_rect.into();

        text_or_rects.push(TextOrRect::Text(TextSpec {
            text: "▏text_or_rects ▏",
            size: TEXT_SIZE,
            layout: TextLayout::WrapInRect(edit_buffer_text_rect),
            spec: VisualSpec {
                rect: edit_buffer_text_rect,
                color: palette![red, fade_alpha],
                z: EDIT_Z.saturating_add(3),
            },
        }));
    }

    if !ui.window_is_focused {
        for t_or_r in text_or_rects.iter_mut() {
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

    (text_or_rects, None)
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
    ui: &mut UIState,
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
    fade_alpha: f32,
) -> Option<Input> {
    let mut input = None;

    {
        text_or_rects.push(TextOrRect::Text(TextSpec {
            text: "▏text_or_rects ▏",
            size,
            layout: TextLayout::WrapInRect(outer_rect),
            spec: VisualSpec {
                rect: outer_rect,
                color: palette![red, fade_alpha],
                z: z.saturating_add(3),
            },
        }));
    }

    input
}

pub fn make_active_tab_visible<'view>(
    ui: &mut UIState,
    view: &'view View,
    FontInfo { tab_char_dim, .. }: &FontInfo,
    (screen_width, _): (f32, f32),
) -> Option<()> {
    Some(())
}

fn make_nth_tab_visible_if_present(
    ui: &mut UIState,
    target_index: usize,
    tab_count: usize,
    tab_width: f32,
) {
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

#[derive(Debug, Default)]
pub struct UIState {
    /// This is should be in the range [0.0, 2.0]. This needs the extra space to repesent the down
    /// side of the sawtooth pattern.
    pub fade_alpha_accumulator: f32,
    // If the user has recently made or is making an input, we don't want a distracting animation
    // during that time. Afterwards though, we do want the animation to start again.
    pub fade_solid_override_accumulator: f32,
    pub mouse: UIFocus,
    pub keyboard: UIFocus,
    pub window_is_focused: bool,
}

impl UIState {
    pub fn note_interaction(&mut self) {
        self.fade_solid_override_accumulator = 1.5;
    }
}

impl UIState {
    pub fn frame_init(&mut self) {
    }
    pub fn frame_end(&mut self) {
    }
}

#[derive(Debug, Default)]
pub struct UIFocus {
    active: UIId,
    hot: UIId,
    next_hot: UIId,
}

impl UIFocus {
    pub fn set_not_active(&mut self) {
        self.active = d!();
    }
    pub fn set_active(&mut self, id: UIId) {
        self.active = id;
    }
    pub fn set_next_hot(&mut self, id: UIId) {
        self.next_hot = id;
    }
    #[allow(dead_code)]
    pub fn set_not_hot(&mut self) {
        self.hot = d!();
    }
    pub fn frame_init(&mut self) {
        if self.active == d!() {
            self.hot = self.next_hot;
        }
        self.next_hot = d!();
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
enum InputType {
    Mouse,
    Keyboard,
    Both,
}

macro_rules! input_type_tag {
    ($input_type: expr) => {{
        use InputType::*;
        match $input_type {
            Mouse => 1,
            Keyboard => 2,
            Both => 3,
        }
    }};
}

ord!(and friends for InputType: t, other in input_type_tag!(t).cmp(&input_type_tag!(other)));

#[derive(Clone, Copy, Debug)]
enum ButtonState {
    Usual,
    Hover(InputType),
    Pressed(InputType),
}
ord!(and friends for ButtonState: s, other in {
    use ButtonState::*;
    macro_rules! button_state_tag {
        ($button_state: expr) => (
            match $button_state {
                Usual => (0, 0),
                Hover(input_type) => (1, input_type_tag!(input_type)),
                Pressed(input_type) => (2, input_type_tag!(input_type)),
            }
        );
    }

    button_state_tag!(s).cmp(&button_state_tag!(other))
});

type DoButtonResult = (bool, ButtonState);

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
    false
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
        underline,
        overline,
        z,
    }: OutlineButtonSpec<'view>,
    state: ButtonState,
    fade_alpha: f32,
) {
    use ButtonState::*;

    let text_w = usize_to_f32_or_65536(text.chars().count()) * char_dim.w;
    let text_rect = center_within((text_w, char_dim.h), rect);

    const BACKGROUND_COLOUR: Colour = TAB_BACKGROUND_COLOUR;
    const TEXT_COLOUR: Colour = TAB_TEXT_COLOUR;

    let text_z = z.saturating_add(3);
    let overline_z = z.saturating_add(2);
    let underline_z = z.saturating_add(1);
    //z is the rect z
    let outline_z = z.saturating_sub(1);

    macro_rules! highlight_colour {
        ($input_type: expr) => {{
            use InputType::*;
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
                    rect: text_rect,
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
    FontInfo {
        status_char_dim,
        find_replace_char_dim,
        ..
    }: FontInfo,
    sswh!(width, height): ScreenSpaceWH,
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
    FontInfo {
        status_char_dim,
        find_replace_char_dim,
        tab_char_dim,
        ..
    }: FontInfo,
    sswh!(width, height): ScreenSpaceWH,
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
    FontInfo {
        find_replace_char_dim,
        tab_char_dim,
        ..
    }: FontInfo,
    sswh!(width, height): ScreenSpaceWH,
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
        MenuMode::Hidden | MenuMode::GoToPosition => get_status_line_y(status_char_dim, height),
        MenuMode::FindReplace(_) => get_find_replace_info(font_info, wh).top_y,
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
        None => d!(),
        Text => get_edit_buffer_xywh(mode, font_info, wh),
        Find => get_find_replace_info(font_info, wh).find_text_xywh,
        Replace => get_find_replace_info(font_info, wh).replace_text_xywh,
        FileSwitcher => get_file_switcher_info(font_info, wh).search_text_xywh,
        GoToPosition => get_go_to_position_info(font_info, wh).input_text_xywh,
    }
}

/// This function determines whether the mouse cursor should use the text selection icon ot not.
pub fn should_show_text_cursor(
    xy: ScreenSpaceXY,
    mode: MenuMode,
    font_info: FontInfo,
    wh: ScreenSpaceWH,
) -> bool {
    let inside_edit_buffer = inside_rect(xy, get_edit_buffer_xywh(mode, font_info, wh).into());
    inside_edit_buffer || match mode {
        MenuMode::Hidden => false,
        MenuMode::FindReplace(_) => {
            let FindReplaceInfo {
                find_outer_rect,
                replace_outer_rect,
                ..
            } = get_find_replace_info(font_info, wh);

            inside_rect(xy, find_outer_rect.into())
            || inside_rect(xy, replace_outer_rect.into())
        }
        MenuMode::FileSwitcher => {
            let FileSwitcherInfo {
                search_outer_rect,
                ..
            } = get_file_switcher_info(font_info, wh);

            inside_rect(xy, search_outer_rect.into())
        }
        MenuMode::GoToPosition => {
            let GoToPositionInfo {
                input_outer_rect,
                ..
            } = get_go_to_position_info(font_info, wh);

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
