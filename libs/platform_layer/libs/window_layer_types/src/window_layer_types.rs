
use macros::d;
use platform_types::{ScrollXY};
use platform_layer_types::{CustomEvent};
use screen_space::{abs, CharDim, ScreenSpaceXY, ScreenSpaceRect};

use bitflags::bitflags;
use std::borrow::Cow;
use std::time::Duration;

mod u24;
pub use u24::U24;

/// `(width, height)`
pub type U24Dimensions = (U24, U24);

pub struct InitOutput<State, EventProxy> {
    pub state: State,
    pub event_proxy: EventProxy,
    pub char_dims: Vec<CharDim>,
    pub dimensions: Dimensions,
}

bitflags! {
    /// Represents the current state of the keyboard modifiers
    ///
    /// Each flag represents a modifier and is set if this modifier is active.
    #[derive(Copy, Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
    pub struct ModifiersState: u32 {
        /// The "shift" key.
        const SHIFT = 0b100;
        /// The "control" key.
        const CTRL = 0b100 << 3;
        /// The "alt" key.
        const ALT = 0b100 << 6;
        /// This is the "windows" key on PC and "command" key on Mac.
        const LOGO = 0b100 << 9;
    }
}

#[non_exhaustive]
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum MouseButton {
    Left,
    //Right,
    //Middle,
    //Other(u16),
}

/// Some platforms have modes for windows that indicate that the window requires
/// user attention. It is valid for a platform to ignore this value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttentionMode {
    Neutral,
    Urgent,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Dimensions {
    pub width: abs::Length,
    pub height: abs::Length,
}

impl core::fmt::Display for Dimensions {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "({}, {})", self.width, self.height)
    }
}

pub type RGBA = [f32; 4];

pub type RatePerSecond = f64;

#[derive(Clone, Copy, Debug)]
pub enum Freshness {
    Stale,
    Fresh,
}

#[derive(Clone, Copy, Debug)]
pub struct RateReport {
    pub rate: RatePerSecond,
    pub freshness: Freshness
}

#[must_use]
pub fn initial_dt(target_rate: Option<RatePerSecond>) -> Duration {
    // This value only affects the first frame, and probably will only be used for
    // visual stuff anyway.
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    Duration::from_nanos((
        (1.0 / target_rate.unwrap_or(
            250.//DEFAULT_TARGET_RATE
        )) * 1_000_000_000.0
    ) as u64)
}

/// AKA `hidpi_factor`/`DpiFactor`. The text rendering library only accepts `f32`,
/// so there's not much point in higher precision elsewhere.
pub type ScaleFactor = f32;

pub type Z = u16;

#[derive(Clone, Debug)]
pub struct VisualSpec {
    pub rect: ScreenSpaceRect,
    /// Rgba colour of rendered item. Defaults to black.
    pub colour: RGBA,
    /// Z values for use in depth testing. Defaults to 32768
    pub z: Z,
}

pub const DEFAULT_Z: Z = 32768;

d!(for VisualSpec: VisualSpec{
    rect: d!(),
    colour: [0.0, 0.0, 0.0, 1.0],
    z: DEFAULT_Z,
});

#[derive(Clone, Debug)]
pub enum TextLayout {
    Unbounded,
    UnboundedLayoutClipped(ScreenSpaceRect, ScrollXY)
}

d!(for TextLayout: TextLayout::Unbounded);

#[derive(Clone, Debug)]
pub struct TextSpec<'text> {
    pub spec: VisualSpec,
    pub text: &'text str,
    /// The font size
    pub size: f32,
    pub layout: TextLayout,
}

#[derive(Clone, Debug)]
pub struct ColouredText<'text> {
    pub text: Cow<'text, str>,
    pub colour: [f32; 4]
}

#[derive(Clone, Debug)]
pub struct MulticolourTextSpec<'text> {
    /// The font size
    pub size: f32,
    pub layout: TextLayout,
    pub rect: ScreenSpaceRect,
    pub z: Z,
    pub text: Vec<ColouredText<'text>>
}

#[derive(Clone, Debug)]
pub enum TextOrRect<'text> {
    Rect(VisualSpec),
    Text(TextSpec<'text>),
    MulticolourText(MulticolourTextSpec<'text>)
}

pub type Vertex = [f32; 14];

pub fn set_alpha(vertex: &mut Vertex, alpha: f32) {
    vertex[3] = alpha;
}

pub struct VertexStruct {
    pub left_top_x: f32,
    pub left_top_y: f32,
    pub left_top_z: f32,
    pub override_alpha: f32,
    pub right_bottom_x: f32,
    pub right_bottom_y: f32,
    pub tex_left_top_x: f32,
    pub tex_left_top_y: f32,
    pub tex_right_bottom_x: f32,
    pub tex_right_bottom_y: f32,
    pub colour_r: f32,
    pub colour_g: f32,
    pub colour_b: f32,
    pub colour_a: f32,
}

impl VertexStruct {
    #[must_use]
    pub fn into_vertex(self) -> Vertex {
        [
            self.left_top_x,
            self.left_top_y,
            self.left_top_z,
            self.override_alpha,
            self.right_bottom_x,
            self.right_bottom_y,
            self.tex_left_top_x,
            self.tex_left_top_y,
            self.tex_right_bottom_x,
            self.tex_right_bottom_y,
            self.colour_r,
            self.colour_g,
            self.colour_b,
            self.colour_a,
        ]
    }
}

/// We use these values so that the default 24 bit depth buffer contains at least 2^16
/// mutually distinct values which are *continuous* which makes mapping them from a u16 easier.
/// Normally, the z values are clamped to be inside [0.0, 1.0], but given only 24 bits of
/// precision, even values within that smaller range are not alwasys distinguishable.
pub const DEPTH_MIN: f32 = 0.3125; // AKA z_to_f32(0)
pub const DEPTH_MAX: f32 = 0.624_992_4; // AKA z_to_f32(65535)

#[must_use]
pub fn z_to_f32(z: u16) -> f32 {
    // for some bizarre reason OpenGL maps most of the depth precision to the middle of the range
    // see https://developer.nvidia.com/content/depth-precision-visualized
    // so we map the u16 to be within the range [0.25, 0.75] in the following way:
    //          s: sign, e: exponent, f: fraction
    //          seee eeee  efff ffff  ffff ffff  ffff ffff
    // `0.25 == 0011_1110__1000_0000__0000_0000__0000_0000`
    // `0.75 == 0011_1111__0100_0000__0000_0000__0000_0000`
    let minimum_bits: u32 = 0.25f32.to_bits();
    // bitwise negation to make larger incoming z mean closer rather than farther away.
    let z_bits = (u32::from(!z)) << 7;
    // iteratively arrived at to shift the ends of the range towards the middle.
    let shift_towards_middle = 1 << 21;

    f32::from_bits(minimum_bits + z_bits + shift_towards_middle)
}

/// Once `to_bits` and `from_bits` are stabilized as const, these will be unnecessary, since then
/// we can just define the consts to be what we are checking they are here.
#[cfg(test)]
mod z_depth_tests {
    use super::*;
    #[test]
    fn depth_min_is_correct() {
        assert_eq!(DEPTH_MIN, z_to_f32(65535))
    }

    #[test]
    fn depth_max_is_correct() {
        assert_eq!(DEPTH_MAX, z_to_f32(0))
    }
}

#[non_exhaustive]
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CursorIcon {
    /// The platform-dependent default cursor.
    #[default]
    Default,
    /// Indicates text that may be selected or edited.
    Text,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum ButtonState {
    Pressed,
    Released
}

pub struct KeyboardInput {
    pub state: ButtonState,
    pub keycode: KeyCode,
    pub modifiers: ModifiersState,
}

pub struct MouseInput {
    pub state: ButtonState,
    pub button: MouseButton,
    pub modifiers: ModifiersState,
}

pub struct MouseWheel {
    pub delta: f32,
    pub modifiers: ModifiersState,
}

pub struct CursorMoved {
    pub position: ScreenSpaceXY,
    pub modifiers: ModifiersState
}

// TODO can we simplify this type any more?
// Reducing the amount of messages we to
// only those the app actually uses, reduces
// the amount new platforms need to support.
// We should consider the possibility of
// wanting to add functionality to the app,
// later however, at not make adding that
// back more painful than necessary.
// Should we flatten the CustomEvent part?
pub enum Event {
    CloseRequested,
    RedrawRequested,
    KeyboardInput(KeyboardInput),
    MouseInput(MouseInput),
    MouseWheel(MouseWheel),
    ScaleFactorChanged(ScaleFactor),
    Resized(Dimensions),
    Focused(bool),
    ReceivedCharacter(char),
    CursorMoved(CursorMoved),
    /// Only sent if we are not in the process of shutting down.
    MainEventsCleared,
    Init,
    UserEvent(CustomEvent)
}

#[non_exhaustive]
/// Symbolic name for a keyboard key.
#[derive(Debug, Hash, Ord, PartialOrd, PartialEq, Eq, Clone, Copy)]
#[repr(u32)]
pub enum KeyCode {
    /// The '1' key over the letters.
    Key1,
    /// The '2' key over the letters.
    Key2,
    /// The '3' key over the letters.
    Key3,
    /// The '4' key over the letters.
    Key4,
    /// The '5' key over the letters.
    Key5,
    /// The '6' key over the letters.
    Key6,
    /// The '7' key over the letters.
    Key7,
    /// The '8' key over the letters.
    Key8,
    /// The '9' key over the letters.
    Key9,
    /// The '0' key over the 'O' and 'P' keys.
    Key0,

    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,

    /// The Escape key, next to F1.
    Escape,

    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,

    /// Print Screen/SysRq.
    Snapshot,
    /// Scroll Lock.
    Scroll,
    /// Pause/Break key, next to Scroll lock.
    Pause,

    /// `Insert`, next to Backspace.
    Insert,
    Home,
    Delete,
    End,
    PageDown,
    PageUp,

    Left,
    Up,
    Right,
    Down,

    /// The Backspace key, right over Enter.
    // TODO: rename
    Back,
    /// The Enter key.
    Return,
    /// The space bar.
    Space,

    /// The "Compose" key on Linux.
    Compose,

    Caret,

    Numlock,
    Numpad0,
    Numpad1,
    Numpad2,
    Numpad3,
    Numpad4,
    Numpad5,
    Numpad6,
    Numpad7,
    Numpad8,
    Numpad9,
    NumpadAdd,
    NumpadDivide,
    NumpadDecimal,
    NumpadComma,
    NumpadEnter,
    NumpadEquals,
    NumpadMultiply,
    NumpadSubtract,

    AbntC1,
    AbntC2,
    Apostrophe,
    Apps,
    Asterisk,
    At,
    Ax,
    Backslash,
    Calculator,
    Capital,
    Colon,
    Comma,
    Convert,
    Equals,
    Grave,
    Kana,
    Kanji,
    LAlt,
    LBracket,
    LControl,
    LShift,
    LWin,
    Mail,
    MediaSelect,
    MediaStop,
    Minus,
    Mute,
    MyComputer,
    // also called "Next"
    NavigateForward,
    // also called "Prior"
    NavigateBackward,
    NextTrack,
    NoConvert,
    OEM102,
    Period,
    PlayPause,
    Plus,
    Power,
    PrevTrack,
    RAlt,
    RBracket,
    RControl,
    RShift,
    RWin,
    Semicolon,
    Slash,
    Sleep,
    Stop,
    Sysrq,
    Tab,
    Underline,
    Unlabeled,
    VolumeDown,
    VolumeUp,
    Wake,
    WebBack,
    WebFavorites,
    WebForward,
    WebHome,
    WebRefresh,
    WebSearch,
    WebStop,
    Yen,
    Copy,
    Paste,
    Cut,
} // KeyCode
