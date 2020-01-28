use platform_types::ScreenSpaceRect;
use macros::{d};

pub type Res<T> = Result<T, Box<dyn std::error::Error>>;

/// ```text
/// [
///     left_top * 3,
///     right_bottom * 2,
///     tex_left_top * 2,
///     tex_right_bottom * 2,
///     color * 4,
///     override_alpha
/// ]
/// ```
pub type Vertex = [f32; 14];

pub fn set_full_alpha(vertex: &mut Vertex) {
    vertex[13] = 1.0;
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct TexCoords {
    pub min: Point,
    pub max: Point,    
}

pub fn extract_tex_coords(vertex: &Vertex) -> TexCoords {
    let mut output: TexCoords = d!();
    // To compensate for y flipping in to_vertex
    output.min.x = vertex[5];
    output.max.y = vertex[6];
    output.max.x = vertex[7];
    output.min.y = vertex[8];
    output
}

/// We use thses values so that the default 24 bit depth buffer contains at least 2^16
/// mutually distinct values which are *continuous* which makes mapping them from a u16 easier.
/// Normally, the z values are clamped to be inside [0.0, 1.0], but given only 24 bits of
/// precision, even values within that smaller range are not alwasys distinguishable.
pub const DEPTH_MIN: f32 = 0.3125; // AKA z_to_f32(0)
pub const DEPTH_MAX: f32 = 0.6249924; // AKA z_to_f32(65535)

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
    let z_bits = ((!z) as u32) << 7;
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


#[derive(Clone, Debug)]
pub struct VisualSpec {
    pub rect: ScreenSpaceRect,
    /// Rgba color of rendered item. Defaults to black.
    pub color: [f32; 4],
    /// Z values for use in depth testing. Defaults to 32768
    pub z: u16,
}

pub const DEFAULT_Z: u16 = 32768;

d!(for VisualSpec: VisualSpec{
    rect: d!(),
    color: [0.0, 0.0, 0.0, 1.0],
    z: DEFAULT_Z,
});

#[derive(Clone, Debug)]
pub enum TextLayout {
    Wrap,
    SingleLine,
    //TODO get wrapping working properly with cursors etc. or get rid of the unused variants here.
    WrapInRect(ScreenSpaceRect),
    Unbounded,
}

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
    pub text: &'text str,
    pub color: [f32; 4]
}

#[derive(Clone, Debug)]
pub struct MulticolourTextSpec<'text> {
    /// The font size
    pub size: f32,
    pub layout: TextLayout,
    pub rect: ScreenSpaceRect,
    pub z: u16,
    pub text: Vec<ColouredText<'text>>
}

#[derive(Clone, Debug)]
pub enum TextOrRect<'text> {
    Rect(VisualSpec),
    Text(TextSpec<'text>),
    MulticolourText(MulticolourTextSpec<'text>)
}