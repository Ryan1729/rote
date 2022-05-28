use screen_space::ssr;
use gl_layer_types::{TextOrRect, TextLayout, TextSpec, VisualSpec};

// These were used in `wimp_render` at the time of writing.
const TEXT_SIZE: f32 = 32.0;
const FIND_REPLACE_SIZE: f32 = 26.0;
const STATUS_SIZE: f32 = 22.0;
const TAB_SIZE: f32 = 16.0;

const TEXT_SIZES: [f32; 4] = [TEXT_SIZE, STATUS_SIZE, TAB_SIZE, FIND_REPLACE_SIZE];

/// As of this writing, we use OpenGL to render the text. Specifically, we maintain
/// a sinlge GPU texture containing the glyphs. OpenGL implementations (that is, the 
/// combinations of hardware and drivers) expose a `GL_MAX_TEXTURE_SIZE` value,
/// indicating the maximum size of a texture. For example, a value of 8192 indicates
/// that an 8192 by 8192 texture is the maximum allowed. The OpenGL 3.3 spec states
/// that the minimum required value is 1024, (see Table 6.38), so we ensure support
/// for that. As a worrying aside, The OpenGL ES (ES for embedded systems) spec says 
/// the minimum is 64. 
///
/// The current implementation only ever uses one GPU texture and attempts to
/// increase the GPU texture size if the requested set of glyphs will not fit in the
/// texture when packed with some particular rectangle packing algorithm.
#[test]
fn rendering_all_the_characters_in_the_font_do_not_cause_the_texture_to_get_too_big() {
    const MAX_TEXTURE_SIZE: u16 = 1024;

    let mut state = text_rendering::new(1.).unwrap();

    {
        let (w, h) = state.texture_dimensions();
        assert!(w <= MAX_TEXTURE_SIZE.into(), "pre-condition failure");
        assert!(h <= MAX_TEXTURE_SIZE.into(), "pre-condition failure");
    }
    
    let all_chars = all_chars();

    state.render_vertices(
        &TEXT_SIZES.map(|size| {
            TextOrRect::Text(TextSpec {
                text: &all_chars,
                size,
                layout: TextLayout::Unbounded,
                spec: VisualSpec {
                    rect: ssr!(0., 0., f32::INFINITY, f32::INFINITY),
                    ..<_>::default()
                }
            })
        }),
        (u32::MAX, u32::MAX),
        |_, _| {},
        |_, _| {},
    ).unwrap();

    {
        let (w, h) = state.texture_dimensions();
        assert!(w <= MAX_TEXTURE_SIZE.into());
        assert!(h <= MAX_TEXTURE_SIZE.into());
    }
}

fn all_chars() -> String {
    let mut output = String::with_capacity(4096);

    let face = ttf_parser::Face::from_slice(
        text_rendering::FONT_BYTES,
        0
    ).unwrap();
    
    let tables = face.tables();

    let subtables = tables.cmap.unwrap().subtables;

    for subtable in subtables {
        subtable.codepoints(|bytes| {
            output.push(char::from_u32(bytes).unwrap());
        })
    }

    output
}