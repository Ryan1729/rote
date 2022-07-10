use screen_space::ssr;
use gl_layer_types::{TextOrRect, TextLayout, TextSpec, U24, VisualSpec};

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

    let sizes = &TEXT_SIZES.map(|size| {
        TextOrRect::Text(TextSpec {
            text: &all_chars,
            size,
            layout: TextLayout::Unbounded,
            spec: VisualSpec {
                rect: ssr!(0., 0., f32::INFINITY, f32::INFINITY),
                ..<_>::default()
            }
        })
    });

    let dimensions = (U24::MAX, U24::MAX);

    if cfg!(feature = "dump-all-characters-texture") {
        use std::io::Write;
        const SIZE: usize = MAX_TEXTURE_SIZE as usize * 2;

        let mut buffer = vec![
            0u8;
            SIZE * SIZE * 3
        ];

        state.render_vertices(
            sizes,
            dimensions,
            |rect, bytes| {
                let mut bytes_i = 0;
                let (w, h) = (u32::from(rect.w), u32::from(rect.h));

                for y in u32::from(rect.y)..u32::from(rect.y) + h {
                    for x in u32::from(rect.x)..u32::from(rect.x) + w {
                        let byte = bytes[bytes_i];
                        bytes_i += 1;

                        let i = (y * SIZE as u32 * 3 + x * 3) as usize;

                        buffer[i] = byte;
                        buffer[i + 1] = byte;
                        buffer[i + 2] = byte;
                    }
                }

                assert_eq!(bytes_i, bytes.len());
            },
            |(_, _)| {},
        ).unwrap();

        let mut output = Vec::with_capacity(buffer.len() + 24);
        writeln!(&mut output, "P6\n{SIZE} {SIZE} 255").unwrap();
        output.extend(buffer);

        std::fs::write("all-characters-texture.ppm", &output).unwrap();
    } else {
        state.render_vertices(
            sizes,
            dimensions,
            |_, _| {},
            |(_, _)| {}
        ).unwrap();
    };

    {
        let (w, h) = state.texture_dimensions();
        assert!(w <= MAX_TEXTURE_SIZE.into(), "({w}, {h})");
        assert!(h <= MAX_TEXTURE_SIZE.into(), "({w}, {h})");
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