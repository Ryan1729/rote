use screen_space::ssr;
use gl_layer_types::{TextOrRect, TextLayout, TextSpec, VisualSpec};

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
        &vec![
            TextOrRect::Text(TextSpec {
                text: &all_chars,
                size: 32.,
                layout: TextLayout::Unbounded,
                spec: VisualSpec {
                    rect: ssr!(0., 0., f32::INFINITY, f32::INFINITY),
                    ..<_>::default()
                }
            })
        ],
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
    dbg!(text_rendering::FONT_BYTES.len());
    todo!()
}