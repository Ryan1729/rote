use super::{*, text_layouts::*};

use glyph_brush::{
    get_lines_iter,
    OwnedSectionText,
    rusttype::{point, vector, Point, PositionedGlyph, Rect},
};
use std::borrow::Cow;

use proptest::prelude::{proptest};
use pub_arb_std::{f32::{usual, within_0_to_1, rounded_non_negative}};

mod arb {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::{any, prop_compose};

    prop_compose!{
        pub fn positive_rect_i32()
        ((a, b, c, d) in (2..0x7FFF_FFFE, 2..0x7FFF_FFFE, 2..0x7FFF_FFFE, 2..0x7FFF_FFFE))
        -> Rect<i32> {
            use std::cmp::{max, min};
            let mut rect: Rect<i32> = d!();
            
            rect.min.x = min(a, c);
            if a == c { rect.min.x -= 1; }
            rect.min.y = min(b, d);
            if b == d { rect.min.y -= 1; }

            rect.max.x = max(a, c);
            if a == c { rect.max.x += 1; }
            rect.max.y = max(b, d);
            if b == d { rect.max.y += 1; }

            rect
        }
    }

    prop_compose!{
        pub fn section_geometry()
        ([sx, sy, bx, by] in proptest::array::uniform4(any::<u32>()))
        -> SectionGeometry {
            SectionGeometry {
                screen_position: (sx as f32, sy as f32),
                bounds: (bx as f32, by as f32),
            }
        }
    }
    
    prop_compose!{
        pub fn section_text_vec()
        (v in vec(section_text(), 0..16))
        -> Vec<OwnedSectionText> {
            v
        }
    }

    prop_compose!{
        pub fn section_text()
        (text in ".*", scale in scale(), color in proptest::array::uniform4(within_0_to_1()))
        -> OwnedSectionText {
            OwnedSectionText {
                text,
                scale,
                color,
                font_id: SINGLE_FONT_ID,
            }
        }
    }

    prop_compose!{
        pub fn scale()(x in rounded_non_negative(), y in rounded_non_negative()) -> Scale {
            Scale { x, y }
        }
    }
}

fn calculate_glyphs_unbounded_layout_clipped_slow<'font, F>(
    clip: Rect<i32>,
    fonts: &F,
    geometry: &SectionGeometry,
    sections: &[SectionText],
) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
where
    F: FontMap<'font>,
{
    // TODO reduce duplication with calculate_glyphs fn
    let mut caret = geometry.screen_position;
    let mut out = vec![];
    dbg!();
    let lines = get_lines_iter(fonts, sections, std::f32::INFINITY);
    dbg!();
    for line in lines {
        let line_height = line.line_height();
    dbg!(caret);
        let tuples = line.aligned_on_screen(caret, HorizontalAlign::Left, VerticalAlign::Top);
    dbg!();
        out.extend(
            tuples
                .into_iter()
                .filter(|(glyph, _, _)| {
                    // TODO when is this None?
                    glyph.pixel_bounding_box()
                        .map(move |pixel_coords| {
                            // true if pixel_coords intersects clip
                            pixel_coords.min.x <= clip.max.x
                            && pixel_coords.min.y <= clip.max.y
                            && clip.min.x <= pixel_coords.max.x
                            && clip.min.y <= pixel_coords.max.y
                        })
                        .unwrap_or(true)
                })
        );

        caret.1 += line_height;
    }

    out
}

fn single_font_map() -> [Font<'static>; 1] {
    [Font::from_bytes(FONT_BYTES).unwrap()]
}
const SINGLE_FONT_ID: FontId = FontId(0);

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version(
        clip in arb::positive_rect_i32(),
        owned_sections in arb::section_text_vec()
    ) {
        let font_map = &single_font_map();

        let geometry = SectionGeometry {
            screen_position: (0.0, 0.0),
            bounds: (99999.0, 99999.0),
        };

        let sections_vec: Vec<SectionText<'_>> = owned_sections
            .iter()
            .map(|owned| SectionText::from(owned))
            .collect();
        let sections = &sections_vec;

        let actual = calculate_glyphs_unbounded_layout_clipped(
            clip.clone(),
            font_map,
            &geometry,
            sections,
        );

        let expected = calculate_glyphs_unbounded_layout_clipped_slow(
            clip.clone(),            
            font_map,
            &geometry,
            sections,
        );

        assert_eq!(format!("{:?}", actual), format!("{:?}", expected));
    }
}