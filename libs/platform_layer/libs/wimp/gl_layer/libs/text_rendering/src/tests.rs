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
        pub fn same_scale_section_text_vec()
        (scale in scale())
        (v in vec(section_text_with_scale(scale), 0..16))
        -> Vec<OwnedSectionText> {
            v
        }
    }

    prop_compose!{
        pub fn section_text_with_scale(scale: Scale)
        (text in ".*", color in proptest::array::uniform4(within_0_to_1()))
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

fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
    clip: Rect<i32>,
    owned_sections: Vec<OwnedSectionText>
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

macro_rules! ost {
    ($text: literal sx $scale_x: literal sy $scale_y: literal) => {
        OwnedSectionText { 
            text: $text.to_string(),
            scale: Scale { x: $scale_x, y: $scale_y },
            color: [0.0, 0.0, 0.0, 1.0],
            font_id: SINGLE_FONT_ID
        }
    };
    ($text: literal s $scale: expr) => {
        OwnedSectionText { 
            text: $text.to_string(),
            scale: $scale,
            color: [0.0, 0.0, 0.0, 1.0],
            font_id: SINGLE_FONT_ID
        }
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version(
        clip in arb::positive_rect_i32(),
        owned_sections in arb::section_text_vec()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            owned_sections
        )
    }
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case() {
    let clip = Rect { min: Point { x: 11897398, y: 2 }, max: Point { x: 11897399, y: 25882408 } };
    let owned_sections = vec![
        ost!("¡¡A" sx 5075140.0 sy 1.0),
        ost!("A ¡4\u{b}!" sx 2204342.0 sy 2.0),
        ost!("\u{b}¡0!" sx 12639997.0 sy 128678.0),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction() {
    let clip = Rect { min: Point { x: 2, y: 2 }, max: Point { x: 3, y: 4 } };
    let owned_sections = vec![
        ost!("  .\n." sx 1.0 sy 2.0),
        ost!("\n.." sx 2.0 sy 1.0),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match(
        clip in arb::positive_rect_i32(),
        owned_sections in arb::same_scale_section_text_vec()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            owned_sections
        )
    }
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_generated_case() {
    let clip = Rect { min: Point { x: 14730591, y: 2 }, max: Point { x: 16281178, y: 44250215 } };
    let scale = Scale { x: 1292157.0, y: 15391379.0 };
    let owned_sections = vec![
        ost!("¡¡0 \u{e000}0¡A¡0Ì\u{102e5c}`\u{b4f4e}%¥%*\'.\u{43106}/\u{35bb6}" s scale),
        ost!("*&\u{7d0c9}\u{1b}\u{c92af}:V\u{dd382}\r{<H$\tV$?\u{7532a}&@{?d㎽`{\r\u{56590}U" s scale),
        ost!("OgÐ\u{32bcd}</\u{fd2cd}Ξ\u{c4f}\u{3e42b}\u{3}" s scale),
        ost!("�𢣻\\%.\u{2e}E*O$/s¥\u{59680}�\u{4f20d}\u{202e}%5\u{6860c}\u{afb1a}\u{202e}\u{6bcda}`\u{3}?" s scale),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_generated_case_reduction() {
    let clip = Rect { min: Point { x: 14730591, y: 2 }, max: Point { x: 16281178, y: 44250215 } };
    let scale = Scale { x: 1289999.0, y: 2.0 };
    let owned_sections = vec![
        ost!("                      - \n-\n                      - " s scale),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_there_is_only_one_section(
        clip in arb::positive_rect_i32(),
        section_text in arb::section_text()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            vec![section_text]
        )
    }
}