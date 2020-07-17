use super::{*, text_layouts::*, unbounded::*};

use glyph_brush::{
    OwnedSectionText,
    rusttype::{point, vector, Point, PositionedGlyph, Rect},
};
use std::borrow::Cow;

use proptest::prelude::{proptest};
use pub_arb_std::{f32::{usual, within_0_to_1, rounded_non_negative}};

mod arb {
    use super::*;
    use proptest::collection::vec;
    use proptest::prelude::{any, prop_compose, Just};

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
        pub fn same_reasonable_scale_section_text_vec()
        (scale in reasonable_scale())
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
        pub fn string_with_many_newlines()
        (text in ".+")
        (indexes in vec(0..text.len(), (text.len() / 2)..text.len()), mut text in Just(text))
        -> String {
            for mut i in indexes {
                // 0 is always a char boundary
                loop {
                    if text.is_char_boundary(i) {
                        text.insert(i, '\n');
                        break;
                    }
                    i -= 1;
                }
            }

            if !text.contains('\n') {
                for mut i in 0..text.len() {
                    // 0 is always a char boundary
                    loop {
                        if text.is_char_boundary(i) {
                            text.insert(i, '\n');
                            break;
                        }
                        i -= 1;
                    }
                }
            }

            text
        }
    }

    proptest!{
        #[test]
        fn string_with_many_newlines_has_at_least_one_newline(
            s in string_with_many_newlines()
        ) {
            assert!(s.contains('\n'));
        }
    }
    

    prop_compose!{
        pub fn section_text_with_many_newlines_and_scale(scale: Scale)
        (text in string_with_many_newlines(), color in color())
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
        pub fn section_text_with_only_periods_and_newlines_and_scale(scale: Scale)
        (text in "[\\.\\n]+", color in color())
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

    const SCALE_MAX: f32 = 1048576.0;

    prop_compose!{
        pub fn reasonable_scale()(x in rounded_non_negative(), y in rounded_non_negative()) -> Scale {
            Scale { 
                x: if x <= SCALE_MAX { x } else { SCALE_MAX },
                y: if y <= SCALE_MAX { y } else { SCALE_MAX },
            }
        }
    }

    prop_compose!{
        pub fn color()(c in proptest::array::uniform4(within_0_to_1())) -> [f32; 4] {
            c
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

    let lines = get_lines_iter(fonts, sections, std::f32::INFINITY);

    for line in lines {
        let line_height = line.line_height();

        let tuples = line.aligned_on_screen(caret, HorizontalAlign::Left, VerticalAlign::Top);

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

macro_rules! scale {
    ($scale_x: expr, $scale_y: expr) => {
        Scale { x: $scale_x, y: $scale_y }
    }
}

macro_rules! ost {
    ($text: literal $(,)? sx $scale_x: literal sy $scale_y: literal) => {
        OwnedSectionText { 
            text: $text.to_string(),
            scale: scale!($scale_x, $scale_y),
            color: [0.0, 0.0, 0.0, 1.0],
            font_id: SINGLE_FONT_ID
        }
    };
    ($text: literal $(,)? s $scale: expr) => {
        OwnedSectionText { 
            text: $text.to_string(),
            scale: $scale,
            color: [0.0, 0.0, 0.0, 1.0],
            font_id: SINGLE_FONT_ID
        }
    };
    ($text: expr, sx $scale_x: literal sy $scale_y: literal) => {
        OwnedSectionText { 
            text: $text.to_string(),
            scale: scale!($scale_x, $scale_y),
            color: [0.0, 0.0, 0.0, 1.0],
            font_id: SINGLE_FONT_ID
        }
    };
    ($text: expr, s $scale: expr) => {
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

/* this test took a while to reduce from the above, and has revealed multiple tests that have been 
copied below, but no it seems to only be failing because the scales are not the same for all 
sections, which we would like to be able to assume in order to optimize.
#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction() {
    let clip = Rect { min: Point { x: 2, y: 2 }, max: Point { x: 3, y: 4 } };
    let owned_sections = vec![
        ost!("  .\n." sx 1.0 sy 2.0),
        ost!("\n ." sx 2.0 sy 1.0),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}
*/

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_when_the_scales_match_the_above(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text_with_scale(scale!(1.0, 2.0)),
        s2 in arb::section_text_with_scale(scale!(2.0, 1.0)),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            vec![s1, s2]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_with_many_newlines_when_the_scales_match_the_above(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text_with_many_newlines_and_scale(scale!(1.0, 2.0)),
        s2 in arb::section_text_with_many_newlines_and_scale(scale!(2.0, 1.0)),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            vec![s1, s2]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_with_only_periods_and_newlines_when_the_scales_match_the_above(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text_with_only_periods_and_newlines_and_scale(scale!(1.0, 2.0)),
        s2 in arb::section_text_with_only_periods_and_newlines_and_scale(scale!(2.0, 1.0)),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            vec![s1, s2]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match(
        clip in arb::positive_rect_i32(),
        owned_sections in arb::same_reasonable_scale_section_text_vec()
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

/// This one was reduced after a change to the code made the above one start passing.
#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_generated_case_alternative_reduction() {
    let clip = Rect { min: Point { x: 1, y: 2 }, max: Point { x: 16281178, y: 44250000 } };
    let scale = Scale { x: 1.0, y: 15391300.0 };
    let owned_sections = vec![
        ost!("\n\n        aaaaaaaaaaaaaaa" s scale),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

/* 
// I don't think we care about cases that only happen with ridiculously large scales
#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_many_blank_lines_generated_case() {
    let clip = Rect { min: Point { x: 2, y: 41960326 }, max: Point { x: 584956, y: 42351515 } };
    let scale = Scale { x: 452285.0, y: 5245041.0 };
    let owned_sections = vec![
        ost!("\u{b}" s scale),
        ost!("\u{b}" s scale),
        ost!("\u{b}" s scale),
        ost!("\u{b}" s scale),
        ost!("\u{b}\u{b}\r/" s scale),
        ost!("%𩏤Î3\u{7f}\u{78ae8}\\\u{2}&\u{0}?q7$S?~\u{1b}\u{6}=¥.*<{\u{3}v\u{b}\"s*" s scale),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_many_blank_lines_generated_case_reduction() {
    let clip = Rect {
        min: Point { x: 0, y: 32041127 },
        max: Point { x: 0, y: 32041127 * 2 }
    };
    let scale = Scale { x: 1.0, y: 4004187.0 };
    let owned_sections = vec![
        ost!("\n\n\n\n\n\n\n𩏤" s scale),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_many_blank_lines_generated_case_automated_reduction(
        min_y in 32041127..=32041127,
        scale_y in 4004187..=4004187,
        line_count in 0..=7,
    ) {
        let min_y: i32 = min_y as _;
        let clip = Rect {
            min: Point { x: 0, y: min_y },
            max: Point { x: 0, y: min_y + min_y }
        };
        let scale = Scale { x: 1.0, y: scale_y as f32 };

        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            vec![ost!(format!("{}𩏤", "\n".repeat(line_count as usize)), s scale),]
        )
    }
}*/

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

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_there_is_exactly_two_sections(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text(),
        s2 in arb::section_text(),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            vec![s1, s2]
        )
    }
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_instructive_case() {
    let clip = Rect {
        min: Point {
            x: 2,
            y: 0,
        },
        max: Point {
            x: i32::max_value(),
            y: i32::max_value(),
        },
    };

    let section_text = ost!(
    // the "*" are within the clip rect, and the "." is not.
    // This breaks the current (as of this writing) implementation.
r"
  *
.
  *
"
        s scale!(1.0, 1.0)
    );

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        vec![section_text]
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction_alternate() {
    let clip = Rect { min: Point { x: 2, y: 2 }, max: Point { x: i32::max_value(), y: i32::max_value() } };
    let owned_sections = vec![
        ost!(
r"
  .
.
  .
" 
            sx 2.0 sy 1.0
        )
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction_alternate2() {
    let clip = Rect { min: Point { x: 9008, y: 1 }, max: Point { x: 9009, y: 25882408 } };
    let owned_sections = vec![
        ost!("aaa\n" sx 45700.0 sy 1.0),
        ost!("\naaa" sx 10000.0 sy 2.0),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        owned_sections
    )
}