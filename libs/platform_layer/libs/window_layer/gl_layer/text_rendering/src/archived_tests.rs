macro_rules! ignored {
    ($($tokens: tt)*) => {}
}
ignored!{
use super::{*, text_layouts::*};

use glyph_brush::{
    CalculatedGlyph,
    SectionGeometry,
    OwnedSectionText,
    Point,
    Rect,
    intersects,
};

use proptest::{proptest};
use pub_arb_std::{f32::{within_0_to_1, rounded_non_negative}};

mod arb {
    use super::*;
    use proptest::collection::vec;
    use proptest::{extra::*, prop_compose, Just};

    prop_compose!{
        pub fn positive_rect_i32()
        ((a, b, c, d) in (2..0x7FFF_FFFE, 2..0x7FFF_FFFE, 2..0x7FFF_FFFE, 2..0x7FFF_FFFE))
        -> Rect {
            use std::cmp::{max, min};
            let mut rect: Rect = d!();

            rect.min.x = min(a, c) as f32;
            if a == c { rect.min.x -= 1.; }
            rect.min.y = min(b, d) as f32;
            if b == d { rect.min.y -= 1.; }

            rect.max.x = max(a, c) as f32;
            if a == c { rect.max.x += 1.; }
            rect.max.y = max(b, d) as f32;
            if b == d { rect.max.y += 1.; }

            rect
        }
    }

    prop_compose!{
        pub fn section_geometry()
        ([sx, sy, bx, by] in proptest::array::uniform4(any_u32()))
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
        (text in ".*", colour in proptest::array::uniform4(within_0_to_1()))
        -> OwnedSectionText {
            OwnedSectionText {
                text,
                colour,
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
        pub fn section_text_with_many_newlines()
        (text in string_with_many_newlines(), colour in colour())
        -> OwnedSectionText {
            OwnedSectionText {
                text,
                colour,
            }
        }
    }

    prop_compose!{
        pub fn section_text_with_only_periods_and_newlines()
        (text in "[\\.\\n]+", colour in colour())
        -> OwnedSectionText {
            OwnedSectionText {
                text,
                colour,
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
        pub fn colour()(c in proptest::array::uniform4(within_0_to_1())) -> [f32; 4] {
            c
        }
    }
}

// This version is meant to be clearly correct, but willing to be slow in order to
// meet that goal.
fn calculate_glyphs_unbounded_layout_clipped_slow<'font>(
    clip: Rect,
    font: &Font<'font>,
    scale: Scale,
    geometry: &SectionGeometry,
    sections: &[SectionText],
) -> Vec<CalculatedGlyph<'font>>
{
    // TODO reduce duplication with calculate_glyphs fn
    let mut caret = geometry.screen_position;
    let mut out = vec![];

    let lines = unbounded::get_lines_iter(font, scale, sections);

    for line in lines {
        let v_metrics = font.v_metrics(scale);
        let line_height: f32 = v_metrics.ascent - v_metrics.descent + v_metrics.line_gap;

        out.extend(
            line
                .glyphs
                .into_iter()
                .filter(|(glyph, _)| {
                    intersects(font, glyph, &clip)
                })
                .map(|(glyph, colour)| {
                    CalculatedGlyph {
                        glyph, 
                        colour,
                    }
                })
        );

        caret.1 += line_height;
    }

    out
}

fn single_font() -> Font<'static> {
    Font::from_bytes(FONT_BYTES).unwrap()
}

fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
    clip: Rect,
    scale: Scale,
    owned_sections: Vec<OwnedSectionText>
) {
    let font = &single_font();

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
        &font,
        scale,
        &geometry,
        sections,
    );

    let expected = calculate_glyphs_unbounded_layout_clipped_slow(
        clip.clone(),            
        &font,
        scale,
        &geometry,
        sections,
    );

    assert_eq!(format!("{:?}", actual), format!("{:?}", expected));
}

macro_rules! scale {
    ($scale: expr) => {
        Scale { x: $scale, y: $scale }
    };
    ($scale_x: expr, $scale_y: expr) => {
        Scale { x: $scale_x, y: $scale_y }
    }
}

macro_rules! ost {
    ($text: expr) => {
        OwnedSectionText { 
            text: $text.to_string(),
            colour: [0.0, 0.0, 0.0, 1.0],
        }
    };
    ($text: expr) => {
        OwnedSectionText { 
            text: $text.to_string(),
            colour: [0.0, 0.0, 0.0, 1.0],
        }
    }
}

proptest!{
    #[test]
    #[ignore]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version(
        clip in arb::positive_rect_i32(),
        scale in arb::scale(),
        owned_sections in arb::section_text_vec()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale,
            owned_sections
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_with_reasonable_scales(
        clip in arb::positive_rect_i32(),
        scale in arb::reasonable_scale(),
        owned_sections in arb::section_text_vec()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale,
            owned_sections
        )
    }
}

macro_rules! make_clip {
    ($x1: expr, $y1: expr, max_value, max_value $(,)?) => {
        make_clip!($x1, $y1, 1.0 / 0.0, 1.0 / 0.0)
    };
    ($x1: expr, $y1: expr, $x2: expr, $y2: expr $(,)?) => {
        Rect { min: Point { x: $x1 as f32, y: $y1 as f32 }, max: Point { x: $x2 as f32, y: $y2 as f32 } }
    }
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case() {
    let clip = make_clip!(11897398, 2, 11897399, 25882408);
    let scale = scale!(12639997.0, 128678.0);
    let owned_sections = vec![
        ost!("¡¡A"),
        ost!("A ¡4\u{b}!"),
        ost!("\u{b}¡0!"),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
        owned_sections
    )
}

// this test took a while to reduce from the above, and has revealed multiple tests that have been 
// copied below.
#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction() {
    let clip = make_clip!(2, 2, 3, 4);
    let scale = scale!(1.0, 2.0);
    let owned_sections = vec![
        ost!("  .\n."),
        ost!("\n ."),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
        owned_sections
    )
}

// These add_with_overflow tests came up because rusttype uses i32's internally.
// We don't currently care about screens/windows that large so we're ignoring these for now.
#[test]
#[ignore]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_add_with_overflow_generated_case() {
    let clip = make_clip!(1, 1, 3, 3);
    let scale = scale!(1.0, 16520385.0);
    let owned_sections = vec![
        ost!("¡a\u{0}A   a"),
        ost!("¡¡a¡A\u{0}¡¡A¡ \u{b}0¡\u{0}¡\u{b}¡ ¡¡A ¡¡¡A"),
        ost!("\u{0}a\u{b}¡  ¡¡  A"),
        ost!("\u{0}a\u{b}¡  ¡¡  A"),
        ost!("AA¡ ¡0\u{0}AA\u{0} A\u{b}¡\u{b}aa \u{b} 0 "),
        ost!("A0¡0\u{e000}¡ a¡\u{b}\u{0}\u{0}¡"),
        ost!(" 0¡\u{b}aA¡\u{0}\u{b}¡ a"),
        ost!("aa¡ ¡¡¡¡¡ aa\u{e000}a¡\u{0}A0A 0\u{0}¡a"),
        ost!("¡¡¡ aaa¡¡¡¡A 0¡  a"),
        ost!("\u{0}¡¡a\u{0}\u{b}0 A\u{0}0a¡¡A¡aa\u{b}¡ \u{b}¡ \u{0} \u{e000}aA\u{0}0"),
        ost!("0  ¡¡A¡¡¡0\u{b}   ¡¡ \u{b}¡0¡¡  ¡¡AaA"),
        ost!("¡\u{0}\u{0}¡aaaſ"),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
        owned_sections
    )
}

#[test]
#[ignore]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_add_with_overflow_reduced_case() {
    let clip = make_clip!(1, 1, 3, 3);
    let scale = scale!(1.0, 2_140_000_000.0);
    let owned_sections = vec![
        ost!("_____"),
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
        owned_sections
    )
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_when_the_scales_match_the_above(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text(),
        s2 in arb::section_text(),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale!(1.0, 2.0),
            vec![s1, s2]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_with_many_newlines_when_the_scales_match_the_above(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text_with_many_newlines(),
        s2 in arb::section_text_with_many_newlines(),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale!(1.0, 2.0),
            vec![s1, s2]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_with_only_periods_and_newlines_when_the_scales_match_the_above(
        clip in arb::positive_rect_i32(),
        s1 in arb::section_text_with_only_periods_and_newlines(),
        s2 in arb::section_text_with_only_periods_and_newlines(),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale!(1.0, 2.0),
            vec![s1, s2]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match(
        clip in arb::positive_rect_i32(),
        scale in arb::reasonable_scale(),
        owned_sections in arb::section_text_vec()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale,
            owned_sections
        )
    }
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_generated_case() {
    let clip = make_clip!(14730591, 2, 16281178, 44250215);
    let scale = scale!(1292157.0, 15391379.0);
    let owned_sections = vec![
        ost!("¡¡0 \u{e000}0¡A¡0Ì\u{102e5c}`\u{b4f4e}%¥%*\'.\u{43106}/\u{35bb6}"),
        ost!("*&\u{7d0c9}\u{1b}\u{c92af}:V\u{dd382}\r{<H$\tV$?\u{7532a}&@{?d㎽`{\r\u{56590}U"),
        ost!("OgÐ\u{32bcd}</\u{fd2cd}Ξ\u{c4f}\u{3e42b}\u{3}"),
        ost!("�𢣻\\%.\u{2e}E*O$/s¥\u{59680}�\u{4f20d}\u{202e}%5\u{6860c}\u{afb1a}\u{202e}\u{6bcda}`\u{3}?"),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_generated_case_reduction() {
    let clip = make_clip!(14730591, 2, 16281178, 44250215);
    let scale = Scale { x: 1289999.0, y: 2.0 };
    let owned_sections = vec![
        ost!("                      - \n-\n                      - "),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
        owned_sections
    )
}

/// This one was reduced after a change to the code made the above one start passing.
#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_the_scales_match_in_this_generated_case_alternative_reduction() {
    let clip = make_clip!(1, 2, 16281178, 44250000);
    let scale = Scale { x: 1.0, y: 15391300.0 };
    let owned_sections = vec![
        ost!("\n\n        aaaaaaaaaaaaaaa"),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale,
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
        scale in arb::reasonable_scale(),
        section_text in arb::section_text()
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale,
            vec![section_text]
        )
    }
}

proptest!{
    #[test]
    fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_if_there_is_exactly_two_sections(
        clip in arb::positive_rect_i32(),
        scale in arb::reasonable_scale(),
        s1 in arb::section_text(),
        s2 in arb::section_text(),
    ) {
        calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
            clip,
            scale,
            vec![s1, s2]
        )
    }
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_instructive_case() {
    let clip = make_clip!(2, 0, max_value, max_value);

    let section_text = ost!(
    // the "*" are within the clip rect, and the "." is not.
    // This breaks the current (as of this writing) implementation.
r"
  *
.
  *
"
    );

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale!(1.0, 1.0),
        vec![section_text]
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction_alternate() {
    let clip = make_clip!(2, 2, max_value, max_value);
    let owned_sections = vec![
        ost!(
r"
  .
.
  .
"
        )
    ]; 

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale!(2.0, 1.0),
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_in_this_generated_case_reduction_alternate2() {
    let clip = make_clip!(9008, 1, 9009, 25882408);
    let owned_sections = vec![
        ost!("aaa\n"),
        ost!("\naaa"),
    ];

    calculate_glyphs_unbounded_layout_clipped_matches_the_slow_version_on(
        clip,
        scale!(45700.0, 1.0),
        owned_sections
    )
}

#[test]
fn calculate_glyphs_unbounded_layout_clipped_slow_puts_5_a_characters_on_the_same_line() {
    let clip = make_clip!(0, 1, 99999, 99999);
    let scale = scale!(16.0);
    let font = &single_font();

    let geometry = SectionGeometry {
        screen_position: (0.0, 0.0),
        bounds: (99999.0, 99999.0),
    };

    let owned_sections = vec![
        ost!("aaa"),
        ost!("aa"),
    ];

    let sections_vec: Vec<SectionText<'_>> = owned_sections
        .iter()
        .map(|owned| SectionText::from(owned))
        .collect();
    let sections = &sections_vec;

    let glyphs = calculate_glyphs_unbounded_layout_clipped_slow(
        clip.clone(),            
        font,
        scale,
        &geometry,
        sections,
    );

    for w in glyphs.windows(2) {
        let y1 = w[0].glyph.position().y;
        let y2 = w[1].glyph.position().y;
        assert_eq!(y1, y2);
    }
}
}