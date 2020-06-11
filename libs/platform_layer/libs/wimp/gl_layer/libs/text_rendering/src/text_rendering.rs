use platform_types::{
    screen_positioning::{CharDim, ScreenSpaceXY, ScreenSpaceRect, ScrollXY, TextSpaceXYWH},
    NonNegF32,
    char_dim, non_neg_f32, tsxywh, ssr,
};
use gl_layer_types::{Vertex, VertexStruct, TexCoords, set_alpha, TextOrRect, Res};

use macros::{d, dbg};
use panic_safe_rope::is_linebreak_char;

use std::convert::TryFrom;

use glyph_brush::*;
use glyph_brush::{
    rusttype::{Font, Scale, Rect, VMetrics},
    Bounds, GlyphBrush, GlyphBrushBuilder, RectSpec, Layout, PixelCoords, Section,
    get_unbounded_line_glyphs_iter
};

mod text_layouts {
    use super::*;
    use macros::{dbg};
    use glyph_brush::{
        get_lines_iter,
        rusttype::{point, vector, Point, PositionedGlyph, Rect},
    };
    use std::borrow::Cow;

    use std::f32::INFINITY;
    //const INFINITY: f32 = 65536.0; // 64k pixels ought to be enough for anybody!
    const INFINITY_RECT: Rect<f32> = Rect {
        min: Point {
            x: -INFINITY,
            y: -INFINITY,
        },
        max: Point {
            x: INFINITY,
            y: INFINITY,
        },
    };

    #[derive(Hash)]
    pub struct Wrap {}

    //
    // Most of this is copied from the  <L: LineBreaker> GlyphPositioner for Layout<L> impl in
    // `glyph-brush-layout`. The one part I wanted to change is noted below, but while I was at it
    // I simplified it by removing support for layout options I have yet to have a need for.
    //
    fn calculate_glyphs<'font, F: FontMap<'font>>(
        font_map: &F,
        mut caret: (f32, f32),
        bound_w: f32,
        sections: &[SectionText<'_>],
    ) -> Vec<(PositionedGlyph<'font>, Color, FontId)> {
        let mut out = vec![];

        let lines = get_lines_iter(font_map, sections, bound_w);

        for line in lines {
            // This is the part that needed to be changed.
            /*
            // top align can bound check & exit early
            if v_align_top && caret.1 >= screen_position.1 + bound_h {
                break;
            }
            */

            let line_height = line.line_height();
            out.extend(line.aligned_on_screen(caret, HorizontalAlign::Left, VerticalAlign::Top));
            caret.1 += line_height;
        }

        out
    }

    /// This is a macro since traits with generics can't be made into objects.
    macro_rules! recalculate_glyphs_body {
        (
            $positioner: expr, $previous: expr, $change: expr, $fonts: expr, $geometry: expr, $sections: expr
        ) => {{
            let positioner = $positioner;
            let previous = $previous;
            let change = $change;
            let fonts = $fonts;
            let geometry = $geometry;
            let sections = $sections;
            match change {
                GlyphChange::Geometry(old) if old.bounds == geometry.bounds => {
                    // position change
                    let adjustment = vector(
                        geometry.screen_position.0 - old.screen_position.0,
                        geometry.screen_position.1 - old.screen_position.1,
                    );

                    let mut glyphs = previous.into_owned();
                    for (glyph, ..) in &mut glyphs {
                        let new_pos = glyph.position() + adjustment;
                        glyph.set_position(new_pos);
                    }

                    glyphs
                }
                GlyphChange::Color if !sections.is_empty() && !previous.is_empty() => {
                    let new_color = sections[0].color;
                    if sections.iter().all(|s| s.color == new_color) {
                        // if only the color changed, but the new section only use a single color
                        // we can simply set all the olds to the new color
                        let mut glyphs = previous.into_owned();
                        for (_, color, ..) in &mut glyphs {
                            *color = new_color;
                        }
                        glyphs
                    } else {
                        positioner.calculate_glyphs(fonts, geometry, sections)
                    }
                }
                GlyphChange::Alpha if !sections.is_empty() && !previous.is_empty() => {
                    let new_alpha = sections[0].color[3];
                    if sections.iter().all(|s| s.color[3] == new_alpha) {
                        // if only the alpha changed, but the new section only uses a single alpha
                        // we can simply set all the olds to the new alpha
                        let mut glyphs = previous.into_owned();
                        for (_, color, ..) in &mut glyphs {
                            color[3] = new_alpha;
                        }
                        glyphs
                    } else {
                        positioner.calculate_glyphs(fonts, geometry, sections)
                    }
                }
                _ => positioner.calculate_glyphs(fonts, geometry, sections),
            }
        }};
    }
    //
    //
    //
    impl GlyphPositioner for Wrap {
        fn calculate_glyphs<'font, F: FontMap<'font>>(
            &self,
            font_map: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText<'_>],
        ) -> Vec<(PositionedGlyph<'font>, Color, FontId)> {
            let SectionGeometry {
                screen_position,
                bounds: (bound_w, _),
                ..
            } = *geometry;

            calculate_glyphs(font_map, screen_position, bound_w, sections)
        }

        fn bounds_rect(&self, geometry: &SectionGeometry) -> Rect<f32> {
            let SectionGeometry {
                screen_position: (screen_x, screen_y),
                bounds: (bound_w, bound_h),
            } = *geometry;

            let (x_min, x_max) = HorizontalAlign::Left.x_bounds(screen_x, bound_w);
            let (y_min, _) = VerticalAlign::Top.y_bounds(screen_y, bound_h);
            let y_max = INFINITY; // never cut off the bottom of the text.

            Rect {
                min: point(x_min, y_min),
                max: point(x_max, y_max),
            }
        }

        #[allow(clippy::float_cmp)]
        fn recalculate_glyphs<'font, F>(
            &self,
            previous: Cow<'_, Vec<(PositionedGlyph<'font>, Color, FontId)>>,
            change: GlyphChange,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText<'_>],
        ) -> Vec<(PositionedGlyph<'font>, Color, FontId)>
        where
            F: FontMap<'font>,
        {
            recalculate_glyphs_body!(self, previous, change, fonts, geometry, sections)
        }
    }

    #[derive(Hash)]
    pub struct WrapInRect {
        pub rect: ScreenSpaceRect,
    }

    impl GlyphPositioner for WrapInRect {
        fn calculate_glyphs<'font, F>(
            &self,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
        where
            F: FontMap<'font>,
        {
            Wrap {}.calculate_glyphs(fonts, geometry, sections)
        }
        fn bounds_rect(&self, geometry: &SectionGeometry) -> Rect<f32> {
            let rect = self.rect;
            let mut wide_bounds = Wrap {}.bounds_rect(geometry);

            wide_bounds.min.x = if wide_bounds.min.x < rect.min.x {
                rect.min.x.into()
            } else {
                wide_bounds.min.x
            };
            wide_bounds.min.y = if wide_bounds.min.y < rect.min.y {
                rect.min.y.into()
            } else {
                wide_bounds.min.y
            };

            wide_bounds.max.x = if wide_bounds.max.x > rect.max.x {
                rect.max.x.into()
            } else {
                wide_bounds.max.x
            };
            wide_bounds.max.y = if wide_bounds.max.y > rect.max.y {
                rect.max.y.into()
            } else {
                wide_bounds.max.y
            };
            wide_bounds
        }

        fn recalculate_glyphs<'font, F>(
            &self,
            previous: Cow<Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>>,
            change: GlyphChange,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
        where
            F: FontMap<'font>,
        {
            recalculate_glyphs_body!(self, previous, change, fonts, geometry, sections)
        }
    }

    #[derive(Hash)]
    pub struct Unbounded {}

    impl GlyphPositioner for Unbounded {
        fn calculate_glyphs<'font, F>(
            &self,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
        where
            F: FontMap<'font>,
        {
            calculate_glyphs(fonts, geometry.screen_position, INFINITY, sections)
        }
        fn bounds_rect(&self, _: &SectionGeometry) -> Rect<f32> {
            INFINITY_RECT
        }
        fn recalculate_glyphs<'font, F>(
            &self,
            previous: Cow<Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>>,
            change: GlyphChange,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
        where
            F: FontMap<'font>,
        {
            recalculate_glyphs_body!(self, previous, change, fonts, geometry, sections)
        }
    }

    #[derive(Hash)]
    pub struct UnboundedLayoutClipped {
        clip: Rect<i32>,
        // only needed for the hash, so that the glyph_brush caching
        // works properly.
        scroll: ScrollXY
    }

    impl UnboundedLayoutClipped {
        pub fn new(clip_ssr: ScreenSpaceRect, scroll: ScrollXY) -> Self {
            Self { 
                clip: ssr_to_rusttype_i32(clip_ssr),
                scroll,
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
/*
    type UnboundedLine<'font> = (
        Vec<(RelativePositionedGlyph<'font>, [f32; 4], FontId)>,
        VMetrics
    );

    const V_METRICS_ZERO: VMetrics = VMetrics {
        ascent: 0.0,
        descent: 0.0,
        line_gap: 0.0,
    };

    fn get_unbounded_line_glyphs_iter<'a, 'b, 'font, F>(
        fonts: &'b F,
        sections: &'a [SectionText],
    ) -> impl IntoIterator<
        Item = UnboundedLine<'font>,
        IntoIter = std::vec::IntoIter<UnboundedLine<'font>>
    >
    where
        'font: 'a + 'b,
        F: FontMap<'font>, {
        let mut output: Vec<UnboundedLine> = Vec::with_capacity(sections.len());

        let mut current_line = Vec::with_capacity(16);
        let mut current_x = 0.0;
        let mut current_y = 0.0;
        let mut last_glyph_id = None;
        let mut max_v_metrics = V_METRICS_ZERO;

        macro_rules! push_line {
            () => {
                if current_line.len() > 0 {
                    output.push((current_line, max_v_metrics));
                }
                current_line = Vec::with_capacity(16);
                current_x = 0.0;
                current_y = 0.0;
                last_glyph_id = None;
                max_v_metrics = V_METRICS_ZERO;
            }
        }

        for section in sections.iter() {
            let mut chars = section.text.chars().peekable();
            while let Some(c) = chars.next() {
                if c == '\r' && chars.peek() == Some(&'\n') {
                    push_line!();
                    chars.next();
                } else if is_linebreak_char(c) {
                    push_line!();
                } else {
                    let font = fonts
                        .font(section.font_id);

                    let glyph = font
                        .glyph(c)
                        .scaled(section.scale);

                    let v_metrics = font.v_metrics(glyph.scale());
                    if v_metrics.ascent > max_v_metrics.ascent {
                        let diff_y = v_metrics.ascent - current_y;
                        current_y += diff_y;
        
                        // modify all smaller lined glyphs to occupy the new larger line
                        for (glyph, ..) in &mut current_line {
                            glyph.relative.y += diff_y;
                        }
        
                        max_v_metrics = v_metrics;
                    }

                    if let Some(id) = last_glyph_id.take() {
                        current_x += font.pair_kerning(section.scale, id, glyph.id());
                    }
                    last_glyph_id = Some(glyph.id());

                    let advance_width = glyph.h_metrics().advance_width;

                    if !c.is_whitespace() {
                        let rel_glyph = RelativePositionedGlyph {
                            relative: point(current_x, current_y),
                            glyph,
                        };
                    
                        current_line.push(
                            (rel_glyph, section.color, section.font_id),
                        );
                    }


                    current_x += advance_width;
                }
            }     
        }
        
        push_line!();

        output
    }
*/
    // This is a separate function to aid in testing
    pub fn calculate_glyphs_unbounded_layout_clipped<'font, F>(
        clip: Rect<i32>,
        fonts: &F,
        geometry: &SectionGeometry,
        sections: &[SectionText],
    ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
    where
        F: FontMap<'font>, {
        perf_viz::record_guard!("UnboundedLayoutClipped::calculate_glyphs");
        
        perf_viz::start_record!("has_multiple_scales_or_fonts");
        let has_multiple_scales_or_fonts = !sections.windows(2)
            .all(|w| 
                w[0].scale == w[1].scale
                && w[0].font_id == w[1].font_id
            );
        perf_viz::end_record!("has_multiple_scales_or_fonts");

        if has_multiple_scales_or_fonts {
            return calculate_glyphs_unbounded_layout_clipped_slow(
                clip,
                fonts,
                geometry,
                sections,
            );
        }

        let mut out = vec![];

        if sections.len() == 0 {
            return out;
        }

        let (font_id, scale) = (sections[0].font_id, sections[0].scale);

        let font = fonts.font(font_id);

        let lines_vec = get_unbounded_line_glyphs_iter(fonts, sections);

        let mut lines = lines_vec.into_iter();

        perf_viz::record_guard!("UnboundedLayoutClipped loop");

        if let Some(mut glyphs) = lines.next() {
            perf_viz::start_record!("UnboundedLayoutClipped loop prep");

            let v_metrics = font.v_metrics(scale);
            let line_height: f32 = v_metrics.ascent - v_metrics.descent + v_metrics.line_gap;
            
            let mut min_y: f32 = clip.min.y as f32 - line_height - line_height;
            let mut max_y: f32 = clip.max.y as f32 + line_height + line_height;

            perf_viz::end_record!("UnboundedLayoutClipped loop prep");

            if line_height <= 0.0 || !(min_y <= max_y) {
                dbg!(line_height <= 0.0 || !(min_y <= max_y));
                return out;
            }

            let mut caret = geometry.screen_position;
            
            perf_viz::start_record!("lines.nth");
            // This should only mean we get slower past 65536 lines,
            // not that we don't display them.
            // TODO test this.
            let to_skip = -(caret.1 - min_y) / line_height;
            let to_skip: u16 = if to_skip > 65535.0 {
                65535.0
            } else if to_skip >= 0.0 {
                to_skip
            } else {
                // NaN ends up here
                0.0
            } as u16;

            if to_skip > 0 {
                caret.1 = caret.1 + f32::from(to_skip) * line_height;
                if let Some(g) = lines.nth((to_skip - 1) as _) {
                    glyphs = g;
                } else {
                    perf_viz::end_record!("lines.nth");
                    return out;
                }
            }

            perf_viz::end_record!("lines.nth");

            loop {
                let new_caret_height = caret.1 + line_height;
                
                // we assume that the lines are sorted from top to bottom
                // TODO simplify additions/subtractions
                if caret.1 < min_y {
                    // just run the part after the if elses.
                } else if new_caret_height >= max_y {
                    break
                } else {
                    perf_viz::start_record!("glyph.screen_positioned");
                    let screen_pos = point(caret.0, caret.1);

                    let tuples = glyphs
                        .into_iter()
                        .map(|(glyph, color, font_id)| 
                            (glyph.screen_positioned(screen_pos), color, font_id)
                        );
                    perf_viz::end_record!("glyph.screen_positioned");
        
                    perf_viz::start_record!("out.extend");
                    out.extend(
                        tuples
                            .into_iter()
                            .filter(|(glyph, _, _): &(PositionedGlyph<'_>, [f32; 4], FontId)| {
                                // TODO when is this None?
                                let should_keep = glyph.pixel_bounding_box()
                                    .map(move |pixel_coords| {
                                        // true if pixel_coords intersects clip
                                        pixel_coords.min.x <= clip.max.x
                                        && pixel_coords.min.y <= clip.max.y
                                        && clip.min.x <= pixel_coords.max.x
                                        && clip.min.y <= pixel_coords.max.y
                                    })
                                    .unwrap_or(true);
        
                                should_keep
                            })
                    );
                    perf_viz::end_record!("out.extend");
                }

                caret.1 = new_caret_height;
                
                perf_viz::start_record!("lines.next()");
                if let Some(g) = lines.next() {
                    glyphs = g;
                    perf_viz::end_record!("lines.next()");
                } else {
                    perf_viz::end_record!("lines.next()");
                    break
                }
            }
        }
    
        out
    }

    impl GlyphPositioner for UnboundedLayoutClipped {
        fn calculate_glyphs<'font, F>(
            &self,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
        where
            F: FontMap<'font>,
        {
            calculate_glyphs_unbounded_layout_clipped(
                self.clip,
                fonts,
                geometry,
                sections,
            )
        }
        fn bounds_rect(&self, _: &SectionGeometry) -> Rect<f32> {
            INFINITY_RECT
        }
        fn recalculate_glyphs<'font, F>(
            &self,
            previous: Cow<Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>>,
            change: GlyphChange,
            fonts: &F,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<(PositionedGlyph<'font>, [f32; 4], FontId)>
        where
            F: FontMap<'font>,
        {
            recalculate_glyphs_body!(self, previous, change, fonts, geometry, sections)
        }
    }
}
use text_layouts::{Unbounded, UnboundedLayoutClipped, Wrap, WrapInRect};

/// As of this writing, casting f32 to i32 is undefined behaviour if the value does not fit!
/// https://github.com/rust-lang/rust/issues/10184
fn f32_to_i32_or_max(f: f32) -> i32 {
    if f >= i32::min_value() as f32 && f <= i32::max_value() as f32 {
        f as i32
    } else {
        // make this the default so NaN etc. end up here
        i32::max_value()
    }
}

pub type TextureRect = glyph_brush::rusttype::Rect<u32>;

pub struct State<'font> {
    pub glyph_brush: GlyphBrush<'font, Vertex>,
    pub hidpi_factor: f32,
}

impl <'font> State<'font> {
    pub fn set_dimensions(&mut self, hidpi_factor: f32) {
        self.hidpi_factor = hidpi_factor;
    
        // if we don't reset the cache like this then we render a stretched
        // version of the text on window resize.
        let (t_w, t_h) = self.glyph_brush.texture_dimensions();
        self.glyph_brush.resize_texture(t_w, t_h);
    }
    pub fn texture_dimensions(&self) -> (u32, u32) {
        self.glyph_brush.texture_dimensions()
    }

    pub fn resize_texture(&mut self, new_width: u32, new_height: u32) {
        self.glyph_brush.resize_texture(new_width, new_height);
    }

    #[perf_viz::record]
    pub fn render_vertices<Update, Resize>(
        &mut self,
        text_or_rects: Vec<TextOrRect>,
        dimensions: (u32, u32),
        update_texture: Update,
        mut resize_texture: Resize,
    ) -> Option<Vec<Vertex>>
    where
        for <'r> Update: FnMut(TextureRect, &'r [u8]) + Copy,
        Resize: FnMut(u32, u32) + Copy,
    {
        use TextOrRect::*;

        let mut rect_specs = Vec::new();

        perf_viz::start_record!("for &text_or_rects");
        for t_or_r in text_or_rects {
            use gl_layer_types::*;

            let glyph_brush = &mut self.glyph_brush;

            macro_rules! queue {
                ($layout: ident, $section: ident) => {
                    perf_viz::start_record!("queue!");
                    match $layout {
                        TextLayout::SingleLine => glyph_brush.queue($section),
                        TextLayout::Wrap => glyph_brush.queue_custom_layout($section, &Wrap {}),
                        TextLayout::WrapInRect(rect) => {
                            glyph_brush.queue_custom_layout($section, &WrapInRect { rect })
                        }
                        TextLayout::Unbounded => {
                            glyph_brush.queue_custom_layout($section, &Unbounded {})
                        }
                        TextLayout::UnboundedLayoutClipped(ssr, scroll) => {
                            glyph_brush.queue_custom_layout($section, &UnboundedLayoutClipped::new(ssr, scroll))
                        }
                    };
                    perf_viz::end_record!("queue!");
                }
            }

            match t_or_r {
                Rect(VisualSpec {
                    rect,
                    color,
                    z,
                }) => {
                    perf_viz::start_record!("Rect");
                    let pixel_coords: PixelCoords = ssr_to_rusttype_i32(rect);

                    let ssr!(_, _, max_x, max_y) = rect;    

                    let mut bounds: Bounds = d!();
                    bounds.max.x = max_x.into();
                    bounds.max.y = max_y.into();
                    rect_specs.push(RectSpec {
                        pixel_coords,
                        bounds,
                        color,
                        z: z_to_f32(z),
                    });
                    perf_viz::end_record!("Rect");
                }
                Text(TextSpec {
                    text,
                    size,
                    layout,
                    spec: VisualSpec { rect, color, z },
                }) => {
                    perf_viz::start_record!("Text");
                    let section = Section {
                        text: &text,
                        scale: get_scale(size, self.hidpi_factor),
                        screen_position: rect.min.into(),
                        bounds: rect.max.into(),
                        color,
                        layout: match layout {
                            TextLayout::SingleLine => Layout::default_single_line(),
                            TextLayout::Wrap 
                            | TextLayout::WrapInRect(_) 
                            | TextLayout::Unbounded 
                            | TextLayout::UnboundedLayoutClipped(_, _) => {
                                Layout::default_wrap()
                            }
                        },
                        z: z_to_f32(z),
                        ..d!()
                    };
    
                    queue!(layout, section);
                    perf_viz::end_record!("Text");
                }
                MulticolourText(MulticolourTextSpec{
                    size,
                    layout,
                    rect,
                    z,
                    text,
                }) => {
                    perf_viz::start_record!("MulticolourText");
                    let scale = get_scale(size, self.hidpi_factor);
                    let section = VariedSection {
                        screen_position: rect.min.into(),
                        bounds: rect.max.into(),
                        layout: match layout {
                            TextLayout::SingleLine => Layout::default_single_line(),
                            TextLayout::Wrap 
                            | TextLayout::WrapInRect(_) 
                            | TextLayout::Unbounded 
                            | TextLayout::UnboundedLayoutClipped(_, _) => {
                                Layout::default_wrap().line_breaker(glyph_brush::BuiltInLineBreaker::AnyCharLineBreaker)
                            }
                        },
                        z: z_to_f32(z),
                        text: text.iter().map(|ColouredText { text, colour }| {
                            SectionText {
                                text: &text,
                                scale,
                                color: *colour,
                                ..d!()
                            }
                        }).collect(),
                        ..d!()
                    };
    
                    queue!(layout, section);
                    perf_viz::end_record!("MulticolourText");
                }
            }
        }
        perf_viz::end_record!("for &text_or_rects");

        loop {
            let brush_action = self.glyph_brush.process_queued(
                update_texture,
                to_vertex_maker((dimensions.0 as f32, dimensions.1 as f32)),
                Some(AdditionalRects {
                    set_alpha,
                    rect_specs: rect_specs.clone(), // clone needed since we loop sometimes.
                }),
            );
    
            match brush_action {
                Ok(BrushAction::Draw(verticies)) => return Some(verticies),
                Ok(BrushAction::ReDraw) => return None,
                Err(BrushError::TextureTooSmall { suggested: (new_width, new_height), .. }) => {
                    perf_viz::record_guard!("TextureTooSmall");
                    resize_texture(new_width, new_height);
                    self.resize_texture(new_width, new_height);
                },
            }
        }
    }
}

pub type CharDims = Vec<CharDim>;

const FONT_BYTES: &[u8] = include_bytes!("./fonts/FiraCode-Retina.ttf");

pub fn new(hidpi_factor: f32, text_sizes: &[f32]) -> Res<(State, CharDims)> {

    let font = Font::from_bytes(FONT_BYTES)?;
    macro_rules! get_char_dim {
        ($scale:expr) => {{
            let scale = $scale;
            char_dim!({
                    // We currently assume the font is monospaced.
                    let em_space_char = '\u{2003}';
                    let h_metrics = font.glyph(em_space_char).scaled(scale).h_metrics();

                    h_metrics.advance_width
                },
                {
                    let v_metrics = font.v_metrics(scale);

                    v_metrics.ascent + -v_metrics.descent + v_metrics.line_gap
                },
            )
        }};
    }

    let mut char_dims = Vec::with_capacity(text_sizes.len());

    for size in text_sizes {
        char_dims.push(get_char_dim!(get_scale(*size, hidpi_factor)));
    }

    let glyph_brush = get_glyph_brush(&font);

    return Ok((
        State {
            glyph_brush,
            hidpi_factor,
        },
        char_dims
    ))
}

fn get_glyph_brush<'font, A: Clone>(font: &Font<'font>) -> GlyphBrush<'font, A> {
    GlyphBrushBuilder::using_font(font.clone())
        // Leaving this at the default of 0.1 makes the cache get cleared too often.
        // Putting this at 1.0 means that the characters are visibly poorly kerned.
        // This value seems like a happy medium at the moment.
        .gpu_cache_position_tolerance(0.25)
        .build()
}

fn get_scale(size: f32, hidpi_factor: f32) -> Scale {
    Scale::uniform((size * hidpi_factor).round())
}



#[inline]
#[perf_viz::record]
fn to_vertex_maker((screen_w, screen_h): (f32, f32)) -> impl Fn(glyph_brush::GlyphVertex) -> Vertex + Copy {
    move |glyph_brush::GlyphVertex {
        mut tex_coords,
        pixel_coords,
        bounds,
        color,
        z,
    }: glyph_brush::GlyphVertex| {
        perf_viz::record_guard!("to_vertex");
        let gl_bounds = rusttype::Rect {
            min: rusttype::point(
                2.0 * (bounds.min.x / screen_w - 0.5),
                2.0 * (0.5 - bounds.min.y / screen_h),
            ),
            max: rusttype::point(
                2.0 * (bounds.max.x / screen_w - 0.5),
                2.0 * (0.5 - bounds.max.y / screen_h),
            ),
        };
    
        let mut gl_rect = rusttype::Rect {
            min: rusttype::point(
                2.0 * (pixel_coords.min.x as f32 / screen_w - 0.5),
                2.0 * (0.5 - pixel_coords.min.y as f32 / screen_h),
            ),
            max: rusttype::point(
                2.0 * (pixel_coords.max.x as f32 / screen_w - 0.5),
                2.0 * (0.5 - pixel_coords.max.y as f32 / screen_h),
            ),
        };
    
        // handle overlapping bounds, modify uv_rect to preserve texture aspect
        if gl_rect.max.x > gl_bounds.max.x {
            let old_width = gl_rect.width();
            gl_rect.max.x = gl_bounds.max.x;
            tex_coords.max.x = tex_coords.min.x + tex_coords.width() * gl_rect.width() / old_width;
        }
        if gl_rect.min.x < gl_bounds.min.x {
            let old_width = gl_rect.width();
            gl_rect.min.x = gl_bounds.min.x;
            tex_coords.min.x = tex_coords.max.x - tex_coords.width() * gl_rect.width() / old_width;
        }
        // note: y access is flipped gl compared with screen,
        // texture is not flipped (ie is a headache)
        if gl_rect.max.y < gl_bounds.max.y {
            let old_height = gl_rect.height();
            gl_rect.max.y = gl_bounds.max.y;
            tex_coords.max.y = tex_coords.min.y + tex_coords.height() * gl_rect.height() / old_height;
        }
        if gl_rect.min.y > gl_bounds.min.y {
            let old_height = gl_rect.height();
            gl_rect.min.y = gl_bounds.min.y;
            tex_coords.min.y = tex_coords.max.y - tex_coords.height() * gl_rect.height() / old_height;
        }
    
        VertexStruct {
            left_top_x: gl_rect.min.x,
            left_top_y: gl_rect.max.y,
            left_top_z: z,
            override_alpha: 0.0,
            right_bottom_x: gl_rect.max.x,
            right_bottom_y: gl_rect.min.y,
            // this isn't `mix.x, min.y, max.x, max.y` in order to flip the y axis
            tex_left_top_x: tex_coords.min.x,
            tex_left_top_y: tex_coords.max.y,
            tex_right_bottom_x: tex_coords.max.x,
            tex_right_bottom_y: tex_coords.min.y,
            color_r: color[0],
            color_g: color[1],
            color_b: color[2],
            color_a: color[3],
        }.into_vertex()
    }
}

fn ssr_to_rusttype_i32(ssr!(min_x, min_y, max_x, max_y): ScreenSpaceRect) -> Rect<i32> {
    let mut rect: Rect<i32> = d!();
    rect.min.x = min_x.trunc_to_i32();
    rect.min.y = min_y.trunc_to_i32();
    rect.max.x = max_x.trunc_to_i32();
    rect.max.y = max_y.trunc_to_i32();
    rect
}

#[cfg(any(test, feature = "pub_arb"))]
mod tests;