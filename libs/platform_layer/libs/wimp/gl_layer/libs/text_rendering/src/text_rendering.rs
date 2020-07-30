#![deny(unused)]
use platform_types::{
    screen_positioning::{CharDim, ScreenSpaceXY, ScreenSpaceRect, ScrollXY},
    char_dim, ssr,
};
use gl_layer_types::{Vertex, VertexStruct, set_alpha, TextOrRect, Res};

#[allow(unused_imports)]
use macros::{d, dbg};

use glyph_brush::*;
use glyph_brush::{
    rusttype::{Font, Scale, Rect},
    Bounds, GlyphBrush, GlyphBrushBuilder, RectSpec, PixelCoords, Section,
};

mod text_layouts {
    use super::*;
    use macros::{dbg};
    use glyph_brush::{
        rusttype::{point, vector},
    };
    use std::borrow::Cow;

    fn calculate_glyphs<'font>(
        font: &Font<'font>,
        scale: Scale,
        mut caret: (f32, f32),
        sections: &[SectionText<'_>],
    ) -> Vec<CalculatedGlyph<'font>> {
        let mut out = vec![];

        let lines = unbounded::get_lines_iter(font, scale, sections);
        
        let v_metrics = font.v_metrics(scale);
        let line_height: f32 = v_metrics.ascent - v_metrics.descent + v_metrics.line_gap;

        for line in lines {
            if line.glyphs.len() > 0 {
                let screen_pos = point(caret.0, caret.1);
    
                out.extend(
                    line.glyphs
                        .into_iter()
                        .map(|(glyph, color)| 
                            (glyph.screen_positioned(screen_pos), color)
                        )
                );
            }
            caret.1 += line_height;
        }

        out
    }

    /// This is a macro since traits with generics can't be made into objects.
    macro_rules! recalculate_glyphs_body {
        (
            $positioner: expr, $previous: expr, $change: expr, $fonts: expr, $scale: expr, $geometry: expr, $sections: expr
        ) => {{
            let positioner = $positioner;
            let previous = $previous;
            let change = $change;
            let fonts = $fonts;
            let scale = $scale;
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
                    // even if the colour changed only slightly, we still want to do a fresh calculation
                    #[allow(clippy::float_cmp)]
                    if sections.iter().all(|s| s.color == new_color) {
                        // if only the color changed, but the new section only use a single color
                        // we can simply set all the olds to the new color
                        let mut glyphs = previous.into_owned();
                        for (_, color, ..) in &mut glyphs {
                            *color = new_color;
                        }
                        glyphs
                    } else {
                        positioner.calculate_glyphs(fonts, scale, geometry, sections)
                    }
                }
                GlyphChange::Alpha if !sections.is_empty() && !previous.is_empty() => {
                    let new_alpha = sections[0].color[3];
                    // even if the alpha changed only slightly, we still want to do a fresh calculation
                    #[allow(clippy::float_cmp)]
                    if sections.iter().all(|s| s.color[3] == new_alpha) {
                        // if only the alpha changed, but the new section only uses a single alpha
                        // we can simply set all the olds to the new alpha
                        let mut glyphs = previous.into_owned();
                        for (_, color, ..) in &mut glyphs {
                            color[3] = new_alpha;
                        }
                        glyphs
                    } else {
                        positioner.calculate_glyphs(fonts, scale, geometry, sections)
                    }
                }
                _ => positioner.calculate_glyphs(fonts, scale, geometry, sections),
            }
        }};
    }
    //
    //
    //
    #[derive(Hash)]
    pub(crate) struct Unbounded {}

    impl GlyphPositioner for Unbounded {
        fn calculate_glyphs<'font>(
            &self,
            font: &Font<'font>,
            scale: Scale,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<CalculatedGlyph<'font>>
        {
            calculate_glyphs(font, scale, geometry.screen_position, sections)
        }
        fn recalculate_glyphs<'font>(
            &self,
            previous: Cow<Vec<CalculatedGlyph<'font>>>,
            change: GlyphChange,
            fonts: &Font<'font>,
            scale: Scale,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<CalculatedGlyph<'font>>
        {
            recalculate_glyphs_body!(self, previous, change, fonts, scale, geometry, sections)
        }
    }

    #[derive(Hash)]
    pub(crate) struct UnboundedLayoutClipped {
        clip: Rect<i32>,
        // only needed for the hash, so that the glyph_brush caching
        // works properly.
        scroll: ScrollXY
    }

    impl UnboundedLayoutClipped {
        pub(crate) fn new(clip_ssr: ScreenSpaceRect, scroll: ScrollXY) -> Self {
            Self { 
                clip: ssr_to_rusttype_i32(clip_ssr),
                scroll,
            }
        }
    }

    // This is a separate function to aid in testing
    pub(crate) fn calculate_glyphs_unbounded_layout_clipped<'font>(
        clip: Rect<i32>,
        font: &Font<'font>,
        scale: Scale,
        geometry: &SectionGeometry,
        sections: &[SectionText],
    ) -> Vec<CalculatedGlyph<'font>>
    {
        perf_viz::record_guard!("UnboundedLayoutClipped::calculate_glyphs");
        let mut out = vec![];

        if sections.len() == 0 {
            return out;
        }

        let mut lines = unbounded::get_lines_iter(font, scale, sections);

        perf_viz::record_guard!("UnboundedLayoutClipped loop");

        if let Some(mut line) = lines.next() {
            perf_viz::start_record!("UnboundedLayoutClipped loop prep");

            let v_metrics = font.v_metrics(scale);
            let line_height: f32 = v_metrics.ascent - v_metrics.descent + v_metrics.line_gap;
            
            let min_y: f32 = clip.min.y as f32 - line_height - line_height;
            let max_y: f32 = clip.max.y as f32 + line_height + line_height;

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
                if let Some(l) = lines.nth((to_skip - 1) as _) {
                    line = l;
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

                    let tuples = line
                        .glyphs
                        .into_iter()
                        .map(|(glyph, color)| 
                            (glyph.screen_positioned(screen_pos), color)
                        );
                    perf_viz::end_record!("glyph.screen_positioned");
        
                    perf_viz::start_record!("out.extend");
                    out.extend(
                        tuples
                            .into_iter()
                            .filter(|(glyph, _): &CalculatedGlyph<'_>| {
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
                if let Some(l) = lines.next() {
                    line = l;
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
        fn calculate_glyphs<'font>(
            &self,
            font: &Font<'font>,
            scale: Scale,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<CalculatedGlyph<'font>>
        {
            calculate_glyphs_unbounded_layout_clipped(
                self.clip,
                font,
                scale,
                geometry,
                sections,
            )
        }
        fn recalculate_glyphs<'font>(
            &self,
            previous: Cow<Vec<CalculatedGlyph<'font>>>,
            change: GlyphChange,
            font: &Font<'font>,
            scale: Scale,
            geometry: &SectionGeometry,
            sections: &[SectionText],
        ) -> Vec<CalculatedGlyph<'font>>
        {
            recalculate_glyphs_body!(self, previous, change, font, scale, geometry, sections)
        }
    }
}
use text_layouts::{Unbounded, UnboundedLayoutClipped};

pub type TextureRect = glyph_brush::rusttype::Rect<u32>;

pub struct State<'font> {
    pub(crate) glyph_brush: GlyphBrush<'font, Vertex>,
    pub(crate) hidpi_factor: f32,
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

    pub(crate) fn resize_texture(&mut self, new_width: u32, new_height: u32) {
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
                        scale,
                        z: z_to_f32(z),
                        text: text.iter().map(|ColouredText { text, colour }| {
                            SectionText {
                                text: &text,
                                color: *colour,
                            }
                        }).collect(),
                        font_id: d!(),
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

pub(crate) type CharDims = Vec<CharDim>;

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

mod unbounded {
    use glyph_brush::{
        rusttype::{
            ScaledGlyph,
            point,
            vector,
            Scale,
            Font,
        },
        RelativePositionedGlyph,
        SectionText,
        Color,
    };
    use linebreak::{
        Linebreak,
        LinebreakIter,
    };
    #[perf_viz::record]
    pub(crate) fn get_lines_iter<'a, 'b, 'font>(
        font: &'b Font<'font>,
        scale: Scale,
        sections: &'a [SectionText<'a>],
    ) -> UnboundedLines<'a, 'b, 'font>
    where
        'font: 'a + 'b,
    {
        Characters::new(
            font,
            scale,
            sections.iter(),
        )
        .lines()
    }
    
    pub(crate) struct UnboundedLine<'font> {
        pub(crate) glyphs: Vec<(RelativePositionedGlyph<'font>, [f32; 4])>,
    }
    
    pub(crate) struct UnboundedLines<'a, 'b, 'font>
    where
        'font: 'a + 'b,
    {
        characters: Characters<'a, 'b, 'font>,
    }
    
    impl<'font> Iterator for UnboundedLines<'_, '_, 'font> {
        type Item = UnboundedLine<'font>;
    
        fn next(&mut self) -> Option<Self::Item> {
            let characters = &self.characters;
            let mut caret = vector(
                0.0,
                characters.font.v_metrics(characters.scale).ascent
            );
            let mut line: UnboundedLine = UnboundedLine{
                glyphs: Vec::new(),
            };
    
            let mut line_progressed = false;

            loop {
                let mut layout_width = 0.0;
                let mut last_glyph_id = None;
                let mut hard_break = false;
                let mut character_progressed = false;
        
                let font = self.characters.font;
                let scale = self.characters.scale;
                for Character {
                    glyph,
                    color,
                    is_linebreak,
                    control,
                } in &mut self.characters
                {
                    character_progressed = true;
                    {
                        if let Some(id) = last_glyph_id.take() {
                            layout_width += font.pair_kerning(scale, id, glyph.id());
                        }
                        last_glyph_id = Some(glyph.id());
                    }
        
                    if !control {
                        let advance_width = glyph.h_metrics().advance_width;

                        let mut positioned = RelativePositionedGlyph {
                            relative: point(layout_width, 0.0),
                            glyph,
                        };
        
                        layout_width += advance_width;
        
                        if positioned.bounds().is_some() {
                            positioned.relative = positioned.relative + caret;
                            line.glyphs.push((positioned, color));
                        }
                    }
        
                    if is_linebreak {
                        hard_break = true;
                        break;
                    }
                }
        
                if character_progressed {
                    line_progressed = true;
                    caret.x += layout_width;
                    if hard_break {
                        break
                    }
                } else {
                    break
                }
            }
    
            Some(line).filter(|_| line_progressed)
        }
    }
    
    /// `Character` iterator
    struct Characters<'a, 'b, 'font>
    where
        'font: 'a + 'b,
    {
        font: &'b Font<'font>,
        scale: Scale,
        section_text: slice::Iter<'a, SectionText<'a>>,
        part_info: Option<PartInfo<'a>>,
    }
    
    struct PartInfo<'a> {
        section: &'a SectionText<'a>,
        info_chars: CharIndices<'a>,
        linebreaks: LinebreakIter<'a>,
        next_break: Option<Linebreak>,
    }
    
    use std::{
        iter::{Iterator},
        slice,
        str::CharIndices,
    };
    
    impl<'a, 'b, 'font> Characters<'a, 'b, 'font>
    {
        /// Returns a new `Characters` iterator.
        fn new(
            font: &'b Font<'font>,
            scale: Scale,
            section_text: slice::Iter<'a, SectionText<'a>>,
        ) -> Self {
            Self {
                font,
                scale,
                section_text,
                part_info: None,
            }
        }
    
        fn lines(self) -> UnboundedLines<'a, 'b, 'font> {
            UnboundedLines {
                characters: self,
            }
        }
    }
    
    impl<'font> Iterator for Characters<'_, '_, 'font>
    {
        type Item = Character<'font>;
    
        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            if self.part_info.is_none() {
                let section = self.section_text.next()?;
                self.part_info = Some(PartInfo {
                    section,
                    info_chars: section.text.char_indices(),
                    linebreaks: linebreak::iter(section.text),
                    next_break: None,
                });
            }
    
            {
                let PartInfo {
                    section:
                        SectionText {
                            color,
                            text,
                        },
                    info_chars,
                    linebreaks,
                    next_break,
                } = self.part_info.as_mut().unwrap();
    
                if let Some((byte_index, c)) = info_chars.next() {
                    if next_break.is_none() || next_break.unwrap().offset() <= byte_index {
                        loop {
                            let next = linebreaks.next();
                            if next.is_none() || next.unwrap().offset() > byte_index {
                                *next_break = next;
                                break;
                            }
                        }
                    }
    
                    let glyph = self.font.glyph(c).scaled(self.scale);
    
                    let c_len = c.len_utf8();
                    let mut linebreak = next_break.filter(|b| b.offset() == byte_index + c_len);
                    if linebreak.is_some() && byte_index + c_len == text.len() {
                        // handle inherent end-of-str hard breaks

                        // to check if the previous end char (say '$') should hard break construct
                        // a str "$ " an check if the line break logic flags a hard break at index 1
                        let mut last_end_bytes: [u8; 5] = [b' '; 5];
                        c.encode_utf8(&mut last_end_bytes);
                        linebreak = if let Ok(last_end_padded) = std::str::from_utf8(&last_end_bytes[0..=c_len]) {
                            linebreak::iter(last_end_padded).next()
                        } else {
                            None
                        };
                    }

                    let is_linebreak = if let Some(Linebreak::Hard(..)) = linebreak {
                        true
                    } else {
                        false
                    };
    
                    return Some(Character {
                        glyph,
                        color: *color,
                        is_linebreak,
                        control: c.is_control(),
                    });
                }
            }
    
            self.part_info = None;
            self.next()
        }
    }
    
    /// Single character info
    struct Character<'font> {
        glyph: ScaledGlyph<'font>,
        color: Color,
        is_linebreak: bool,
        /// Equivalent to `char::is_control()`.
        control: bool,
    }
}

#[cfg(any(test, feature = "pub_arb"))]
mod tests;