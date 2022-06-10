#![deny(unused)]
use screen_space::{
    CharDim,
    char_dim, ssr,
};
use gl_layer_types::{Dimensions, Vertex, VertexStruct, set_alpha, TextOrRect, Res};

#[allow(unused_imports)]
use macros::{d, dbg};

use glyph_brush::{
    Rect,
    Font, Scale,
    Bounds, BrushAction, BrushError, GlyphBrush,
    RectSpec, PixelCoords, Section, SectionText, VariedSection,
    AdditionalRects, GlyphVertex,
    new_glyph,
    get_advance_width,
    get_line_height,
    point,
};

mod text_layouts {
    use super::unbounded;
    use macros::{d, dbg};
    use screen_space::{
        ScreenSpaceRect,
        ssr,
    };
    use glyph_brush::{
        Font, Scale, Rect, point,
        CalculatedGlyph,
        GlyphPositioner,
        SectionGeometry,
        SectionText,
        add_position,
        get_line_height,
        intersects,
    };

    pub fn ssr_to_glyph_brush_rect(ssr!(min_x, min_y, max_x, max_y): ScreenSpaceRect) -> Rect {
        let mut rect: Rect = d!();
        rect.min.x = min_x.into();
        rect.min.y = min_y.into();
        rect.max.x = max_x.into();
        rect.max.y = max_y.into();
        rect
    }

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
            let mut out = vec![];
            let mut caret = geometry.screen_position;

            let lines = unbounded::get_lines_iter(font, scale, sections);
            
            let line_height: f32 = get_line_height(font, scale);
    
            for line in lines {
                if !line.glyphs.is_empty() {
                    let screen_pos = point(caret.0, caret.1);
        
                    out.extend(
                        line.glyphs
                            .into_iter()
                            .map(|(mut glyph, colour)| {
                                add_position(&mut glyph, screen_pos);
                                CalculatedGlyph{glyph, colour}
                            })
                    );
                }
                caret.1 += line_height;
            }
    
            out
        }
    }

    pub(crate) struct UnboundedLayoutClipped {
        clip: Rect,
        // Needed for when this struct is hashed, so that the characters are 
        // adjusted properly when the scroll changes.
        scroll_hash: u64
    }

    impl core::hash::Hash for UnboundedLayoutClipped {
        fn hash<H: core::hash::Hasher>(&self, hasher: &mut H) {
            self.clip.min.x.to_bits().hash(hasher);
            self.clip.min.y.to_bits().hash(hasher);
            self.clip.max.x.to_bits().hash(hasher);
            self.clip.max.y.to_bits().hash(hasher);

            self.scroll_hash.hash(hasher);
        }
    }

    impl UnboundedLayoutClipped {
        pub(crate) fn new<H: core::hash::Hash>(clip_ssr: ScreenSpaceRect, scroll: H) -> Self {
            // TODO use rustc hash after moving into glyph_brush
            use core::hash::{Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            scroll.hash(&mut hasher);
            Self { 
                clip: ssr_to_glyph_brush_rect(clip_ssr),
                scroll_hash: hasher.finish(),
            }
        }
    }

    // This is a separate function to aid in testing
    pub(crate) fn calculate_glyphs_unbounded_layout_clipped<'font>(
        clip: Rect,
        font: &Font<'font>,
        scale: Scale,
        geometry: &SectionGeometry,
        sections: &[SectionText],
    ) -> Vec<CalculatedGlyph<'font>>
    {
        perf_viz::record_guard!("UnboundedLayoutClipped::calculate_glyphs");
        let mut out = vec![];

        if sections.is_empty() {
            return out;
        }

        let mut lines = unbounded::get_lines_iter(font, scale, sections);

        perf_viz::record_guard!("UnboundedLayoutClipped loop");

        if let Some(mut line) = lines.next() {
            perf_viz::start_record!("UnboundedLayoutClipped loop prep");

            let line_height: f32 = get_line_height(font, scale);
            
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
            // The allow is here instead of above the `as` because of
            // "error[E0658]: attributes on expressions are experimental"
            #[allow(clippy::cast_possible_truncation)]
            let to_skip: u16 = if to_skip > 65535.0 {
                65535
            } else if to_skip >= 0.0 {
                // We just checked if it was too high
                to_skip as u16
            } else {
                // NaN ends up here
                0
            };

            if to_skip > 0 {
                caret.1 += f32::from(to_skip) * line_height;
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
                if caret.1 < min_y {
                    // just run the part after the if elses.
                } else if new_caret_height >= max_y {
                    break
                } else {
                    perf_viz::start_record!("glyph.screen_positioned");
                    let screen_pos = point(caret.0, caret.1);

                    let cgs = line
                        .glyphs
                        .into_iter()
                        .map(|(mut glyph, colour)| {
                            add_position(&mut glyph, screen_pos);
                            CalculatedGlyph{glyph, colour}
                        });
                    perf_viz::end_record!("glyph.screen_positioned");
        
                    perf_viz::start_record!("out.extend");
                    out.extend(
                        cgs
                            .filter(|cg: &CalculatedGlyph<'_>| {
                                intersects(font, &cg.glyph, &clip)
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
    }
}
use text_layouts::{Unbounded, UnboundedLayoutClipped};

pub type TextureRect = glyph_brush::TextureRect;

pub struct State<'font> {
    pub(crate) glyph_brush: GlyphBrush<'font, Vertex>,
    pub(crate) hidpi_factor: f32,
}

impl <'font> State<'font> {
    pub fn set_dimensions(&mut self, hidpi_factor: f32) {
        self.hidpi_factor = hidpi_factor;
    
        // if we don't reset the cache like this then we render a stretched
        // version of the text on window resize.
        self.glyph_brush.resize_texture(
            self.glyph_brush.texture_dimensions()
        );
    }
    #[must_use]
    pub fn texture_dimensions(&self) -> Dimensions {
        self.glyph_brush.texture_dimensions()
    }

    pub(crate) fn resize_texture(&mut self, dimensions: Dimensions) {
        self.glyph_brush.resize_texture(dimensions);
    }

    #[perf_viz::record]
    #[must_use]
    pub fn render_vertices<Update, Resize>(
        &mut self,
        text_or_rects: &[TextOrRect],
        dimensions: Dimensions,
        update_texture: Update,
        mut resize_texture: Resize,
    ) -> Option<Vec<Vertex>>
    where
        for <'r> Update: FnMut(TextureRect, &'r [u8]) + Copy,
        Resize: FnMut(Dimensions) + Copy,
    {
        use TextOrRect::*;

        // We guess that usually around half of them are rects.
        let mut rect_specs = Vec::with_capacity(text_or_rects.len() / 2);

        perf_viz::start_record!("for &text_or_rects");
        for t_or_r in text_or_rects {
            use gl_layer_types::*;

            let glyph_brush = &mut self.glyph_brush;

            macro_rules! queue {
                ($layout: ident, $section: ident) => {
                    perf_viz::start_record!("queue!");
                    match $layout {
                        TextLayout::Unbounded => {
                            glyph_brush.queue_layout($section, &Unbounded {})
                        }
                        TextLayout::UnboundedLayoutClipped(ssr, scroll) => {
                            glyph_brush.queue_layout(
                                $section,
                                &UnboundedLayoutClipped::new(*ssr, scroll)
                            )
                        }
                    };
                    perf_viz::end_record!("queue!");
                }
            }

            match t_or_r {
                Rect(VisualSpec {
                    rect,
                    colour,
                    z,
                }) => {
                    perf_viz::start_record!("Rect");
                    let colour = *colour;

                    let pixel_coords: PixelCoords = 
                        text_layouts::ssr_to_glyph_brush_rect(*rect);

                    let ssr!(_, _, max_x, max_y) = rect;    

                    let mut bounds: Bounds = d!();
                    bounds.max.x = max_x.into();
                    bounds.max.y = max_y.into();
                    rect_specs.push(RectSpec {
                        pixel_coords,
                        bounds,
                        colour,
                        z: z_to_f32(*z),
                    });
                    perf_viz::end_record!("Rect");
                }
                Text(TextSpec {
                    text,
                    size,
                    layout,
                    spec: VisualSpec { rect, colour, z },
                }) => {
                    perf_viz::start_record!("Text");
                    let colour = *colour;
                    let section = Section {
                        text,
                        scale: get_scale(*size, self.hidpi_factor),
                        screen_position: rect.min.into(),
                        bounds: rect.max.into(),
                        colour,
                        z: z_to_f32(*z),
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
                    let scale = get_scale(*size, self.hidpi_factor);
                    let section = VariedSection {
                        screen_position: rect.min.into(),
                        bounds: rect.max.into(),
                        scale,
                        z: z_to_f32(*z),
                        text: text.iter().map(|ColouredText { text, colour }| {
                            SectionText {
                                text: &text,
                                colour: *colour,
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
                to_vertex_maker(
                    (dimensions.0.into(), dimensions.1.into())
                ),
                Some(AdditionalRects {
                    set_alpha,
                    rect_specs: rect_specs.clone(), // clone needed since we loop sometimes.
                }),
            );
    
            match brush_action {
                Ok(BrushAction::Draw(verticies)) => return Some(verticies),
                Ok(BrushAction::ReDraw) => return None,
                Err(BrushError::TextureTooSmall { suggested, .. }) => {
                    perf_viz::record_guard!("TextureTooSmall");
                    resize_texture(suggested);
                    self.resize_texture(suggested);
                },
            }
        }
    }

    #[must_use]
    pub fn get_char_dims(&self, text_sizes: &[f32]) -> CharDims {
        // We currently assume there is exactly one font used.
        let font = &self.glyph_brush.fonts()[0];

        // We currently assume the font is monospaced.
        let em_space_char = '\u{2003}';
    
        macro_rules! get_char_dim {
            ($scale:expr) => {{
                let scale = $scale;
                char_dim!({
                        let em_space_glyph = new_glyph(&font, em_space_char, scale, d!());
                        get_advance_width(&font, &em_space_glyph)
                    },
                    get_line_height(&font, scale),
                )
            }};
        }
    
        let mut char_dims = Vec::with_capacity(text_sizes.len());
    
        for size in text_sizes {
            char_dims.push(get_char_dim!(get_scale(*size, self.hidpi_factor)));
        }
    
        char_dims
    }
}

pub(crate) type CharDims = Vec<CharDim>;

pub const FONT_BYTES: &[u8] = include_bytes!("./fonts/FiraCode-Retina.ttf");
pub const FONT_LICENSE: &str = include_str!("./fonts/LICENSE");

pub fn new(hidpi_factor: f32) -> Res<State<'static>> {
    let font = Font::from_bytes(FONT_BYTES)?;

    Ok(
        State {
            glyph_brush: GlyphBrush::using_font(font.clone()),
            hidpi_factor,
        }
    )
}

fn get_scale(size: f32, hidpi_factor: f32) -> Scale {
    Scale::uniform((size * hidpi_factor).round())
}

#[inline]
#[perf_viz::record]
fn to_vertex_maker((screen_w, screen_h): (f32, f32)) -> impl Fn(GlyphVertex) -> Vertex + Copy {
    move |GlyphVertex {
        mut tex_coords,
        pixel_coords,
        bounds,
        colour,
        z,
    }: GlyphVertex| {
        perf_viz::record_guard!("to_vertex");
        let gl_bounds = Rect {
            min: point(
                2.0 * (bounds.min.x / screen_w - 0.5),
                2.0 * (0.5 - bounds.min.y / screen_h),
            ),
            max: point(
                2.0 * (bounds.max.x / screen_w - 0.5),
                2.0 * (0.5 - bounds.max.y / screen_h),
            ),
        };
    
        let mut gl_rect = Rect {
            min: point(
                2.0 * (pixel_coords.min.x as f32 / screen_w - 0.5),
                2.0 * (0.5 - pixel_coords.min.y as f32 / screen_h),
            ),
            max: point(
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
            colour_r: colour[0],
            colour_g: colour[1],
            colour_b: colour[2],
            colour_a: colour[3],
        }.into_vertex()
    }
}

mod unbounded {
    use glyph_brush::{
        Glyph,
        point,
        Scale,
        Font,
        SectionText,
        Colour,
        add_position,
        new_glyph,
        get_advance_width,
    };
    use macros::{d};
    use linebreak::{
        Linebreak,
        LinebreakIter,
    };

    use std::{
        iter::{Iterator},
        slice,
        str::CharIndices, 
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
        UnboundedLines {
            characters: Characters {
                font,
                scale,
                section_text: sections.iter(),
                part_info: None,
            },
        }
    }
    
    pub(crate) struct UnboundedLine<'font> {
        pub(crate) glyphs: Vec<(Glyph<'font>, [f32; 4])>,
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
            let mut caret = point(
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
                    colour,
                    is_linebreak,
                    control,
                } in &mut self.characters
                {
                    character_progressed = true;
                    {
                        /* option one '='
                        let mut kern_state = d!();
                        // ...
                        layout_width = kern_state.next_width(layout_width, &glyph);
                        option two '+='
                        let mut kern_state = d!();
                        // ...
                        (return 0 on the first one)
                        layout_width += kern_state.next_kerning(&glyph);
                        option three Option<kerning>,'+='
                        let mut kern_state = d!();
                        // ...
                        if let Some(kerning) = kern_state.next_kerning(&glyph) {
                            layout_width += kerning;
                        }
                        option four Option<kern_state>,'+='
                        let mut kern_state_op = None;
                        // ...
                        if let Some(kern_state) = kern_state_op {
                            layout_width += kern_state.next_kerning(&glyph);
                        } else {
                            kern_state_op = Some(new_kern_state(&glyph));
                        }
                        I'm leaning towards three, since it requires less babysitting.
                        Then again, four is more explicit, and keeps more code in the 
                        shared part.
                        */
                        if let Some(id) = last_glyph_id.take() {
                            layout_width += font.pair_kerning(scale, id, glyph.id());
                        }
                        last_glyph_id = Some(glyph.id());
                    }
        
                    if !control {
                        let advance_width = get_advance_width(font, &glyph);

                        let mut positioned = glyph.clone();
                        add_position(
                            &mut positioned,
                            point(caret.x + layout_width, caret.y)
                        );

                        line.glyphs.push((positioned, colour));

                        layout_width += advance_width;
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
                            colour,
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
    
                    let glyph = new_glyph(&self.font, c, self.scale, d!());
    
                    let c_len = c.len_utf8();
                    let mut linebreak = next_break.filter(|b| b.offset() == byte_index + c_len);
                    if linebreak.is_some() && byte_index + c_len == text.len() {
                        // handle inherent end-of-str hard breaks

                        // The code after this doesn't care about the index, so we 
                        // just use 0
                        linebreak = Some(if is_linebreak_char::is_linebreak_char(c) {
                            Linebreak::Hard(0)
                        } else {
                            Linebreak::Soft(0)
                        });
                    }

                    let is_linebreak = matches!(linebreak, Some(Linebreak::Hard(..)));
    
                    return Some(Character {
                        glyph,
                        colour: *colour,
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
        glyph: Glyph<'font>,
        colour: Colour,
        is_linebreak: bool,
        /// Equivalent to `char::is_control()`.
        control: bool,
    }
}

#[cfg(any(test, feature = "pub_arb"))]
mod tests;