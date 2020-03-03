use platform_types::{
    screen_positioning::{CharDim, ScreenSpaceRect},
    ssr,
};
use gl_layer_types::{Vertex, extract_tex_coords, TexCoords, set_full_alpha, TextOrRect, Res};

use macros::{d};

use glyph_brush::*;
use glyph_brush::{
    rusttype::{Font, Scale},
    Bounds, GlyphBrush, GlyphBrushBuilder, HighlightRange, Layout, PixelCoords, Section,
};

mod text_layouts {
    use super::*;
    use glyph_brush::{
        get_lines_iter,
        rusttype::{point, vector, Point, PositionedGlyph, Rect},
    };
    use std::borrow::Cow;
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
            let y_max = std::f32::INFINITY; // never cut off the bottom of the text.

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

    pub struct WrapInRect {
        pub rect: ScreenSpaceRect,
    }
    impl std::hash::Hash for WrapInRect {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.rect.min.0.to_bits().hash(state);
            self.rect.min.1.to_bits().hash(state);
            self.rect.max.0.to_bits().hash(state);
            self.rect.max.1.to_bits().hash(state);
        }
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

            wide_bounds.min.x = if wide_bounds.min.x < rect.min.0 {
                rect.min.0
            } else {
                wide_bounds.min.x
            };
            wide_bounds.min.y = if wide_bounds.min.y < rect.min.1 {
                rect.min.1
            } else {
                wide_bounds.min.y
            };

            wide_bounds.max.x = if wide_bounds.max.x > rect.max.0 {
                rect.max.0
            } else {
                wide_bounds.max.x
            };
            wide_bounds.max.y = if wide_bounds.max.y > rect.max.1 {
                rect.max.1
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

    use std::f32::INFINITY;
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

    pub fn tex_coords_to_rect(coords: TexCoords) -> Rect<f32> {
        let mut r = Rect::default();
                        
        r.min.x = coords.min.x;
        r.min.y = coords.min.y;
        r.max.x = coords.max.x;
        r.max.y = coords.max.y;

        r
    }
}
use text_layouts::{Unbounded, Wrap, WrapInRect, tex_coords_to_rect};

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

        let mut highlight_ranges = Vec::new();

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

        perf_viz::start_record!("for &text_or_rects");
        for t_or_r in text_or_rects {
            use gl_layer_types::*;

            let glyph_brush = &mut self.glyph_brush;

            match t_or_r {
                Rect(VisualSpec {
                    rect: ssr!(min_x, min_y, max_x, max_y),
                    color,
                    z,
                }) => {
                    let mut pixel_coords: PixelCoords = d!();
                    pixel_coords.min.x = f32_to_i32_or_max(min_x);
                    pixel_coords.min.y = f32_to_i32_or_max(min_y);
                    pixel_coords.max.x = f32_to_i32_or_max(max_x);
                    pixel_coords.max.y = f32_to_i32_or_max(max_y);
    
                    let mut bounds: Bounds = d!();
                    bounds.max = (max_x, max_y).into();
                    highlight_ranges.push(HighlightRange {
                        pixel_coords,
                        bounds,
                        color,
                        z: z_to_f32(z),
                    });
                }
                Text(TextSpec {
                    text,
                    size,
                    layout,
                    spec: VisualSpec { rect, color, z },
                }) => {
                    let section = Section {
                        text: &text,
                        scale: get_scale(size, self.hidpi_factor),
                        screen_position: rect.min,
                        bounds: rect.max,
                        color,
                        layout: match layout {
                            TextLayout::SingleLine => Layout::default_single_line(),
                            TextLayout::Wrap | TextLayout::WrapInRect(_) | TextLayout::Unbounded => {
                                Layout::default_wrap()
                            }
                        },
                        z: z_to_f32(z),
                        ..d!()
                    };
    
                    match layout {
                        TextLayout::SingleLine => glyph_brush.queue(section),
                        TextLayout::Wrap => glyph_brush.queue_custom_layout(section, &Wrap {}),
                        TextLayout::WrapInRect(rect) => {
                            glyph_brush.queue_custom_layout(section, &WrapInRect { rect })
                        }
                        TextLayout::Unbounded => {
                            glyph_brush.queue_custom_layout(section, &Unbounded {})
                        }
                    };
                }
                MulticolourText(MulticolourTextSpec{
                    size,
                    layout,
                    rect,
                    z,
                    text,
                }) => {
                    let scale = get_scale(size, self.hidpi_factor);
                    let section = VariedSection {
                        screen_position: rect.min,
                        bounds: rect.max,
                        layout: match layout {
                            TextLayout::SingleLine => Layout::default_single_line(),
                            TextLayout::Wrap | TextLayout::WrapInRect(_) | TextLayout::Unbounded => {
                                Layout::default_wrap().line_breaker(glyph_brush::BuiltInLineBreaker::AnyCharLineBreaker)
                            }
                        },
                        z: z_to_f32(z),
                        text: text.into_iter().map(|ColouredText { text, color }| {
                            SectionText {
                                text,
                                scale,
                                color,
                                ..d!()
                            }
                        }).collect(),
                        ..d!()
                    };
    
                    match layout {
                        TextLayout::SingleLine => glyph_brush.queue(section),
                        TextLayout::Wrap => glyph_brush.queue_custom_layout(section, &Wrap {}),
                        TextLayout::WrapInRect(rect) => {
                            glyph_brush.queue_custom_layout(section, &WrapInRect { rect })
                        }
                        TextLayout::Unbounded => {
                            glyph_brush.queue_custom_layout(section, &Unbounded {})
                        }
                    };
                }
            }
        }
        perf_viz::end_record!("for &text_or_rects");

        loop {
            let brush_action = self.glyph_brush.process_queued(
                dimensions,
                update_texture,
                to_vertex,
                Some(AdditionalRects {
                    set_full_alpha,
                    extract_tex_coords: |v| {
                        tex_coords_to_rect(extract_tex_coords(v))
                    },
                    highlight_ranges: highlight_ranges.clone(), // clone needed since we loop sometimes.
                }),
            );
    
            match brush_action {
                Ok(BrushAction::Draw(verticies)) => return Some(verticies),
                Ok(BrushAction::ReDraw) => return None,
                Err(BrushError::TextureTooSmall { suggested: (new_width, new_height), .. }) => {
                    resize_texture(new_width, new_height);
                    self.resize_texture(new_width, new_height);
                },
            }
        }
    }
}

pub type CharDims = Vec<CharDim>;

pub fn new(hidpi_factor: f32, text_sizes: &[f32]) -> Res<(State, CharDims)> {
    const FONT_BYTES: &[u8] = include_bytes!("./fonts/FiraCode-Retina.ttf");
    let font: Font<'static> = Font::from_bytes(FONT_BYTES)?;

    macro_rules! get_char_dim {
        ($scale:expr) => {{
            let scale = $scale;
            CharDim {
                w: {
                    // We currently assume the font is monospaced.
                    let em_space_char = '\u{2003}';
                    let h_metrics = font.glyph(em_space_char).scaled(scale).h_metrics();

                    h_metrics.advance_width
                },
                h: {
                    let v_metrics = font.v_metrics(scale);

                    v_metrics.ascent + -v_metrics.descent + v_metrics.line_gap
                },
            }
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
fn to_vertex(
    glyph_brush::GlyphVertex {
        mut tex_coords,
        pixel_coords,
        bounds,
        screen_dimensions: (screen_w, screen_h),
        color,
        z,
    }: glyph_brush::GlyphVertex,
) -> Vertex {
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

    [
        gl_rect.min.x,
        gl_rect.max.y,
        z,
        gl_rect.max.x,
        gl_rect.min.y,
        // this isn't `mix.x, min.y, max.x, max.y` in order to flip the y axis
        tex_coords.min.x,
        tex_coords.max.y,
        tex_coords.max.x,
        tex_coords.min.y,
        //
        color[0],
        color[1],
        color[2],
        color[3],
        0.0,
    ]
}