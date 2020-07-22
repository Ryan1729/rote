use super::*;
use full_rusttype::point;
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    borrow::Cow,
    collections::hash_map::Entry,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    i32, mem, slice,
    sync::{Mutex, MutexGuard},
};

/// [`PositionedGlyph`](struct.PositionedGlyph.html) iterator.
pub type PositionedGlyphIter<'a, 'font> = std::iter::Map<
    slice::Iter<'a, (rusttype::PositionedGlyph<'font>, [f32; 4], FontId)>,
    fn(
        &'a (rusttype::PositionedGlyph<'font>, [f32; 4], FontId),
    ) -> &'a rusttype::PositionedGlyph<'font>,
>;

/// Common glyph layout logic.
///
/// # Example
/// ```no_run
/// # use glyph_brush::GlyphBrush;
/// use glyph_brush::GlyphCruncher;
///
/// # let glyph_brush: GlyphBrush<'_, ()> = unimplemented!();
/// let default_font = glyph_brush.fonts()[0];
/// ```
pub trait GlyphCruncher<'font> {
    /// Returns the pixel bounding box for the input section using a custom layout.
    /// The box is a conservative whole number pixel rectangle that can contain the section.
    ///
    /// If the section is empty or would result in no drawn glyphs will return `None`.
    ///
    /// [`glyphs_custom_layout`](#method.glyphs_custom_layout) should be preferred if the
    /// bounds are to be used to inform further layout logic.
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
    fn pixel_bounds_custom_layout<'a, S, L>(
        &mut self,
        section: S,
        custom_layout: &L,
    ) -> Option<Rect<i32>>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>;

    /// Returns an iterator over the `PositionedGlyph`s of the given section with a custom layout.
    ///
    /// Generally only drawable glyphs will be returned as invisible glyphs, like spaces,
    /// are discarded during layout.
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
    fn glyphs_custom_layout<'a, 'b, S, L>(
        &'b mut self,
        section: S,
        custom_layout: &L,
    ) -> PositionedGlyphIter<'b, 'font>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>;

    /// Returns the available fonts.
    ///
    /// The `FontId` corresponds to the index of the font data.
    fn fonts(&self) -> &[Font<'font>];

    /// Returns a bounding box for the section glyphs calculated using each glyph's
    /// vertical & horizontal metrics.
    ///
    /// If the section is empty or would result in no drawn glyphs will return `None`.
    ///
    /// Invisible glyphs, like spaces, are discarded during layout so trailing ones will
    /// not affect the bounds.
    ///
    /// Benefits from caching, see [caching behaviour](#caching-behaviour).
    fn glyph_bounds_custom_layout<'a, S, L>(
        &mut self,
        section: S,
        custom_layout: &L,
    ) -> Option<Rect<f32>>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section = section.into();

        self.glyphs_custom_layout(section, custom_layout)
            .filter_map(|glyph| glyph.font().map(|f| (f, glyph)))
            .fold(None, |b: Option<Rect<f32>>, (font, glyph)| {
                let hm = glyph.unpositioned().h_metrics();
                let vm = font.v_metrics(glyph.scale());
                let pos = glyph.position();
                let lbound = Rect {
                    min: point(pos.x - hm.left_side_bearing, pos.y - vm.ascent),
                    max: point(pos.x + hm.advance_width, pos.y - vm.descent),
                };

                b.map(|b| {
                    let min_x = b.min.x.min(lbound.min.x);
                    let max_x = b.max.x.max(lbound.max.x);
                    let min_y = b.min.y.min(lbound.min.y);
                    let max_y = b.max.y.max(lbound.max.y);
                    Rect {
                        min: point(min_x, min_y),
                        max: point(max_x, max_y),
                    }
                })
                .or_else(|| Some(lbound))
            })
    }
}

/// Cut down version of a [`GlyphBrush`](struct.GlyphBrush.html) that can calculate pixel bounds,
/// but is unable to actually render anything.
///
/// Build using a [`GlyphCalculatorBuilder`](struct.GlyphCalculatorBuilder.html).
///
/// # Caching behaviour
///
/// Calls to [`GlyphCalculatorGuard::pixel_bounds`](#method.pixel_bounds),
/// [`GlyphCalculatorGuard::glyphs`](#method.glyphs) calculate the positioned glyphs for a
/// section. This is cached so future calls to any of the methods for the same section are much
/// cheaper.
///
/// Unlike a [`GlyphBrush`](struct.GlyphBrush.html) there is no concept of actually drawing
/// the section to imply when a section is used / no longer used. Instead a `GlyphCalculatorGuard`
/// is created, that provides the calculation functionality. Dropping indicates the 'cache frame'
/// is over, similar to when a `GlyphBrush` draws. Section calculations are cached for the next
/// 'cache frame', if not used then they will be dropped.
pub struct GlyphCalculator<'font, H = DefaultSectionHasher> {
    fonts: Vec<Font<'font>>,

    // cache of section-layout hash -> computed glyphs, this avoid repeated glyph computation
    // for identical layout/sections common to repeated frame rendering
    calculate_glyph_cache: Mutex<FxHashMap<u64, GlyphedSection<'font>>>,

    section_hasher: H,
}

impl<H> fmt::Debug for GlyphCalculator<'_, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GlyphCalculator")
    }
}

impl<'font, H: BuildHasher + Clone> GlyphCalculator<'font, H> {
    pub fn cache_scope<'a>(&'a self) -> GlyphCalculatorGuard<'a, 'font, H> {
        GlyphCalculatorGuard {
            fonts: &self.fonts,
            glyph_cache: self.calculate_glyph_cache.lock().unwrap(),
            cached: FxHashSet::default(),
            section_hasher: self.section_hasher.clone(),
        }
    }

    /// Returns the available fonts.
    ///
    /// The `FontId` corresponds to the index of the font data.
    pub fn fonts(&self) -> &[Font<'_>] {
        &self.fonts
    }
}

/// [`GlyphCalculator`](struct.GlyphCalculator.html) scoped cache lock.
pub struct GlyphCalculatorGuard<'brush, 'font: 'brush, H = DefaultSectionHasher> {
    fonts: &'brush Vec<Font<'font>>,
    glyph_cache: MutexGuard<'brush, FxHashMap<u64, GlyphedSection<'font>>>,
    cached: FxHashSet<u64>,
    section_hasher: H,
}

impl<H: BuildHasher> GlyphCalculatorGuard<'_, '_, H> {
    /// Returns the calculate_glyph_cache key for this sections glyphs
    fn cache_glyphs<L>(&mut self, section: &VariedSection<'_>, layout: &L) -> u64
    where
        L: GlyphPositioner,
    {
        let section_hash = {
            let mut hasher = self.section_hasher.build_hasher();
            section.hash(&mut hasher);
            layout.hash(&mut hasher);
            hasher.finish()
        };

        if let Entry::Vacant(entry) = self.glyph_cache.entry(section_hash) {
            let geometry = SectionGeometry::from(section);
            entry.insert(GlyphedSection {
                glyphs: layout.calculate_glyphs(self.fonts, &geometry, &section.text),
                z: section.z,
            });
        }

        section_hash
    }
}

impl fmt::Debug for GlyphCalculatorGuard<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GlyphCalculatorGuard")
    }
}

impl<'font, H: BuildHasher> GlyphCruncher<'font> for GlyphCalculatorGuard<'_, 'font, H> {
    fn pixel_bounds_custom_layout<'a, S, L>(
        &mut self,
        section: S,
        custom_layout: &L,
    ) -> Option<Rect<i32>>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section_hash = self.cache_glyphs(&section.into(), custom_layout);
        self.cached.insert(section_hash);
        self.glyph_cache[&section_hash].pixel_bounds()
    }

    fn glyphs_custom_layout<'a, 'b, S, L>(
        &'b mut self,
        section: S,
        custom_layout: &L,
    ) -> PositionedGlyphIter<'b, 'font>
    where
        L: GlyphPositioner + Hash,
        S: Into<Cow<'a, VariedSection<'a>>>,
    {
        let section_hash = self.cache_glyphs(&section.into(), custom_layout);
        self.cached.insert(section_hash);
        self.glyph_cache[&section_hash].glyphs()
    }

    #[inline]
    fn fonts(&self) -> &[Font<'font>] {
        &self.fonts
    }
}

impl<H> Drop for GlyphCalculatorGuard<'_, '_, H> {
    fn drop(&mut self) {
        let cached = mem::take(&mut self.cached);
        self.glyph_cache.retain(|key, _| cached.contains(key));
    }
}

/// Builder for a [`GlyphCalculator`](struct.GlyphCalculator.html).
///
#[derive(Debug, Clone)]
pub struct GlyphCalculatorBuilder<'a, H = DefaultSectionHasher> {
    font_data: Vec<Font<'a>>,
    section_hasher: H,
}

impl<'a> GlyphCalculatorBuilder<'a> {
    /// Specifies the default font data used to render glyphs.
    /// Referenced with `FontId(0)`, which is default.
    pub fn using_font_bytes<B: Into<SharedBytes<'a>>>(font_0_data: B) -> Self {
        Self::using_font(Font::from_bytes(font_0_data).unwrap())
    }

    pub fn using_fonts_bytes<B, V>(font_data: V) -> Self
    where
        B: Into<SharedBytes<'a>>,
        V: Into<Vec<B>>,
    {
        Self::using_fonts(
            font_data
                .into()
                .into_iter()
                .map(|data| Font::from_bytes(data).unwrap())
                .collect::<Vec<_>>(),
        )
    }

    /// Specifies the default font used to render glyphs.
    /// Referenced with `FontId(0)`, which is default.
    pub fn using_font(font_0_data: Font<'a>) -> Self {
        Self::using_fonts(vec![font_0_data])
    }

    pub fn using_fonts<V: Into<Vec<Font<'a>>>>(fonts: V) -> Self {
        Self {
            font_data: fonts.into(),
            section_hasher: DefaultSectionHasher::default(),
        }
    }
}

impl<'a, H: BuildHasher> GlyphCalculatorBuilder<'a, H> {
    /// Adds additional fonts to the one added in [`using_font`](#method.using_font) /
    /// [`using_font_bytes`](#method.using_font_bytes).
    ///
    /// Returns a [`FontId`](struct.FontId.html) to reference this font.
    pub fn add_font_bytes<B: Into<SharedBytes<'a>>>(&mut self, font_data: B) -> FontId {
        self.font_data
            .push(Font::from_bytes(font_data.into()).unwrap());
        FontId(self.font_data.len() - 1)
    }

    /// Adds additional fonts to the one added in [`using_font`](#method.using_font) /
    /// [`using_font_bytes`](#method.using_font_bytes).
    ///
    /// Returns a [`FontId`](struct.FontId.html) to reference this font.
    pub fn add_font(&mut self, font_data: Font<'a>) -> FontId {
        self.font_data.push(font_data);
        FontId(self.font_data.len() - 1)
    }

    /// Sets the section hasher. `GlyphCalculator` cannot handle absolute section hash collisions
    /// so use a good hash algorithm.
    ///
    /// This hasher is used to distinguish sections, rather than for hashmap internal use.
    ///
    /// Defaults to [xxHash](https://docs.rs/twox-hash).
    pub fn section_hasher<T: BuildHasher>(
        self,
        section_hasher: T,
    ) -> GlyphCalculatorBuilder<'a, T> {
        GlyphCalculatorBuilder {
            font_data: self.font_data,
            section_hasher,
        }
    }

    /// Builds a `GlyphCalculator`
    pub fn build(self) -> GlyphCalculator<'a, H> {
        GlyphCalculator {
            fonts: self.font_data,
            calculate_glyph_cache: Mutex::default(),
            section_hasher: self.section_hasher,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct GlyphedSection<'font> {
    pub glyphs: Vec<(PositionedGlyph<'font>, Color, FontId)>,
    pub z: f32,
}

impl<'a> PartialEq<GlyphedSection<'a>> for GlyphedSection<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.z == other.z
            && self.glyphs.len() == other.glyphs.len()
            && self.glyphs.iter().zip(other.glyphs.iter()).all(|(l, r)| {
                l.2 == r.2
                    && l.1 == r.1
                    && l.0.id() == r.0.id()
                    && l.0.position() == r.0.position()
                    && l.0.scale() == r.0.scale()
            })
    }
}

impl<'font> GlyphedSection<'font> {
    pub(crate) fn pixel_bounds(&self) -> Option<Rect<i32>> {
        let Self {
            ref glyphs, ..
        } = *self;

        let mut no_match = true;

        let mut pixel_bounds = Rect {
            min: point(0, 0),
            max: point(0, 0),
        };

        for Rect { min, max } in glyphs
            .iter()
            .filter_map(|&(ref g, ..)| g.pixel_bounding_box())
        {
            if no_match || min.x < pixel_bounds.min.x {
                pixel_bounds.min.x = min.x;
            }
            if no_match || min.y < pixel_bounds.min.y {
                pixel_bounds.min.y = min.y;
            }
            if no_match || max.x > pixel_bounds.max.x {
                pixel_bounds.max.x = max.x;
            }
            if no_match || max.y > pixel_bounds.max.y {
                pixel_bounds.max.y = max.y;
            }
            no_match = false;
        }

        Some(pixel_bounds).filter(|_| !no_match)
    }

    #[inline]
    pub(crate) fn glyphs(&self) -> PositionedGlyphIter<'_, 'font> {
        self.glyphs.iter().map(|(g, ..)| g)
    }
}