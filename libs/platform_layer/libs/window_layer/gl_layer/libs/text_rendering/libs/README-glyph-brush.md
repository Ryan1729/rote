# Modification Note

The copies of crates from [glyph-brush](https://github.com/alexheretic/glyph-brush) have been modified to suit the needs of rote. In particular the initial copy was made from commit [35faf7357115785bcd1b611b90c1d31bb4307962](https://github.com/alexheretic/glyph-brush/commit/35faf7357115785bcd1b611b90c1d31bb4307962).

# glyph-brush
Fast cached text rendering.

## [glyph_brush](glyph-brush) [![](https://img.shields.io/crates/v/glyph_brush.svg)](https://crates.io/crates/glyph_brush) [![](https://docs.rs/glyph_brush/badge.svg)](https://docs.rs/glyph_brush)
Render API agnostic rasterization & draw caching text rendering.

## [gfx_glyph](gfx-glyph) [![](https://img.shields.io/crates/v/gfx_glyph.svg)](https://crates.io/crates/gfx_glyph) [![](https://docs.rs/gfx_glyph/badge.svg)](https://docs.rs/gfx_glyph)
glyph_brush wrapper for [gfx-rs v0.18](https://github.com/gfx-rs/gfx/tree/pre-ll) (OpenGL).

## [glyph_brush_layout](glyph-brush-layout) [![](https://img.shields.io/crates/v/glyph_brush_layout.svg)](https://crates.io/crates/glyph_brush_layout) [![](https://docs.rs/glyph_brush_layout/badge.svg)](https://docs.rs/glyph_brush_layout)
Text layout for [rusttype](https://gitlab.redox-os.org/redox-os/rusttype) used in glyph_brush.


## Examples
`cargo run -p glyph_brush --example opengl --release`

![](https://i.ibb.co/rvS2vp9/glyph-brush-example.png)

Also look at the individual crate readmes.

## Minimum supported rust compiler
All crates maintained with [latest stable rust](https://gist.github.com/alexheretic/d1e98d8433b602e57f5d0a9637927e0c).
