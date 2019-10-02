// This file was split off of a file that was part of https://github.com/alexheretic/glyph-brush
use gl::types::*;
use glyph_brush::*;
use glyph_brush::{
    rusttype::{Font, Scale},
    Bounds, GlyphBrush, GlyphBrushBuilder, HighlightRange, Layout, PixelCoords, Section,
};
use macros::{d, invariants_checked};
use platform_types::CharDim;
use shared::Res;
use std::{ffi::CString, mem, ptr, str};

pub const EDIT_Z: f32 = 0.5;
pub const HIGHLIGHT_Z: f32 = 0.4375;
pub const CURSOR_Z: f32 = 0.375;
pub const STATUS_BACKGROUND_Z: f32 = 0.25;
pub const STATUS_Z: f32 = 0.125;

pub struct State<'font> {
    vertex_count: usize,
    vertex_max: usize,
    program: u32,
    fs: u32,
    vs: u32,
    vbo: u32,
    vao: u32,
    glyph_brush: GlyphBrush<'font, Vertex>,
    glyph_texture: u32,
    hidpi_factor: f32,
}

/// ```text
/// [
///     left_top * 3,
///     right_bottom * 2,
///     tex_left_top * 2,
///     tex_right_bottom * 2,
///     color * 4,
///     override_alpha
/// ]
/// ```
pub type Vertex = [GLfloat; 14];

fn set_full_alpha(vertex: &mut Vertex) {
    vertex[13] = 1.0;
}

fn extract_tex_coords(vertex: &Vertex) -> TexCoords {
    let mut output: TexCoords = d!();
    // To compensate for y flipping in to_vertex
    output.min.x = vertex[5];
    output.max.y = vertex[6];
    output.max.x = vertex[7];
    output.min.y = vertex[8];
    output
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

macro_rules! gl_assert_ok {
    () => {{
        if invariants_checked!() {
            let err = gl::GetError();
            assert_eq!(err, gl::NO_ERROR, "{}", gl_err_to_str(err));
        }
    }};
}

pub fn init<F>(
    hidpi_factor: f32,
    text_sizes: &[f32],
    load_fn: F,
) -> Res<(State<'static>, Vec<CharDim>)>
where
    F: FnMut(&'static str) -> *const GLvoid,
{
    const FONT_BYTES: &[u8] = include_bytes!("./fonts/FiraCode-Retina.ttf");
    let font: Font<'static> = Font::from_bytes(FONT_BYTES)?;

    let glyph_brush = get_glyph_brush(&font);

    // Load the OpenGL function pointers
    gl::load_with(load_fn);

    // Create GLSL shaders
    let vs = compile_shader(include_str!("shader/vert.glsl"), gl::VERTEX_SHADER)?;
    let fs = compile_shader(include_str!("shader/frag.glsl"), gl::FRAGMENT_SHADER)?;
    let program = link_program(vs, fs)?;

    let mut vao = 0;
    let mut vbo = 0;
    let mut glyph_texture = 0;

    unsafe {
        // Create Vertex Array Object
        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);

        // Create a Vertex Buffer Object
        gl::GenBuffers(1, &mut vbo);
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);

        // Enamble Depth testing so we can occlude things while sending them down in any order
        gl::Enable(gl::DEPTH_TEST);

        {
            // Create a texture for the glyphs
            // The texture holds 1 byte per pixel as alpha data
            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
            gl::GenTextures(1, &mut glyph_texture);
            gl::BindTexture(gl::TEXTURE_2D, glyph_texture);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as _);
            let (width, height) = glyph_brush.texture_dimensions();
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RED as _,
                width as _,
                height as _,
                0,
                gl::RED,
                gl::UNSIGNED_BYTE,
                ptr::null(),
            );
            gl_assert_ok!();
        }

        // Use shader program
        gl::UseProgram(program);
        gl::BindFragDataLocation(program, 0, CString::new("out_color")?.as_ptr());

        // Specify the layout of the vertex data
        let mut offset = 0;
        for (v_field, float_count) in &[
            ("left_top", 3),
            ("right_bottom", 2),
            ("tex_left_top", 2),
            ("tex_right_bottom", 2),
            ("color", 4),
            ("override_alpha", 1),
        ] {
            let attr = gl::GetAttribLocation(program, CString::new(*v_field)?.as_ptr());
            if attr < 0 {
                return Err(format!("{} GetAttribLocation -> {}", v_field, attr).into());
            }
            gl::VertexAttribPointer(
                attr as _,
                *float_count,
                gl::FLOAT,
                gl::FALSE as _,
                mem::size_of::<Vertex>() as _,
                offset as _,
            );
            gl::EnableVertexAttribArray(attr as _);
            gl::VertexAttribDivisor(attr as _, 1);

            offset += float_count * 4;
        }

        // Enabled alpha blending
        gl::Enable(gl::BLEND);
        gl::BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        // Use srgb for consistency with other examples
        gl::Enable(gl::FRAMEBUFFER_SRGB);
        gl::ClearColor(0.02, 0.02, 0.02, 1.0);
    }

    let vertex_count = 0;
    let vertex_max = vertex_count;

    macro_rules! get_char_dim {
        ($scale:ident) => {
            CharDim {
                w: {
                    // We currently assume the font is monospaced.
                    let em_space_char = '\u{2003}';
                    let h_metrics = font.glyph(em_space_char).scaled($scale).h_metrics();

                    h_metrics.advance_width
                },
                h: {
                    let v_metrics = font.v_metrics($scale);

                    v_metrics.ascent + -v_metrics.descent + v_metrics.line_gap
                },
            }
        };
    }

    let mut char_dims = Vec::with_capacity(text_sizes.len());

    for size in text_sizes {
        let scale = Scale::uniform((size * hidpi_factor).round());
        char_dims.push(get_char_dim!(scale));
    }

    Ok((
        State {
            vertex_count,
            vertex_max,
            program,
            fs,
            vs,
            vbo,
            vao,
            glyph_brush,
            glyph_texture,
            hidpi_factor,
        },
        char_dims,
    ))
}

pub fn set_dimensions(state: &mut State, hidpi_factor: f32, (width, height): (i32, i32)) {
    unsafe {
        gl::Viewport(0, 0, width, height);
    }

    state.hidpi_factor = hidpi_factor;

    //if we don't reset the cache like this then we render a stretched
    //version of the text on window resize.
    let (t_w, t_h) = state.glyph_brush.texture_dimensions();
    state.glyph_brush.resize_texture(t_w, t_h);
}

#[derive(Clone, Debug)]
pub struct VisualSpec {
    /// Position on screen to render, in pixels from top-left. Defaults to (0, 0).
    pub screen_position: (f32, f32),
    /// Max (width, height) bounds, in pixels from top-left. Defaults to unbounded.
    pub bounds: (f32, f32),
    /// Rgba color of rendered item. Defaults to black.
    pub color: [f32; 4],
    /// Z values for use in depth testing. Defaults to 0.0
    pub z: f32,
}
d!(for VisualSpec: VisualSpec{
    screen_position: (0.0, 0.0),
    bounds: (std::f32::INFINITY, std::f32::INFINITY),
    color: [0.0, 0.0, 0.0, 1.0],
    z: 0.0,
});

#[derive(Clone, Debug)]
pub enum TextOrRect<'text> {
    Text {
        spec: VisualSpec,
        text: &'text str,
        /// The font size
        size: f32,
    },
    Rect(VisualSpec),
}

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

#[perf_viz::record]
pub fn render(
    State {
        ref mut vertex_count,
        ref mut vertex_max,
        ref mut glyph_brush,
        ..
    }: &mut State,
    text_and_rects: Vec<TextOrRect>,
    width: u32,
    height: u32,
) -> Res<()> {
    let mut highlight_ranges = Vec::new();
    perf_viz::start_record!("for &text_and_rects");
    for t_or_r in text_and_rects {
        match t_or_r {
            TextOrRect::Text {
                text,
                size,
                spec:
                    VisualSpec {
                        screen_position,
                        bounds,
                        color,
                        z,
                    },
            } => glyph_brush.queue(Section {
                text: &text,
                scale: Scale::uniform(size),
                screen_position,
                bounds,
                color,
                layout: Layout::default_wrap(),
                z,
                ..d!()
            }),
            TextOrRect::Rect(VisualSpec {
                screen_position: (min_x, min_y),
                bounds: (max_x, max_y),
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
                    z,
                });
            }
        }
    }
    perf_viz::end_record!("for &text_and_rects");

    let query_ids = [0; 1];
    if cfg!(feature = "time-render") {
        // Adding and then retreving this query for how long the gl rendering took,
        // "implicitly flushes the GL pipeline" according to this docs page:
        // https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBeginQuery.xhtml
        // Without something flushing the queue, as of this writing, the frames do not
        // appear to render as quickly. That is, after user input the updated frame does
        // shows up after a noticably longer delay. Oddly enough, `glFinish` produces the
        // same speed up, and but it takes longer than this query does, (around a ms or so)
        // at least on my current machine + driver setup. Here's a question abotut this:
        // https://gamedev.stackexchange.com/q/172737
        // For the time being, I'm making this feature enabled by default since it is
        // currently faster, but thi may well not be true any more on a future machine/driver
        // so it seems worth it to keep it a feature.
        unsafe {
            gl::GenQueries(1, query_ids.as_ptr() as _);
            gl::BeginQuery(gl::TIME_ELAPSED, query_ids[0])
        }
    }
    let dimensions = (width, height);
    let mut brush_action;
    loop {
        perf_viz::start_record!("process_queued");
        brush_action = glyph_brush.process_queued(
            dimensions,
            |rect, tex_data| unsafe {
                perf_viz::start_record!("|rect, tex_data|");
                // Update part of gpu texture with new glyph alpha values
                gl::TexSubImage2D(
                    gl::TEXTURE_2D,
                    0,
                    rect.min.x as _,
                    rect.min.y as _,
                    rect.width() as _,
                    rect.height() as _,
                    gl::RED,
                    gl::UNSIGNED_BYTE,
                    tex_data.as_ptr() as _,
                );
                gl_assert_ok!();
                perf_viz::end_record!("|rect, tex_data|");
            },
            to_vertex,
            Some(AdditionalRects {
                set_full_alpha,
                extract_tex_coords,
                highlight_ranges: highlight_ranges.clone(),
            }),
        );
        perf_viz::end_record!("process_queued");

        match brush_action {
            Ok(_) => break,
            Err(BrushError::TextureTooSmall { suggested, .. }) => unsafe {
                perf_viz::record_guard!("BrushError::TextureTooSmall");
                let (new_width, new_height) = suggested;
                eprint!("\r                            \r");
                eprintln!("Resizing glyph texture -> {}x{}", new_width, new_height);
                // Recreate texture as a larger size to fit more
                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    0,
                    gl::RED as _,
                    new_width as _,
                    new_height as _,
                    0,
                    gl::RED,
                    gl::UNSIGNED_BYTE,
                    ptr::null(),
                );
                gl_assert_ok!();
                glyph_brush.resize_texture(new_width, new_height);
            },
        }
    }
    match brush_action? {
        BrushAction::Draw(vertices) => {
            perf_viz::record_guard!("BrushAction::Draw");
            // Draw new vertices
            *vertex_count = vertices.len();
            unsafe {
                if vertex_max < vertex_count {
                    gl::BufferData(
                        gl::ARRAY_BUFFER,
                        (*vertex_count * mem::size_of::<Vertex>()) as GLsizeiptr,
                        vertices.as_ptr() as _,
                        gl::DYNAMIC_DRAW,
                    );
                } else {
                    gl::BufferSubData(
                        gl::ARRAY_BUFFER,
                        0,
                        (*vertex_count * mem::size_of::<Vertex>()) as GLsizeiptr,
                        vertices.as_ptr() as _,
                    );
                }
            }
            *vertex_max = *vertex_max.max(vertex_count);
        }
        BrushAction::ReDraw => {}
    }

    unsafe {
        perf_viz::record_guard!("DrawArraysInstanced");
        gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
        gl::DrawArraysInstanced(gl::TRIANGLE_STRIP, 0, 4, *vertex_count as _);
    }

    //See comment in above "time-render" check.
    if cfg!(feature = "time-render") {
        let mut time_elapsed = 0;
        unsafe {
            gl::EndQuery(gl::TIME_ELAPSED);
            gl::GetQueryObjectiv(query_ids[0], gl::QUERY_RESULT, &mut time_elapsed);
            gl::DeleteQueries(1, query_ids.as_ptr() as _);
        }
    } else {
        unsafe {
            gl::Finish();
        }
    }

    Ok(())
}

fn get_glyph_brush<'font, A: Clone>(font: &Font<'font>) -> GlyphBrush<'font, A> {
    GlyphBrushBuilder::using_font(font.clone())
        // Leaving this at the default of 0.1 makes the cache get cleared too often.
        // Putting this at 1.0 means that the characters are visibly poorly kerned.
        // This value seems like a happy medium at the moment.
        .gpu_cache_position_tolerance(0.25)
        .build()
}

pub fn cleanup(
    &State {
        program,
        fs,
        vs,
        vbo,
        vao,
        glyph_texture,
        ..
    }: &State,
) -> Res<()> {
    unsafe {
        gl::DeleteProgram(program);
        gl::DeleteShader(fs);
        gl::DeleteShader(vs);
        gl::DeleteBuffers(1, &vbo);
        gl::DeleteVertexArrays(1, &vao);
        gl::DeleteTextures(1, &glyph_texture);
    }

    Ok(())
}

fn gl_err_to_str(err: u32) -> &'static str {
    match err {
        gl::INVALID_ENUM => "INVALID_ENUM",
        gl::INVALID_VALUE => "INVALID_VALUE",
        gl::INVALID_OPERATION => "INVALID_OPERATION",
        gl::INVALID_FRAMEBUFFER_OPERATION => "INVALID_FRAMEBUFFER_OPERATION",
        gl::OUT_OF_MEMORY => "OUT_OF_MEMORY",
        gl::STACK_UNDERFLOW => "STACK_UNDERFLOW",
        gl::STACK_OVERFLOW => "STACK_OVERFLOW",
        _ => "Unknown error",
    }
}

fn compile_shader(src: &str, ty: GLenum) -> Res<GLuint> {
    let shader;
    unsafe {
        shader = gl::CreateShader(ty);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes())?;
        gl::ShaderSource(shader, 1, &c_str.as_ptr(), ptr::null());
        gl::CompileShader(shader);

        // Get the compile status
        let mut status = GLint::from(gl::FALSE);
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != GLint::from(gl::TRUE) {
            let mut len = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            gl::GetShaderInfoLog(
                shader,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut GLchar,
            );
            return Err(str::from_utf8(&buf)?.into());
        }
    }
    Ok(shader)
}

fn link_program(vs: GLuint, fs: GLuint) -> Res<GLuint> {
    unsafe {
        let program = gl::CreateProgram();
        gl::AttachShader(program, vs);
        gl::AttachShader(program, fs);
        gl::LinkProgram(program);
        // Get the link status
        let mut status = GLint::from(gl::FALSE);
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut status);

        // Fail on error
        if status != GLint::from(gl::TRUE) {
            let mut len: GLint = 0;
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            gl::GetProgramInfoLog(
                program,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut GLchar,
            );
            return Err(str::from_utf8(&buf)?.into());
        }
        Ok(program)
    }
}
