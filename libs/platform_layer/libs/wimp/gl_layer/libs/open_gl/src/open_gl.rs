use std::{ffi::CString, mem, ptr, str};
// This file was split off of a file that was part of https://github.com/alexheretic/glyph-brush
use gl::types::*;

use gl_layer_types::{DEPTH_MIN, DEPTH_MAX, Vertex, VERTEX_SPEC, Res};

use macros::{invariants_checked};

macro_rules! gl_assert_ok {
    () => {{
        if invariants_checked!() {
            let err = gl::GetError();
            assert_eq!(err, gl::NO_ERROR, "{}", gl_err_to_str(err));
        }
    }};
}

pub type LoadFnOutput = *const GLvoid;

pub struct State {
    vertex_count: usize,
    vertex_max: usize,
    program: u32,
    fs: u32,
    vs: u32,
    vbo: u32,
    vao: u32,
    glyph_texture: u32,
    query_ids: [GLuint; 1],
}

impl State {
    pub fn new<F>(
        clear_colour: [f32; 4], // the clear colour currently flashes up on exit.
        (width, height): (u32, u32),
        load_fn: F,
    ) -> Res<Self>
    where F: FnMut(&'static str) -> LoadFnOutput {
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
    
            // Enable depth testing so we can occlude things while sending them down in any order
            gl::Enable(gl::DEPTH_TEST);
            gl::DepthRangef(DEPTH_MIN, DEPTH_MAX);
    
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
                
                gl::TexImage2D(
                    gl::TEXTURE_2D,
                    0,
                    gl::R8 as _,
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
            for (v_field, float_count) in &VERTEX_SPEC {
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
    
            // We specifically do *not* enable `FRAMEBUFFER_SRGB` because we currently are passing
            // sRGB colours into the shader, rather than linear colours, so the extra linear to sRGB
            // conversion that this setting would apply, would make our colours too bright. If we want
            // to do colour blends in the shader, we'll need to enable this and convert our input
            // colours to linear ourselves.
            //gl::Enable(gl::FRAMEBUFFER_SRGB);
    
            gl::ClearColor(
                clear_colour[0],
                clear_colour[1],
                clear_colour[2],
                clear_colour[3],
            );
    
            let mut depth_bits = 0;
            gl::GetIntegerv(3414, &mut depth_bits);
            dbg!(depth_bits);
        }
    
        let vertex_count = 0;
        let vertex_max = vertex_count;
    
        Ok(State {
            vertex_count,
            vertex_max,
            program,
            fs,
            vs,
            vbo,
            vao,
            glyph_texture,
            query_ids: [0; 1]
        })
    }

    pub fn set_dimensions(&mut self, (width, height): (i32, i32)) {
        unsafe {
            gl::Viewport(0, 0, width, height);
        }
    }

    pub fn draw_vertices(&mut self, mut vertices: Vec<Vertex>) {
        let vertex_count = &mut self.vertex_count;
        let vertex_max = &mut self.vertex_max;

        vertices.sort_by(|v1, v2| {
            use std::cmp::Ordering::*;
            let z1 = v1[2];
            let z2 = v2[2];

            if z1 > z2 {
                Less
            } else if z2 < z1 {
                Greater
            } else {
                // NaN ends up here
                Equal
            }
        });
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

    pub fn update_texture(x: u32, y: u32, w: u32, h: u32, tex_data: &[u8]) {
        unsafe {
            gl::TexSubImage2D(
                gl::TEXTURE_2D,
                0,
                x as _,
                y as _,
                w as _,
                h as _,
                gl::RED,
                gl::UNSIGNED_BYTE,
                tex_data.as_ptr() as _,
            );
            gl_assert_ok!();
        }
    }

    pub fn resize_texture(new_width: u32, new_height: u32) {
        unsafe {
            // Recreate texture as a larger size to fit more
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::R8 as _,
                new_width as _,
                new_height as _,
                0,
                gl::RED,
                gl::UNSIGNED_BYTE,
                ptr::null(),
            );
            gl_assert_ok!();
        }
    }

    #[perf_viz::record]
    pub fn begin_frame(&mut self) {
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
            // currently faster, but this may well not be true any more on a future machine/driver
            // so it seems worth it to keep it a feature.
            unsafe {
                gl::GenQueries(1, self.query_ids.as_ptr() as _);
                gl::BeginQuery(gl::TIME_ELAPSED, self.query_ids[0])
            }
        }
    }

    #[perf_viz::record]
    pub fn end_frame(&mut self) {
        perf_viz::start_record!("gl::Clear & gl::DrawArraysInstanced");
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
            gl::DrawArraysInstanced(gl::TRIANGLE_STRIP, 0, 4, self.vertex_count as _);
        }
        perf_viz::end_record!("gl::Clear & gl::DrawArraysInstanced");
    
        //See comment in above "time-render" check.
        if cfg!(feature = "time-render") {
            perf_viz::record_guard!("query Finish");
            let query_ids = &mut self.query_ids;
            let mut time_elapsed = 0;
            unsafe {
                gl::EndQuery(gl::TIME_ELAPSED);
                gl::GetQueryObjectiv(query_ids[0], gl::QUERY_RESULT, &mut time_elapsed);
                gl::DeleteQueries(1, query_ids.as_ptr() as _);
            }
        } else {
            perf_viz::record_guard!("gl::Finish");
            unsafe {
                gl::Finish();
            }
        }
    }

    pub fn cleanup(
        &self,
    ) -> Res<()> {
        let &State {
            program,
            fs,
            vs,
            vbo,
            vao,
            glyph_texture,
            ..
        } = self;
        
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
            return Err(std::str::from_utf8(&buf)?.into());
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
            return Err(std::str::from_utf8(&buf)?.into());
        }
        Ok(program)
    }
}
