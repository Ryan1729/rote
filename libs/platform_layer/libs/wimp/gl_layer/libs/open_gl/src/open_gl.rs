// This file was split off of a file that was part of https://github.com/alexheretic/glyph-brush

use std::{ffi::CString, mem, ptr, str};

use gl33::{*, global_loader::*};
pub use gl33::global_loader::load_global_gl;

use gl_layer_types::{DEPTH_MIN, DEPTH_MAX, Vertex, VERTEX_SPEC, Res};

use macros::{invariants_checked};

macro_rules! gl_assert_ok {
    () => {{
        if invariants_checked!() {
            let err = glGetError();
            assert_eq!(err, GL_NO_ERROR, "{}", gl_err_to_str(err));
        }
    }};
}

pub type LoadFnOutput = *const core::ffi::c_void;
pub type LoadFn<'a> = dyn Fn(*const u8) -> LoadFnOutput + 'a;

type GLint = i32;
type GLuint = u32;

/// As of this writing, [this opengl wiki page](https://www.khronos.org/opengl/wiki/OpenGL_Type)
/// says that `GLsizeiptr` should be an non-negative type. And the things we use it for
/// are sematically non-negative. But, the gl.xml file that the bindings are based on
/// says it's an `ssize_t`, (read as "signed pointer size type"). Sigh.
type GLsizeiptr = isize;

pub struct State {
    vertex_count: usize,
    vertex_max: usize,
    program: GLuint,
    fs: GLuint,
    vs: GLuint,
    v_buffer_o: GLuint,
    v_array_o: GLuint,
    glyph_texture: GLuint,
    query_ids: [GLuint; 1],
}

impl State {
    /// # Errors
    /// Returns an `Err` if the shaders cannot be compiled or linked.
    // `CString::new("out_color")?` won't return an `Err`, since "out_color" has no
    // nul char.
    pub fn new(
        clear_colour: [f32; 4], // the clear colour currently flashes up on exit.
        (width, height): (u32, u32),
        load_fn: &LoadFn<'_>,
    ) -> Res<Self> {
        // Load the OpenGL function pointers
        // SAFETY: The passed load_fn must always return accurate function pointer 
        // values, or null on failure.
        // TODO make this fn unsafe making that responsibilty clear to the caller.
        unsafe { load_global_gl(load_fn); }
    
        // Create GLSL shaders
        let vs = compile_shader(include_str!("shader/vert.glsl"), GL_VERTEX_SHADER)?;
        let fs = compile_shader(include_str!("shader/frag.glsl"), GL_FRAGMENT_SHADER)?;
        let program = link_program(vs, fs)?;
    
        let mut v_array_o = 0;
        let mut v_buffer_o = 0;
        let mut glyph_texture = 0;
    
        // SAFETY: We've set up the OpenGL stuff correctly.
        unsafe {
            // Create Vertex Array Object
            glGenVertexArrays(1, &mut v_array_o);
            glBindVertexArray(v_array_o);
    
            // Create a Vertex Buffer Object
            glGenBuffers(1, &mut v_buffer_o);
            glBindBuffer(GL_ARRAY_BUFFER, v_buffer_o);
    
            // Enable depth testing so we can occlude things while sending them down in any order
            glEnable(GL_DEPTH_TEST);
            glDepthRange(DEPTH_MIN.into(), DEPTH_MAX.into());
    
            {
                // We don't care if GL constants wrap. They only care about the bits.
                #[allow(clippy::cast_possible_wrap)]
                const CLAMP_TO_EDGE: GLint = GL_CLAMP_TO_EDGE.0 as _;
                #[allow(clippy::cast_possible_wrap)]
                const LINEAR: GLint = GL_LINEAR.0 as _;
                #[allow(clippy::cast_possible_wrap)]
                const R8: GLint = GL_R8.0 as _;
                // Create a texture for the glyphs
                // The texture holds 1 byte per pixel as alpha data
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
                glGenTextures(1, &mut glyph_texture);
                glBindTexture(GL_TEXTURE_2D, glyph_texture);
                
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, CLAMP_TO_EDGE);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, CLAMP_TO_EDGE);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, LINEAR);
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, LINEAR);
                
                glTexImage2D(
                    GL_TEXTURE_2D,
                    0,
                    R8,
                    width as _,
                    height as _,
                    0,
                    GL_RED,
                    GL_UNSIGNED_BYTE,
                    ptr::null(),
                );
                gl_assert_ok!();
            }
    
            // Use shader program
            glUseProgram(program);
            glBindFragDataLocation(
                program,
                0,
                CString::new("out_color")?.as_ptr() as _
            );
    
            // Specify the layout of the vertex data
            let mut offset = 0;
            for (v_field, float_count) in &VERTEX_SPEC {
                // Who cares what happens if a `Vertex` is over 2 billion bytes?
                #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
                const VERTEX_SIZE: GLint = mem::size_of::<Vertex>() as GLint;

                let attr: i32 = glGetAttribLocation(
                    program,
                    CString::new(*v_field)?.as_ptr() as _
                );
                if attr < 0 {
                    return Err(format!("{} GetAttribLocation -> {}", v_field, attr).into());
                }
                // We just checked if it was negative
                #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
                let attr = attr as GLuint;
                glVertexAttribPointer(
                    attr,
                    *float_count,
                    GL_FLOAT,
                    false as _,
                    VERTEX_SIZE,
                    offset as _,
                );
                glEnableVertexAttribArray(attr as _);
                glVertexAttribDivisor(attr as _, 1);
    
                offset += float_count * 4;
            }
    
            // Enabled alpha blending
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
            // We specifically do *not* enable `FRAMEBUFFER_SRGB` because we currently are passing
            // sRGB colours into the shader, rather than linear colours, so the extra linear to sRGB
            // conversion that this setting would apply, would make our colours too bright. If we want
            // to do colour blends in the shader, we'll need to enable this and convert our input
            // colours to linear ourselves.
            //glEnable(glFRAMEBUFFER_SRGB);
    
            glClearColor(
                clear_colour[0],
                clear_colour[1],
                clear_colour[2],
                clear_colour[3],
            );

            #[cfg(any())]
            {
                let mut depth_bits = 0;
                glGetIntegerv(GLenum(3414), &mut depth_bits);
                dbg!(depth_bits);
            }
        }
    
        let vertex_count = 0;
        let vertex_max = vertex_count;
    
        Ok(State {
            vertex_count,
            vertex_max,
            program,
            fs,
            vs,
            v_buffer_o,
            v_array_o,
            glyph_texture,
            query_ids: [0; 1]
        })
    }

    #[allow(clippy::unused_self)]
    pub fn set_dimensions(&mut self, (width, height): (GLuint, GLuint)) {
        // The documentation on `glViewport`, says it is meant to take GLsizei.
        // The documentation on `GLsizei`, says it is meant to be unsigned.
        #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
        let (width, height) = (width as GLint, height as GLint);
        // SAFETY: The otherwise unused `&mut self` prevents another thread from
        // calling this method.
        unsafe {
            glViewport(0, 0, width, height);
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
            } else if z1 < z2 {
                Greater
            } else {
                // NaN ends up here
                Equal
            }
        });
        // Draw new vertices
        *vertex_count = vertices.len();
        // SAFETY: We've set up the OpenGL stuff correctly.
        unsafe {
            if vertex_max < vertex_count {
                glBufferData(
                    GL_ARRAY_BUFFER,
                    (*vertex_count * mem::size_of::<Vertex>()) as GLsizeiptr,
                    vertices.as_ptr() as _,
                    GL_DYNAMIC_DRAW,
                );
            } else {
                glBufferSubData(
                    GL_ARRAY_BUFFER,
                    0,
                    (*vertex_count * mem::size_of::<Vertex>()) as GLsizeiptr,
                    vertices.as_ptr() as _,
                );
            }
        }
        *vertex_max = *vertex_max.max(vertex_count);
    }

    pub fn update_texture(x: GLuint, y: GLuint, w: GLuint, h: GLuint, tex_data: &[u8]) {
        unsafe {
            glTexSubImage2D(
                GL_TEXTURE_2D,
                0,
                x as GLint,
                y as GLint,
                w as GLint,
                h as GLint,
                GL_RED,
                GL_UNSIGNED_BYTE,
                tex_data.as_ptr() as _,
            );
            gl_assert_ok!();
        }
    }

    pub fn resize_texture(new_width: u32, new_height: u32) {
        unsafe {
            // Recreate texture as a larger size to fit more
            glTexImage2D(
                GL_TEXTURE_2D,
                0,
                GL_R8.0 as _,
                new_width as _,
                new_height as _,
                0,
                GL_RED,
                GL_UNSIGNED_BYTE,
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
                glGenQueries(1, self.query_ids.as_ptr() as _);
                glBeginQuery(GL_TIME_ELAPSED, self.query_ids[0])
            }
        }
    }

    #[perf_viz::record]
    pub fn end_frame(&mut self) {
        perf_viz::start_record!("glClear & glDrawArraysInstanced");
        unsafe {
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
            glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, self.vertex_count as _);
        }
        perf_viz::end_record!("glClear & glDrawArraysInstanced");
    
        //See comment in above "time-render" check.
        if cfg!(feature = "time-render") {
            perf_viz::record_guard!("query Finish");
            let query_ids = &mut self.query_ids;
            let mut time_elapsed = 0;
            unsafe {
                glEndQuery(GL_TIME_ELAPSED);
                glGetQueryObjectiv(query_ids[0], GL_QUERY_RESULT, &mut time_elapsed);
                glDeleteQueries(1, query_ids.as_ptr() as _);
            }
        } else {
            perf_viz::record_guard!("glFinish");
            unsafe {
                glFinish();
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
            v_buffer_o,
            v_array_o,
            glyph_texture,
            ..
        } = self;
        
        unsafe {
            glDeleteProgram(program);
            glDeleteShader(fs);
            glDeleteShader(vs);
            glDeleteBuffers(1, &v_buffer_o);
            glDeleteVertexArrays(1, &v_array_o);
            glDeleteTextures(1, &glyph_texture);
        }
    
        Ok(())
    }
}


fn gl_err_to_str(err: GLenum) -> &'static str {
    match err {
        GL_INVALID_ENUM => "INVALID_ENUM",
        GL_INVALID_VALUE => "INVALID_VALUE",
        GL_INVALID_OPERATION => "INVALID_OPERATION",
        GL_INVALID_FRAMEBUFFER_OPERATION => "INVALID_FRAMEBUFFER_OPERATION",
        GL_OUT_OF_MEMORY => "OUT_OF_MEMORY",
        GL_STACK_UNDERFLOW => "STACK_UNDERFLOW",
        GL_STACK_OVERFLOW => "STACK_OVERFLOW",
        _ => "Unknown error",
    }
}

fn compile_shader(src: &str, ty: GLenum) -> Res<GLuint> {
    let shader;
    unsafe {
        shader = glCreateShader(ty);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes())?;
        let string_array: [*const u8; 1] = [(&c_str.as_ptr()).cast()];
        glShaderSource(shader, 1, string_array.as_ptr(), ptr::null());
        glCompileShader(shader);

        // Get the compile status
        let mut status = false as _;
        glGetShaderiv(shader, GL_COMPILE_STATUS, &mut status);

        // Fail on error
        if status != true as _ {
            let mut len = 0;
            glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            glGetShaderInfoLog(
                shader,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut u8,
            );
            return Err(std::str::from_utf8(&buf)?.into());
        }
    }
    Ok(shader)
}

fn link_program(vs: GLuint, fs: GLuint) -> Res<GLuint> {
    unsafe {
        let program = glCreateProgram();
        glAttachShader(program, vs);
        glAttachShader(program, fs);
        glLinkProgram(program);
        // Get the link status
        let mut status = false as _;
        glGetProgramiv(program, GL_LINK_STATUS, &mut status);

        // Fail on error
        if status != true as _ {
            let mut len: GLint = 0;
            glGetProgramiv(program, GL_INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            buf.set_len((len as usize) - 1); // subtract 1 to skip the trailing null character
            glGetProgramInfoLog(
                program,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut u8,
            );
            return Err(std::str::from_utf8(&buf)?.into());
        }
        Ok(program)
    }
}
