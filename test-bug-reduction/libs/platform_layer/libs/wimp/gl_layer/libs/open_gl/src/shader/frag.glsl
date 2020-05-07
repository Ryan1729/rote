#version 150

uniform sampler2D font_tex;

in vec2 f_tex_pos;
// Reminder: as of this writing this input colour is in sRGB so we have not
// enabled linear to sRGB conversion. This means that if you want to do colour
// math in here, then the input colour will need to be pre-linearized and the
// GL_FRAMEBUFFER_SRGB flag would need to be enabled.
in vec4 f_color;
in float f_override_alpha;

out vec4 out_color;

void main() {
    vec4 c = f_color.rgba;

    if (f_override_alpha == 0.0) {
        float alpha = texture(font_tex, f_tex_pos).r;
    
        if (alpha <= 0.0) {
            discard;
        }
    
        c.a *= alpha;
    } else {
        c.a = f_override_alpha;
    }

    out_color = c;
}
