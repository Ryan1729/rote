#version 150

uniform sampler2D font_tex;

in vec2 f_tex_pos;
in vec4 f_color;
in float f_override_alpha;

out vec4 out_color;

void main() {
    float alpha = max(texture(font_tex, f_tex_pos).r, f_override_alpha);
    if (alpha <= 0.0) {
        discard;
    }
    out_color = f_color * vec4(1.0, 1.0, 1.0, alpha);
}
