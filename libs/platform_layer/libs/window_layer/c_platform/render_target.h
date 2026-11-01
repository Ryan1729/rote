#ifndef RENDER_TARGET_INCLUDED
# define RENDER_TARGET_INCLUDED

# include "platform_int_types.h"

typedef float render_target_depth;

typedef struct {
    pixel_w width;
    pixel_h height;
    pixel* frame_pixels;
    render_target_depth* depth_buffer;
} render_target;

void render_target_depth_clear(render_target target);

#endif // RENDER_TARGET_INCLUDED

#ifdef RENDER_TARGET_IMPLEMENTATION

void rasterize_target_depth_clear(rasterize_target target) {
    size_t depth_length = target.width * target.height;
    // Clear buffer to the minimum float value
    for (size_t i = 0; i < depth_length; i += 1) {
        target.depth_buffer[i] = -1./0.;
    }
}

#endif // RENDER_TARGET_IMPLEMENTATION

