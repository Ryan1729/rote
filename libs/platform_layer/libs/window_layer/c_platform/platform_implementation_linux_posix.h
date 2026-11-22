#include "platform.h"
// FIXME Need to add this, and several oter libraries that we included in the main c file in the prototype
// #include "rasterize.h"

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif

#if defined(__clang__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wimplicit-fallthrough"
#endif

#define RGFW_IMPLEMENTATION
#include "RGFW.h"

#if defined(__clang__)
# pragma clang diagnostic pop
#endif

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef PLATFORM_DIAGNOSTICS
    #define PLATFORM_DIAGNOSTIC(...) fprintf(stderr, __VA_ARGS__);
#else
    #define PLATFORM_DIAGNOSTIC(...)
#endif

#define PLATFORM_KEY_BITS 512

// FIXME these key types should probably be back in platform.h
// Or maybe a predeclaration? There should be a usage example that
// builds under the dummy implementation to force this
// FIXME once we are fairly sure what things will go where,
// use a per platform prefix for things
typedef unsigned _BitInt(PLATFORM_KEY_BITS) platform_key_flags;

/*
A suggested way to loop over all the flags:
    for (int i = 0; i < PLATFORM_KEY_BITS; i += 1) {
        platform_key_flags flag = PLATFORM_KEY_FLAG_AT_BIT(i);
        // ...
    }
And backwards, which is useful for printing the bits in MSB order
    for (int i = PLATFORM_KEY_BITS - 1; i >= 0; i -= 1) {
        platform_key_flags flag = PLATFORM_KEY_FLAG_AT_BIT(i);
        // ...
    }
*/
#define PLATFORM_KEY_FLAG_AT_BIT(n) ((platform_key_flags)1 << (platform_key_flags)n)

#define PLATFORM_KEY_BACK (PLATFORM_KEY_FLAG_AT_BIT(16))
#define PLATFORM_KEY_FORWARD (PLATFORM_KEY_FLAG_AT_BIT(17))
#define PLATFORM_KEY_REFRESH (PLATFORM_KEY_FLAG_AT_BIT(18))

typedef struct {
    platform_key_flags key_states;
    platform_key_flags was_just_pressed;
} platform_keys;

typedef struct {
    platform_keys keys;
} platform_frame;

struct client_state {
    /* State */
    render_target target;
    platform_nanos last_frame_start;
    platform_nanos frame_interval;
    bool closed;
    platform_frame previous_frame;
    platform_frame current_frame;
    /* User provided */
    platform_update_and_render *update_and_render;
    void *user_data;
};

static clockid_t
platform_get_clockid(void) {
    static clockid_t clock = -1;
    if (clock == -1) {
        struct timespec ts;
        #if defined(_POSIX_MONOTONIC_CLOCK)
        if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0)
            clock = CLOCK_MONOTONIC;
        else
        #endif
            clock = CLOCK_REALTIME;
    }
    return clock;
}

static platform_nanos
platform_get_time(void) {
    struct timespec ts;
    clock_gettime(platform_get_clockid(), &ts);
    return (platform_nanos)ts.tv_sec * PLATFORM_NS_PER_S + (platform_nanos)ts.tv_nsec;
}

static void
platform_sleep_until_imprecise(platform_nanos nanos) {
    struct timespec t = (struct timespec) {
        .tv_sec = (long int)(nanos / PLATFORM_NS_PER_S),
        .tv_nsec = (long int)(nanos % PLATFORM_NS_PER_S)
    };

    clockid_t clock = platform_get_clockid();

    errno = 0;
    do {
        clock_nanosleep(clock, TIMER_ABSTIME, &t, NULL);
    } while (errno == EINTR);
}

#ifndef PLATFORM_SLEEP_UNIT
// Default to 1 ms, it's a common minimum guaranteed to produce
// real sleeps across many platforms.
# define PLATFORM_SLEEP_UNIT (PLATFORM_NS_PER_MS)
#endif

static void
platform_sleep_until(platform_nanos target) {
    platform_nanos now = platform_get_time();

    // Sleep until a small time before the target, in constant sized
    // chunks
    platform_nanos max_slept = PLATFORM_SLEEP_UNIT;
    while (now + max_slept < target) {
        // Sleep for a short time
        platform_sleep_until_imprecise(now + PLATFORM_SLEEP_UNIT);

        const platform_nanos new_now = platform_get_time();
        const platform_nanos next_sleep_ns = (new_now - now);
        if (next_sleep_ns > max_slept) {
            max_slept = next_sleep_ns;
        }
        now = new_now;
    }

    // Sleep a shorter time, subtracting the unit from max_slept to
    // get the amount we ever overshot by.
    platform_nanos max_overshoot = max_slept - PLATFORM_SLEEP_UNIT;
    if (now < target && (target - now) >= max_overshoot) {
        platform_sleep_until_imprecise(target - max_overshoot);
        now = platform_get_time();
    }

    // Sleep for large chunks again, in case the OS sleep made us
    // undershoot by a lot.
    while (now + PLATFORM_SLEEP_UNIT < target) {
        platform_sleep_until_imprecise(now + PLATFORM_SLEEP_UNIT);
        now = platform_get_time();
    }

    // Spin for any remaining time
    while (now < target) {
        // Each loop will take some amount of time, unless the compiler
        // over-optimizes.
        now = platform_get_time();
    }
}

static void
platform_frame_begin(struct client_state *state) {
    //
    // Calculate the new current frame's state, comparing to the
    // previous frame as needed.
    //
    platform_keys new_keys = (platform_keys){
        // state->current_frame.keys.key_states has the values of things
        // that were pressed during what was the last frame. We want to take
        // those into account.
        .was_just_pressed =
            ~state->previous_frame.keys.key_states
            & state->current_frame.keys.key_states,
        // The current frame's key states are the newest we know.
        .key_states = state->current_frame.keys.key_states,
    };

    // Advance time
    state->previous_frame.keys = state->current_frame.keys;
    state->current_frame.keys = new_keys;
}

static void
platform_frame_end([[maybe_unused]] struct client_state *state) {
    // Nothing to do
}

static void
platform_draw_frame(struct client_state *state, platform_nanos current_frame_start)
{
    platform_frame_begin(state);

#ifdef PLATFORM_DIAGNOSTICS
    if (state->current_frame.keys.was_just_pressed) {
        for (int i = PLATFORM_KEY_BITS - 1; i >= 0; i -= 1) {
            if (state->current_frame.keys.was_just_pressed & PLATFORM_KEY_FLAG_AT_BIT(i)) {
                PLATFORM_DIAGNOSTIC("1");
            } else {
                PLATFORM_DIAGNOSTIC("0");
            }
        }
        PLATFORM_DIAGNOSTIC("\n");
    }
#endif

#define PLATFORM_ASSIGN_INPUT(name, bit_number) \
    .name = { \
        .was_just_pressed = (state->current_frame.keys.was_just_pressed & PLATFORM_KEY_FLAG_AT_BIT(bit_number)) != 0, \
        .key_state = (state->current_frame.keys.key_states & PLATFORM_KEY_FLAG_AT_BIT(bit_number)) != 0, \
    }, \


    platform_input input = {
        .key = {
            PLATFORM_FOR_INPUT_KEYS( PLATFORM_ASSIGN_INPUT )
        }
    };

#define PLATFORM_ASSERT_INPUT_BOOLS(name, ...) \
	assert(input.key.name.was_just_pressed == false || input.key.name.was_just_pressed == true); \

	PLATFORM_FOR_INPUT_KEYS( PLATFORM_ASSERT_INPUT_BOOLS )

    platform_ctx ctx = {
        .target = state->target,
        .input = input,
        .elapsed = current_frame_start - state->last_frame_start,
        .user_data = state->user_data,
    };

    render_target_depth_clear(state->target);

    platform_cmd cmd = state->update_and_render(&ctx);

    switch (cmd) {
        case PLATFORM_CMD_NONE:
            // Nothing to do
        break;
        case PLATFORM_CMD_EXIT:
            state->closed = true;
        break;
        default:
            PLATFORM_DIAGNOSTIC("Unknown platform_cmd value: %d", cmd);
        break;
    }

    platform_frame_end(state);
}

// Returns 0 on 
platform_key_flags platform_rgfw_key_to_flags(RGFW_key key) {
    platform_key_flags flag = 0;
    
#define PLATFORM_SET_FLAG(flag_number) flag = (platform_key_flags)1 << (flag_number);

    if (key > 0 && key < 0x20) {
        PLATFORM_SET_FLAG(0x0100 | (key & 0xFF));
    } else if (key >= 0x20 && key <= 0x7E) {
        PLATFORM_SET_FLAG(key);
    } else if (key == RGFW_keyDelete) { // 0x7F
        PLATFORM_SET_FLAG(0x1FF); // delete
    } else if (key >= 0x80 && key <= 0x98) {
        PLATFORM_SET_FLAG(key - 0x80 + 0x1BE);
    } else if (key == RGFW_keyCapsLock) { // 0x99
        PLATFORM_SET_FLAG(0x1E5); // capslock
    } else if (key == RGFW_keyShiftL) { // 0x9a
        PLATFORM_SET_FLAG(0x1E1); // left_shift
    } else if (key == RGFW_keyControlL) { // 0x9b
        PLATFORM_SET_FLAG(0x1E3); // left_control
    } else if (key == RGFW_keyAltL) { // 0x9c
        PLATFORM_SET_FLAG(0x1E9); // left_alt
    } else if (key == RGFW_keySuperL) { // 0x9d
        PLATFORM_SET_FLAG(0x1EB); // left_super
    } else if (key == RGFW_keyShiftR) { // 0x9e
        PLATFORM_SET_FLAG(0x1E2); // right_shift
    } else if (key == RGFW_keyControlR) { // 0x9f
        PLATFORM_SET_FLAG(0x1E4); // right_control
    } else if (key == RGFW_keyAltR) { // 0xa0
        PLATFORM_SET_FLAG(0x1EA); // right_alt
    } else if (key == RGFW_keySuperR) { // 0xa1
        PLATFORM_SET_FLAG(0x1EC); // right_super
    } else if (key == RGFW_keyUp) { // 0xa2
        PLATFORM_SET_FLAG(0x152); // up
    } else if (key == RGFW_keyDown) { // 0xa3
        PLATFORM_SET_FLAG(0x154); // down
    } else if (key == RGFW_keyLeft) { // 0xa4
        PLATFORM_SET_FLAG(0x151); // left
    } else if (key == RGFW_keyRight) { // 0xa5
        PLATFORM_SET_FLAG(0x153); // right
    } else if (key == RGFW_keyInsert) { // 0xa6
        PLATFORM_SET_FLAG(0x163); // insert
    } else if (key == RGFW_keyMenu) { // 0xa7
        PLATFORM_SET_FLAG(0x167); // menu 
    } else if (key == RGFW_keyEnd) { // 0xa8
        PLATFORM_SET_FLAG(0x157); // end
    } else if (key == RGFW_keyHome) { // 0xa9
        PLATFORM_SET_FLAG(0x158); // begin
    } else if (key == RGFW_keyPageUp) { // 0xaa
        PLATFORM_SET_FLAG(0x155); // page_up
    } else if (key == RGFW_keyPageDown) { // 0xab
        PLATFORM_SET_FLAG(0x156); // page_down
    } else if (key == RGFW_keyNumLock) { // 0xac
        PLATFORM_SET_FLAG(0x17F); // num_lock
    } else if (key == RGFW_keyPadSlash) { // 0xad
        PLATFORM_SET_FLAG(0x1AF); // keypad_slash
    } else if (key == RGFW_keyPadMultiply) { // 0xae
        PLATFORM_SET_FLAG(0x1AA); // keypad_asterisk
    } else if (key == RGFW_keyPadPlus) { // 0xaf
        PLATFORM_SET_FLAG(0x1AB); // keypad_plus
    } else if (key == RGFW_keyPadMinus) { // 0xb0
        PLATFORM_SET_FLAG(0x1AD); // keypad_minus
    } else if (key == RGFW_keyPadEqual) { // 0xb2
        PLATFORM_SET_FLAG(0x1BD); // keypad_equal
    } else if (key >= RGFW_keyPad1 && key <= RGFW_keyPad9) { // 0xb3-a
        // Keypad 7 and 8 don't show up here for soem reason?
        PLATFORM_SET_FLAG(key - RGFW_keyPad1 + 0x1B1);
    } else if (key == RGFW_keyPad0) { // 0xbb
        PLATFORM_SET_FLAG(0x1B0); // keypad_zero
    } else if (key == RGFW_keyPadPeriod) { // 0xbc
        PLATFORM_SET_FLAG(0x1AE); // keypad_period
    } else if (key == RGFW_keyPadReturn) { // 0xbd
        PLATFORM_SET_FLAG(0x18D); // keypad_enter
    } else if (key == RGFW_keyScrollLock) { // 0xbe
        PLATFORM_SET_FLAG(0x114); // srcoll_lock
    } else if (key == RGFW_keyPrintScreen) { // 0xbf
        PLATFORM_SET_FLAG(0x115); // system_request
    } else if (key == RGFW_keyPause) { // 0xc0
        PLATFORM_SET_FLAG(0x113); // pause
    }

#undef PLATFORM_SET_FLAG


    return flag;
}

void platform_run(platform_update_and_render u_and_r, platform_run_options options) {
    struct client_state state = {};

    state.target.width = (pixel_w){640};
    state.target.height = (pixel_h){480};
    state.frame_interval = options.frame_interval;
    if (state.frame_interval == 0) {
        state.frame_interval = PLATFORM_NS_PER_S / 60;
    }

    state.update_and_render = u_and_r;
    state.user_data = options.user_data;

    RGFW_init(
        options.app_name ? options.app_name : "Unnamed Application",
        0
    );

    RGFW_window* win = RGFW_createWindow(
        options.window_name ? options.window_name : "Unnamed Window",
        0, 0,
        state.target.width.w, state.target.height.h,
        RGFW_windowCenter | RGFW_windowTransparent
    );
    RGFW_window_setExitKey(win, RGFW_keyEscape);

    RGFW_format pixel_format = RGFW_nativeFormat();
    if (pixel_format != RGFW_formatBGRA8) {
        PLATFORM_DIAGNOSTIC("Native pixel format is not BGRA8. Switching to non-native format. Performance may be degraded.\n");
        pixel_format = RGFW_formatBGRA8;
    }

    state.target.frame_pixels = RGFW_ALLOC((u32)(
        state.target.width.w * state.target.height.h * 4
    ));
	RGFW_surface* surface = RGFW_createSurface(
        (unsigned char*)state.target.frame_pixels,
        state.target.width.w,
        state.target.height.h,
        pixel_format
    );
    
    size_t depth_length = state.target.width.w * state.target.height.h;
    state.target.depth_buffer = RGFW_ALLOC(depth_length * sizeof(render_target_depth));

    state.last_frame_start = platform_get_time();
    bool window_was_resized = false;

     /* main loop */
    while (RGFW_window_shouldClose(win) == RGFW_FALSE) {
        platform_nanos frame_start = platform_get_time();
        //
        // Event Handling
        //
        RGFW_event event;
        while (RGFW_window_checkEvent(win, &event)) {
            if (event.type == RGFW_windowClose) break;

            switch (event.type) {
                case RGFW_eventNone: break;
                case RGFW_keyPressed: {
                    platform_key_flags pressed_flag = 
                        platform_rgfw_key_to_flags(event.key.value);

                    if (pressed_flag) {
                       state.current_frame.keys.key_states |= pressed_flag;
                    } else {
                        PLATFORM_DIAGNOSTIC("Unknown key pressed: %x\n", event.key.value);
                    }
                    
                    break;
                }
                case RGFW_keyReleased: {
                    platform_key_flags released_flag = 
                        platform_rgfw_key_to_flags(event.key.value);
                        
                    if (released_flag) {
                       state.current_frame.keys.key_states &= ~released_flag;
                    } else {
                        PLATFORM_DIAGNOSTIC("Unknown key released: %x\n", event.key.value);
                    }
                    
                    break;
                }
                case RGFW_keyChar: {
                    // Ignored for now
                    //u32 str[2] = {(char)event.keyChar.value, '\0'};

                    break;
                }
                case RGFW_mouseButtonReleased: break;
                case RGFW_mouseButtonPressed: break;
                case RGFW_mouseScroll: break;
                case RGFW_mouseMotion: break;
                case RGFW_mouseRawMotion: break;
                case RGFW_mouseEnter: break;
                case RGFW_mouseLeave: break;
                case RGFW_windowMoved: break;
                case RGFW_windowResized: {
                    i32 w, h;
                    bool success = RGFW_window_fetchSize(win, &w, &h);
                    if (success) {
                        window_was_resized = true;
                        state.target.width = (pixel_w){w > ((i32)PIXEL_POS_INNER_MAX) ? PIXEL_POS_INNER_MAX : (pixel_pos_inner)w};
                        state.target.height = (pixel_h){h > ((i32)PIXEL_POS_INNER_MAX) ? PIXEL_POS_INNER_MAX : (pixel_pos_inner)h};
                    }
                    // TODO? Announce failure somehow?

                    break;
                }
                case RGFW_windowFocusIn: break;
                case RGFW_windowFocusOut: break;
                case RGFW_windowRefresh: break;
                case RGFW_windowClose: break;
                case RGFW_windowMaximized: break;
                case RGFW_windowMinimized: break;
                case RGFW_windowRestored: break;
                case RGFW_dataDrop: break;
                case RGFW_dataDrag: break;
                case RGFW_scaleUpdated: break;
                case RGFW_monitorConnected: break;
                case RGFW_monitorDisconnected: break;
                default: {
                    break;
                }
            }
        }

        // Handle changes to the width and height
        if (window_was_resized) {
            RGFW_surface_free(surface);
            RGFW_free(state.target.frame_pixels);
            state.target.frame_pixels = RGFW_ALLOC((u32)(state.target.width.w * state.target.height.h * 4));
            surface = RGFW_createSurface((unsigned char*)state.target.frame_pixels, state.target.width.w, state.target.height.h, pixel_format);
            
            RGFW_free(state.target.depth_buffer);
            size_t depth_length = state.target.width.w * state.target.height.h;
            state.target.depth_buffer = RGFW_ALLOC(depth_length * sizeof(render_target_depth));
            
            window_was_resized = false;
        }

        //
        // Render
        //

        platform_draw_frame(&state, frame_start);

        RGFW_window_blitSurface(win, surface);

        //
        // Sleep
        //

        platform_sleep_until(frame_start + state.frame_interval);

        state.last_frame_start = frame_start;
    }

    // Closing the app

    if (options.on_close) {
        options.on_close(state.user_data);
    }

    // This is handy in case a program using this library forgets to
    // put a newline while printf debugging. Add a way to disable it
    // just in case though.
    if (!(options.flags & PLATFORM_DO_NOT_FLUSH_STDOUT_BEFORE_EXIT)) {
        fflush(stdout);
    }

    // We'll bother to actually cleanup things that naturally live until
    // just before the program exits only when actually testing for
    // leaks, because the OS will clean up the memory for us anyway,
    // all at once. So for release builds actually freeing things is
    // just wasting time. When we are testing for leaks though, actually
    // freeing stuff and observing a lack of leak warnings ensures we
    // haven't missed any real leaks.
#if PLATFORM_ENABLE_CLEANUP
    RGFW_surface_free(surface);
    RGFW_free(state.target.frame_pixels);
#endif
}
