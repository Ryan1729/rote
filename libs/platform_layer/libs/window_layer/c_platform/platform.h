#ifndef PLATFORM_INCLUDED
#define PLATFORM_INCLUDED

#include "platform_int_types.h"
#include "render_target.h"
#include "platform_for_input_keys.h"

typedef enum {
    PLATFORM_CMD_NONE,
    PLATFORM_CMD_EXIT
} platform_cmd;

typedef enum {
    PLATFORM_KEY_RELEASED = 0,
    PLATFORM_KEY_PRESSED = 1
} platform_key_state;

/*
 * The current input state of a given keyboard key. Designed to have any
 * questions a program may have about the user's input precomputed.
 */
typedef struct {
    bool was_just_pressed;
    platform_key_state key_state;
} platform_key_input;

#define PLATFORM_DEFINE_INPUT_FIELD(name, ...) platform_key_input name;

/*
 * The current keyboard input state. Designed to have any questions a
 * program may have about the user's input precomputed.
 */
typedef struct {
    PLATFORM_FOR_INPUT_KEYS( PLATFORM_DEFINE_INPUT_FIELD )
} platform_key_inputs;

/*
 * The current input state. Designed to have any questions a program may
 * have about the user's input precomputed.
 */
typedef struct {
    platform_key_inputs key;
} platform_input;

typedef u64 platform_nanos;

#define PLATFORM_NS_PER_S (1'000'000'000)
#define PLATFORM_NS_PER_MS (1'000'000)

/*
 * A context object. The paramter passed into the user's
 * update_and_render function.
 */
typedef struct {
    render_target target;
    platform_input input;
    platform_nanos elapsed;
    // Will be the pointer passed in in the options. Thus, if that was
    // NULL, it will be NULL.
    void *user_data;
} platform_ctx;

/*
 * A pointer to a function that would be declared like this:
 * platform_cmd
 * update_and_render(platform_ctx *ctx) {
 *     // ...
 * }
 */
typedef platform_cmd (platform_update_and_render)(platform_ctx *ctx);

typedef void (platform_on_close)(void *user_data);

typedef u16 platform_run_options_flags;

#define PLATFORM_DO_NOT_FLUSH_STDOUT_BEFORE_EXIT (1)

// Set to sane defaults if zero initialized
typedef struct {
    // May be NULL
    void *user_data;
    // May be NULL
    platform_on_close *on_close;
     // Also known as a "class" name for the application.
     // Implementations are encouraged to default this if it is NULL
    char *app_name;
    // Implementations are encouraged to default this if it is NULL
    char *window_name;
    // Zero indicates a sensible default should be used
    platform_nanos frame_interval;
    platform_run_options_flags flags;
} platform_run_options;

void platform_run(platform_update_and_render u_and_r, platform_run_options options);

#endif // PLATFORM_INCLUDED
