# ifndef PLATFORM_INT_TYPES_INCLUDED
# define PLATFORM_INT_TYPES_INCLUDED
// We avoid using any std library stuff in this header, including
// stdint.h and limits.h, so that implementations can include this
// header (and perhaps some headers that include it) above any feature
// defines, such as _POSIX_C_SOURCE etc. As such, we assume a 64 bit 
// machine, but we keep this header small to attempt to make it simple
// to swap these defines, if needed.

typedef unsigned char  u8;
#define U8MAX (255)
typedef signed char    i8;
typedef unsigned short u16;
#define U16MAX (65535)
typedef signed short   i16;
typedef unsigned int   u32;
#define U32MAX (4294967295)
typedef signed int     i32;
#define I32MAX (2147483647)
typedef unsigned long  u64;
#define U64MAX (18446744073709551615)
typedef signed long    i64;

/* 0xAARRGGBB
 *
 * 0xFFFF0000 is red
 * 0xFF00FF00 is green
 * 0xFF0000FF is blue
 */
typedef u32 pixel;

/*
 * The inner numeric type used to represent pixel positions
 */
// 64k pixel positions ought to be enough for anybody!
typedef u16 pixel_pos_inner;

#define PIXEL_POS_INNER_MAX U16MAX

/*
 * Width, in pixels.
 */
typedef struct { pixel_pos_inner w; } pixel_w;

/*
 * Height, in pixels.
 */
typedef struct { pixel_pos_inner h; } pixel_h;

# endif // PLATFORM_INT_TYPES_INCLUDED
