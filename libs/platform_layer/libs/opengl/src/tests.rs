use super::*;
use platform_types::{highlight, CharOffset, Position};
use std::borrow::Borrow;

const W: i32 = 8;
const W_F: f32 = W as f32;
const H: i32 = 16;
const H_F: f32 = H as f32;

macro_rules! p_c {
    (($min_x: expr, $min_y: expr) (max, $max_y: expr)) => {
        p_c!(($min_x, $min_y)(i32::max_value(), $max_y))
    };
    (($min_x: expr, $min_y: expr) ($max_x: expr, $max_y: expr)) => {
        PixelCoords {
            min: ($min_x, $min_y).into(),
            max: ($max_x, $max_y).into(),
        }
    };
}

macro_rules! p_c_assert {
    ($highlight: expr, $output: expr) => {
        assert_eq!(
            highlight_to_pixel_coords($highlight.borrow(), (0.0, 0.0), CharDim { w: W_F, h: H_F }),
            $output,
            "\ninput: {:?}",
            $highlight
        );
    };
}

#[test]
fn highlight_to_pixel_coords_works() {
    // Do we care what this does?
    //p_c_assert!(highlight! {l 0 o 0 l 0 o 0}, p_c!((0, 0)(0, 0)));

    p_c_assert!(highlight! {l 0 o 0 l 0 o max}, p_c!((0, 0)(max, H)));

    p_c_assert!(
        highlight! {l 1 o 400 l 1 o max},
        p_c!((W * 400, H)(max, H * 2))
    );

    p_c_assert!(highlight! {l 2 o 0 l 2 o max}, p_c!((0, H * 2)(max, H * 3)));

    p_c_assert!(
        highlight! {l 3 o 0 l 3 o 300},
        p_c!((0, H * 3)(W * 300, H * 4))
    );
}
