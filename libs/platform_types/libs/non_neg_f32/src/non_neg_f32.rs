#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct NonNegF32(f32);

#[macro_export]
macro_rules! is_non_neg {
    ($float: expr) => {{
        let f = $float;
        // `>= 0.0` is needed to eliminate positive NaNs
        // and `is_sign_positive` is needed to eliminate -0.0
        f >= 0.0 && f.is_sign_positive()
    }};
}

#[macro_export]
macro_rules! non_neg_f32 {
    ($float: literal) => {
        // const assertion 
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{ is_non_neg!($float) } as usize] = [];

        NonNegF32::new_saturating($float)
    };
    ($float: expr) => {
        NonNegF32::new_saturating($float)
    };
}

impl NonNegF32 {
    pub fn new_saturating(f: f32) -> Self {
        Self(if is_non_neg!(f) {
            f
        } else {
            // NaN ends up here
            0.0
        })
    }

    pub fn get(&self) -> f32 {
        self.0
    }

    pub fn to_bits(&self) -> u32 {
        self.0.to_bits()
    }
}

impl From<NonNegF32> for f32 {
    fn from(NonNegF32(f): NonNegF32) -> Self {
        f
    }
}

impl PartialEq<f32> for NonNegF32 {
    fn eq(&self, other: &f32) -> bool {
        self.0 == *other
    }
}

impl PartialEq<NonNegF32> for f32 {
    fn eq(&self, other: &NonNegF32) -> bool {
        *self == other.0
    }
}

impl PartialOrd<f32> for NonNegF32 {
    fn partial_cmp(&self, other: &f32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl PartialOrd<NonNegF32> for f32 {
    fn partial_cmp(&self, other: &NonNegF32) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.0)
    }
}