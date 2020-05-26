/// Assumes `Add` is already impl'd
#[macro_export]
macro_rules! add_assign {
    (for $name: ty) => {
        impl std::ops::AddAssign for $name {
            fn add_assign(&mut self, other: $name) {
                use std::ops::Add;
                *self = self.add(other);
            }
        }
    };
    (<$rhs:ty> for $name: ty) => {
        impl std::ops::AddAssign<$rhs> for $name {
            fn add_assign(&mut self, other: $rhs) {
                use std::ops::Add;
                *self = self.add(other);
            }
        }
    };
}

/// Assumes `Sub` is already impl'd
#[macro_export]
macro_rules! sub_assign {
    (for $name: ty) => {
        impl std::ops::SubAssign for $name {
            fn sub_assign(&mut self, other: $name) {
                use std::ops::Sub;
                *self = self.sub(other);
            }
        }
    };
    (<$rhs:ty> for $name: ty) => {
        impl std::ops::SubAssign<$rhs> for $name {
            fn sub_assign(&mut self, other: $rhs) {
                use std::ops::Sub;
                *self = self.sub(other);
            }
        }
    };
}

/// Assumes `Mul` is already impl'd
#[macro_export]
macro_rules! mul_assign {
    (for $name: ty) => {
        impl std::ops::MulAssign for $name {
            fn mul_assign(&mut self, other: $name) {
                use std::ops::Mul;
                *self = self.mul(other);
            }
        }
    };
    (<$rhs:ty> for $name: ty) => {
        impl std::ops::MulAssign<$rhs> for $name {
            fn mul_assign(&mut self, other: $rhs) {
                use std::ops::Mul;
                *self = self.mul(other);
            }
        }
    };
}

/// Assumes `Div` is already impl'd
#[macro_export]
macro_rules! div_assign {
    (for $name: ty) => {
        impl std::ops::DivAssign for $name {
            fn div_assign(&mut self, other: $name) {
                use std::ops::Div;
                *self = self.div(other);
            }
        }
    };
    (<$rhs:ty> for $name: ty) => {
        impl std::ops::DivAssign<$rhs> for $name {
            fn div_assign(&mut self, other: $rhs) {
                use std::ops::Div;
                *self = self.div(other);
            }
        }
    };
}

#[macro_export]
macro_rules! hash {
    (for $name: ty: $self: ident, $state: ident in $code: expr) => {
        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, $state: &mut H) {
                let $self = self;
                $code
            }        }
    }
}

#[macro_export]
macro_rules! ord {
    (and friends for $name:ty : ($instance:ident) $code:block) => {
        $crate::ord!(and friends for $name : ordable1, ordable2 in {
            fn get_key($instance: &$name) -> impl std::cmp::Ord {
                $code
            }

            get_key(ordable1).cmp(&get_key(ordable2))
        });
    };
    (for $name:ty : ($instance:ident) $code:block) => {
        $crate::ord!(for $name : ordable1, ordable2 in {
            fn get_key($instance: &$name) -> impl std::cmp::Ord {
                $code
            }

            get_key(ordable1).cmp(&get_key(ordable2))
        });
    };
    (and friends for $name:ty : $self:ident, $other:ident in $code:expr) => {
        $crate::ord!(for $name : $self, $other in $code);

        impl std::cmp::PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                use std::cmp::Ord;
                Some(self.cmp(other))
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &Self) -> bool {
                use std::cmp::Ord;
                self.cmp(other) == std::cmp::Ordering::Equal
            }
        }

        impl Eq for $name {}
    };
    (for $name:ty : $self:ident, $other:ident in $code:expr) => {
        impl std::cmp::Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                #[allow(unused_imports)]
                use std::cmp::Ord;
                let $self = self;
                let $other = other;
                $code
            }
        }
    };
}

#[test]
fn ord_works_in_this_key_function_case_example() {
    enum OrdBool {
        False,
        True
    }

    ord!(and friends for OrdBool: (o) {
        use OrdBool::*;
        match o {
            False => 0,
            True => 1,
        }
    });

    // it impls Ord
    assert!(OrdBool::False < OrdBool::True);
    // it impls PartialEq
    assert!(OrdBool::False != OrdBool::True);
}

#[test]
fn ord_works_in_this_nested_key_function_case_example() {
    #[allow(dead_code)]
    enum OrdBool {
        False,
        True
    }
    
    macro_rules! ord_bool_key {
        ($o: expr) => {{
            use OrdBool::*;
            match $o {
                False => 0,
                True => 1,
            }
        }}
    }

    // precondition
    ord!(and friends for OrdBool: (o) {
        ord_bool_key!(o)
    });

    enum OrdOptBool {
        Some(OrdBool),
        None
    }

    ord!(and friends for OrdOptBool: (o) {
        use OrdOptBool::*;
        match o {
            None => 0,
            Some(b) => 1 + ord_bool_key!(b),
        }
    });

    // it impls Ord
    assert!(OrdOptBool::None < OrdOptBool::Some(OrdBool::True));
    // it impls PartialEq
    assert!(OrdOptBool::None != OrdOptBool::Some(OrdBool::True));
}

/// The versions from num-traits don't allow the operands or the result to be different types.
pub mod traits {
    // TODO move these Add/Sub traits into their own crate?
    // that would require users to import that other crate unless we re-export.

    pub trait CheckedAdd<Rhs = Self> {
        type Output;

        #[must_use]
        fn checked_add(self, rhs: Rhs) -> Option<Self::Output>;
    }
    
    pub trait CheckedSub<Rhs = Self> {
        type Output;
    
        #[must_use]
        fn checked_sub(self, rhs: Rhs) -> Option<Self::Output>;
    }
    
    pub trait SaturatingAdd<Rhs = Self> {
        type Output;
    
        #[must_use]
        fn saturating_add(self, rhs: Rhs) -> Self::Output;
    }
    
    pub trait SaturatingSub<Rhs = Self> {
        type Output;
    
        #[must_use]
        fn saturating_sub(self, rhs: Rhs) -> Self::Output;
    }
}
u!{pub traits}

#[macro_export]
macro_rules! usize_newtype {
    ($name: ident) => {
        impl std::ops::Add<usize> for $name {
            type Output = $name;

            fn add(self, other: usize) -> $name {
                $name(self.0 + other)
            }
        }

        $crate::add_assign!(<usize> for $name);

        impl $crate::SaturatingAdd<usize> for $name {
            type Output = $name;

            fn saturating_add(self, other: usize) -> $name {
                $name(self.0.saturating_add(other))
            }
        }

        impl $crate::CheckedAdd<usize> for $name {
            type Output = $name;

            fn checked_add(self, other: usize) -> Option<$name> {
                self.0.checked_add(other).map($name)
            }
        }

        impl std::ops::Sub<usize> for $name {
            type Output = $name;

            fn sub(self, other: usize) -> $name {
                $name(self.0 - other)
            }
        }

        $crate::sub_assign!(<usize> for $name);

        impl std::ops::Sub<$name> for usize {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                $name(self - other.0)
            }
        }

        impl $crate::SaturatingSub<usize> for $name {
            type Output = $name;

            fn saturating_sub(self, other: usize) -> $name {
                $name(self.0.saturating_sub(other))
            }
        }

        impl $crate::SaturatingSub<$name> for usize {
            type Output = $name;

            fn saturating_sub(self, other: $name) -> $name {
                $name(self.saturating_sub(other.0))
            }
        }

        impl $crate::CheckedSub<usize> for $name {
            type Output = $name;

            fn checked_sub(self, other: usize) -> Option<$name> {
                self.0.checked_sub(other).map($name)
            }
        }

        impl $crate::CheckedSub<$name> for usize {
            type Output = $name;

            fn checked_sub(self, other: $name) -> Option<$name> {
                self.checked_sub(other.0).map($name)
            }
        }


        impl PartialOrd<$name> for usize {
            fn partial_cmp(&self, other: &$name) -> Option<std::cmp::Ordering> {
                Some(self.cmp(&other.0))
            }
        }

        impl PartialEq<$name> for usize {
            fn eq(&self, other: &$name) -> bool {
                *self == other.0
            }
        }

        impl PartialOrd<usize> for $name {
            fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
                Some(self.0.cmp(&other))
            }
        }

        impl PartialEq<usize> for $name {
            fn eq(&self, other: &usize) -> bool {
                self.0 == *other
            }
        }
    };
}

#[macro_export]
macro_rules! integer_newtype {
    ($name: ident) => {
        impl std::ops::Add for $name {
            type Output = $name;

            fn add(self, other: $name) -> $name {
                $name(self.0 + other.0)
            }
        }

        $crate::add_assign!(for $name);

        impl $crate::CheckedAdd for $name {
            type Output = Self;
            fn checked_add(self, other: Self) -> Option<Self> {
                self.0.checked_add(other.0).map($name)
            }
        }

        impl $crate::SaturatingAdd for $name {
            type Output = $name;

            fn saturating_add(self, other: Self) -> $name {
                $name(self.0.saturating_add(other.0))
            }
        }

        impl $name {
            /// Seems like 99% of the time we want to do a `checked_add` it's with one
            pub fn checked_add_one(self) -> Option<Self> {
                self.0.checked_add(1).map($name)
            }
        }

        impl std::ops::Sub for $name {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                $name(self.0 - other.0)
            }
        }

        $crate::sub_assign!(for $name);

        impl $crate::CheckedSub for $name {
            type Output = Self;
            fn checked_sub(self, other: Self) -> Option<Self> {
                self.0.checked_sub(other.0).map($name)
            }
        }

        impl $crate::SaturatingSub for $name {
            type Output = $name;

            fn saturating_sub(self, other: Self) -> $name {
                $name(self.0.saturating_sub(other.0))
            }
        }

        impl $name {
            /// Seems like 99% of the time we want to do a `checked_sub` it's with one
            pub fn checked_sub_one(self) -> Option<Self> {
                self.0.checked_sub(1).map($name)
            }
        }

        $crate::ord!(and friends for $name: s, other in s.0.cmp(&other.0));
    };
}

/// Short for `Default::default()`, also makes implementing `Default` more terse.
#[macro_export]
macro_rules! d {
    () => {
        Default::default()
    };
    //the generics monstrosity originated at https://stackoverflow.com/a/52135598
    (
        $(
            <$($generic_name:ident $(: $generic_trait_1:ident $(+ $generic_trait_n:ident)* )? ),+>
        )?
        for $name:ty : $code:expr) => {
        impl $(< $($generic_name $(: $generic_trait_1 $(+ $generic_trait_n)* )* ),* >)?
        Default for $name {
            fn default() -> Self {
                $code
            }
        }
    };
}

/// Short for `use something::*;`.
/// 
/// This is intended mainly for enum imports.
/// So 
/// ```rust
/// # enum SomeEnum { /* ... */ }
/// use SomeEnum::*;
/// ```
/// can become
/// ```rust
/// # enum SomeEnum { /* ... */ }
/// # use macros::{u};
/// u!{SomeEnum}
/// ```
/// which is four characters shorter, plus th import which gets amortized over all usages
/// in the file. In my opinion it is no less understandable, so it seems worth the import
/// in cases where it is used in more than `"macros::{u};".len()/4 == 3` usages.
#[macro_export]
macro_rules! u {
    ($vis: vis $($path: path),+) => {
        $(
            $vis use $path::*;
        )+
    };
    ($($path: path),+) => {
        $(
            use $path::*;
        )+
    };
    () => {
        /// This seems like the most common use case that that is not specific to a particular file.
        /// Note that you cannot always use this to import itself, so you may need to spell out either
        /// `use super::*;` or the macro import. 
        use super::*;
    }
}

#[macro_export]
macro_rules! fmt_display {
    ($(<$generics:tt>)? for $name:ty : $pattern:pat in $($args:tt)+) => (
        impl $(<$generics>)? std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                let $pattern = self;
                write!(f, $($args)+)
            }
        }
    );
}

/// collapse default mode Example: 
/// ```
/// # #[macro_use] extern crate macros; fn main() {
/// struct Ex {
///     blank_me: i32,
///     show_me: String,
/// }
/// 
/// fmt_debug!(collapse default for Ex : me {
///     blank_if_default!(blank_me);
///     field!(show_me);
/// });
/// # }
/// ```
#[macro_export]
macro_rules! fmt_debug {
    ($(<$generics:tt>)? for $name:ty : $pattern:pat in $($args:tt)+) => (
        impl $(<$generics>)? std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                let $pattern = self;
                write!(f, $($args)+)
            }
        }
    );
    (collapse default for $name: ident $(:)? $self: ident $code: block) => {
        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut s = f.debug_struct(stringify!($name));
        
                let $self = self;

                macro_rules! field { 
                    ($field: ident) => {
                        s.field(stringify!($field), &$self.$field);
                    }
                }
        
                let mut any_defaulted = false;
                macro_rules! blank_if_default {
                    ($field: ident) => {
                        blank_if_default!($field, $self.$field == d!())
                    };
                    ($field: ident, $is_default: expr) => {
                        if $is_default {
                            any_defaulted = true;
                        } else {
                            field!($field);
                        }
                    }
                }
        
                $code;
        
                if any_defaulted {
                    s.field(".. d!", &());
                }
        
                s.finish()
            }
        }
    };
}

#[macro_export]
macro_rules! borrow {
    (<$type:ty> for $name:ty : $self:ident in $code:expr) => {
        impl std::borrow::Borrow<$type> for $name {
            fn borrow(&self) -> &$type {
                let $self = self;
                $code
            }
        }
    };
}

#[macro_export]
macro_rules! borrow_mut {
    (<$type:ty> for $name:ty : $self:ident in $code:expr) => {
        impl std::borrow::BorrowMut<$type> for $name {
            fn borrow_mut(&mut self) -> &mut $type {
                let $self = self;
                $code
            }
        }
    };
}

#[macro_export]
macro_rules! some_if {
    ($condition: expr => $output: expr) => {{
        if $condition {
            Some($output)
        } else {
            None
        }
    }};
}

#[macro_export]
macro_rules! some_or {
    ($option: expr, $or: expr) => {{
        if let Some(thing) = $option {
            thing
        } else {
            $or
        }
    }};
}

#[macro_export]
macro_rules! ok_or {
    ($result: expr, $or: expr) => {{
        if let Ok(thing) = $result {
            thing
        } else {
            $or
        }
    }};
}

/// Shortens expressions that evaluate to the usual `[f32; 4] type` used for representing colour.
#[macro_export]
macro_rules! c {
    (
        $red: expr,
        $green: expr,
        $blue: expr $(,)?
    ) => {
        c![$red, $green, $blue, 1.0]
    };

    (
        $red: expr,
        $green: expr,
        $blue: expr,
        $alpha: expr $(,)?
    ) => {
        [$red, $green, $blue, $alpha]
    };
}

#[macro_export]
macro_rules! extra_prints {
    () => {{
        cfg!(feature = "extra-prints")
    }};
}

#[macro_export]
macro_rules! dbg {
    () => {
        if $crate::extra_prints!() {
            std::dbg!()
        } else {

        }
    };
    ($val:expr) => {
        if $crate::extra_prints!() {
            std::dbg!(
                $val
            )
        } else {
            $val
        }
    };
    // Trailing comma with single argument is ignored
    ($val:expr,) => { dbg!($val) };
    ($($val:expr),+ $(,)?) => {
        if $crate::extra_prints!() {
            std::dbg!($($val),+,)
        } else {
            ($($val),+,)
        }
    };
    ($($args: tt)*) => {
        
    };
}

#[cfg(feature = "invariant-checking")]
#[macro_export]
macro_rules! invariant_violation {
    () => ({
        invariant_violation(&format!("invariant was violated! {}:{}", file!(), line!()));
        panic!("invariant was violated!")
    });
    ($code:block, $($rest:tt)*) => {
        invariant_violation!($($rest)*)
    };
    ($msg:expr) => ({
        invariant_violation(&format!("{} {}:{}", $msg, file!(), line!()));
        panic!($msg)
    });
    ($msg:expr,) => (
        invariant_violation!($msg)
    );
    ($fmt:expr, $($arg:tt)+) => ({
        invariant_violation(&format!($fmt, $($arg)*));
        panic!($fmt, $($arg)*)
    });
}

#[cfg(not(feature = "invariant-checking"))]
#[macro_export]
macro_rules! invariant_violation {
    ($code:block, $($rest:tt)*) => {
        $code
    };
    ($($whatever:tt)*) => {};
}

#[cfg(feature = "invariant-checking")]
#[macro_export]
macro_rules! invariant_assert {
    ($($arg:tt)+) => ({
        assert!($($arg)*)
    });
}

#[cfg(not(feature = "invariant-checking"))]
#[macro_export]
macro_rules! invariant_assert {
    ($($whatever:tt)*) => {};
}

#[cfg(feature = "invariant-checking")]
#[macro_export]
macro_rules! invariant_assert_eq {
    ($($arg:tt)+) => ({
        assert_eq!($($arg)*)
    });
}

#[cfg(not(feature = "invariant-checking"))]
#[macro_export]
macro_rules! invariant_assert_eq {
    ($($whatever:tt)*) => {};
}

/// Short for `cfg!(feature = "invariant-checking")`
///
/// This is only slightly nicer to use than using the body of the macro directly, but
/// it's nice to have all the features stuff in one place as a form of documentation.
#[macro_export]
macro_rules! invariants_checked {
    () => {{
        cfg!(feature = "invariant-checking")
    }};
}

