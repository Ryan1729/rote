// Assumes `Add` is already impl'd
#[macro_export]
macro_rules! add_assign {
    (for $name: ident) => {
        impl std::ops::AddAssign for $name {
            fn add_assign(&mut self, other: $name) {
                use std::ops::Add;
                *self = self.add(other);
            }
        }
    };
    (<$rhs:ty> for $name: ident) => {
        impl std::ops::AddAssign<$rhs> for $name {
            fn add_assign(&mut self, other: $rhs) {
                use std::ops::Add;
                *self = self.add(other);
            }
        }
    };
}

// Assumes `Sub` is already impl'd
#[macro_export]
macro_rules! sub_assign {
    (for $name: ident) => {
        impl std::ops::SubAssign for $name {
            fn sub_assign(&mut self, other: $name) {
                use std::ops::Sub;
                *self = self.sub(other);
            }
        }
    };
    (<$rhs:ty> for $name: ident) => {
        impl std::ops::SubAssign<$rhs> for $name {
            fn sub_assign(&mut self, other: $rhs) {
                use std::ops::Sub;
                *self = self.sub(other);
            }
        }
    };
}

#[macro_export]
macro_rules! ord {
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
                use std::cmp::Ord;
                let $self = self;
                let $other = other;
                $code
            }
        }
    };
}

// TODO move these traits into their own crate?
// that would require users to import that other crate unless we re-export.

/// The versions from num-traits don't allow the operands or the result to be different types.
pub trait CheckedAdd<Rhs = Self> {
    type Output;

    fn checked_add(self, rhs: Rhs) -> Option<Self::Output>;
}

pub trait CheckedSub<Rhs = Self> {
    type Output;

    fn checked_sub(self, rhs: Rhs) -> Option<Self::Output>;
}

pub trait SaturatingAdd<Rhs = Self> {
    type Output;

    fn saturating_add(self, rhs: Rhs) -> Self::Output;
}

pub trait SaturatingSub<Rhs = Self> {
    type Output;

    fn saturating_sub(self, rhs: Rhs) -> Self::Output;
}

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
macro_rules! dg {
    ($thing:expr) => {
        if cfg!(debug_assertions) {
            dbg!($thing)
        } else {
            $thing
        }
    };
}

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

// This is only slightly nicer to use than using the body of the macro directly, but
// it's nice to have all the features stuff in one place as a form of documentation.
#[macro_export]
macro_rules! invariants_checked {
    () => {{
        cfg!(feature = "invariant-checking")
    }};
}
