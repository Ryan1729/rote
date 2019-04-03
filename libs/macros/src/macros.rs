#[macro_export]
macro_rules! usize_newtype {
    ($name: ident) => {
        impl std::ops::Add<usize> for $name {
            type Output = $name;

            fn add(self, other: usize) -> $name {
                $name(self.0 + other)
            }
        }

        impl std::ops::AddAssign<usize> for $name {
            fn add_assign(&mut self, other: usize) {
                *self = self.add(other);
            }
        }

        impl std::ops::Sub<usize> for $name {
            type Output = $name;

            fn sub(self, other: usize) -> $name {
                //Should this be saturating?
                $name(self.0 - other)
            }
        }

        impl std::ops::SubAssign<usize> for $name {
            fn sub_assign(&mut self, other: usize) {
                *self = self.sub(other);
            }
        }

        impl std::ops::Sub<$name> for usize {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                //Should this be saturating?
                $name(self - other.0)
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
macro_rules! number_newtype {
    ($name: ident) => {
        impl std::ops::Add for $name {
            type Output = $name;

            fn add(self, other: $name) -> $name {
                $name(self.0 + other.0)
            }
        }
        impl std::ops::Sub for $name {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                $name(self.0 - other.0)
            }
        }
    };
}

#[macro_export]
macro_rules! d {
    () => {
        Default::default()
    };
    (for $name:ty : $code:expr) => {
        impl Default for $name {
            fn default() -> Self {
                $code
            }
        }
    };
}

#[macro_export]
macro_rules! display {
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
macro_rules! dg {
    ($thing:expr) => {
        if cfg!(debug_assertions) {
            dbg!($thing)
        } else {
            $thing
        }
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
