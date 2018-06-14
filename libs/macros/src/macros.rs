#[macro_export]
macro_rules! add_assign {
    (for $name: ident) => {
        impl std::ops::AddAssign for $name {
            fn add_assign(&mut self, other: $name) {
                *self = self.add(other);
            }
        }
    };
    (<$rhs:ty> for $name: ident) => {
        impl std::ops::AddAssign<$rhs> for $name {
            fn add_assign(&mut self, other: $rhs) {
                *self = self.add(other);
            }
        }
    };
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

        //We write this out so you don't need to import `add_assign!` to use this macro
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
macro_rules! integer_newtype {
    ($name: ident) => {
        impl std::ops::Add for $name {
            type Output = $name;

            fn add(self, other: $name) -> $name {
                $name(self.0 + other.0)
            }
        }

        impl std::ops::AddAssign for $name {
            fn add_assign(&mut self, other: $name) {
                *self = self.add(other);
            }
        }

        impl std::ops::Sub for $name {
            type Output = $name;

            fn sub(self, other: $name) -> $name {
                $name(self.0 - other.0)
            }
        }

        impl std::ops::SubAssign for $name {
            fn sub_assign(&mut self, other: $name) {
                *self = self.sub(other);
            }
        }

        impl PartialOrd for $name {
            fn partial_cmp(&self, other: &$name) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $name {
            fn cmp(&self, other: &$name) -> std::cmp::Ordering {
                self.0.cmp(&other.0)
            }
        }

        impl PartialEq for $name {
            fn eq(&self, other: &$name) -> bool {
                self.0 == (*other).0
            }
        }

        impl Eq for $name {}
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
