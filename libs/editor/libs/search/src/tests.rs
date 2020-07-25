use super::*;

use proptest::proptest;

mod arb {
    use proptest::prelude::{Just, prop_compose};

    prop_compose! {
        pub fn needle_and_haystack()
        (needle in "\\PC+", needleless_haystack in "\\PC*")
        (insert_point in 0..=needleless_haystack.chars().count(), needle in Just(needle), needleless_haystack in Just(needleless_haystack)) -> (String, String) {
            let mut haystack = String::with_capacity(needleless_haystack.len() + needle.len());
    
            let mut inserted_count = 0;
            for (i, c) in needleless_haystack.chars().enumerate() {
                if i == insert_point {
                    inserted_count += 1;
                    haystack.push_str(&needle);
                }
                haystack.push(c);
            }
            if needleless_haystack.chars().count() == insert_point {
                inserted_count += 1;
                haystack.push_str(&needle);
            }
            assert_eq!(inserted_count, 1,
                "insert_point {}, needle {:?}, needleless_haystack: {:?}, needleless_haystack.chars().count() {}",
                insert_point,
                needle,
                needleless_haystack,
                needleless_haystack.chars().count()
            );
    
            (needle, haystack)
        }
    }
}

type GetRangesImpl = fn(RopeSlice, RopeSlice, Option<std::num::NonZeroUsize>) -> Vec<(CharOffset, CharOffset)>;

fn given_get_ranges_returns_only_locations_that_match_the_needle_on(
    selected_impl: GetRangesImpl,
    needle: &str,
    haystack: &str
) {
    // precondition
    assert!(needle.len() > 0);

    let needle: Rope = needle.into();
    let haystack: Rope = haystack.into();

    let ranges = selected_impl(needle.full_slice(), haystack.full_slice(), None);

    assert!(ranges.len() > 0);

    let mut chars = haystack.chars();
    let mut previously_used = CharOffset(0);
    for (start, end) in ranges {
        for _ in previously_used.0..start.0 {
            chars.next().unwrap();
        }

        let mut n_chars = needle.chars();
        for _ in start.0..end.0 {
            assert_eq!(chars.next(), n_chars.next());
        }

        previously_used = end;
    }
}

fn get_ranges_returns_only_locations_that_match_the_needle_on(needle: &str, haystack: &str) {
    given_get_ranges_returns_only_locations_that_match_the_needle_on(get_ranges_impl, needle, haystack);
}

proptest! {
    #[test]
    fn get_ranges_returns_only_locations_that_match_the_needle(
        (needle, haystack) in arb::needle_and_haystack()
    ) {
        get_ranges_returns_only_locations_that_match_the_needle_on(&needle, &haystack);
    }
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_identical_needle_and_haystack() {
    get_ranges_returns_only_locations_that_match_the_needle_on("0 ", "0 ");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("0 ", "0 A");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_second_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("0 ", "A0 ");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_late_found_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("0¡", "::0¡");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_reduction_of_the_late_found_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("0A", "::0A");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_second_identical_needle_and_haystack() {
    get_ranges_returns_only_locations_that_match_the_needle_on("\"#\"", "\"#\"");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_reduction_of_the_second_identical_needle_and_haystack() {
    get_ranges_returns_only_locations_that_match_the_needle_on("123", "123");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_fffd_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("\"#\"", "�\"#\"");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_reduction_of_the_fffd_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("121", "A121");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_registered_trademark_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("\"#\"", "®�\"#\"");
}

#[test]
fn get_ranges_returns_only_locations_that_match_the_needle_on_this_reduction_of_the_registered_trademark_generated_example() {
    get_ranges_returns_only_locations_that_match_the_needle_on("121", "34121");
}

proptest! {
    #[test]
    fn get_ranges_does_not_make_stuff_up(
        needle in "[0-9]+",
        needleless_haystack in "[a-z]+"
    ) {
        let needle: Rope = needle.into();
        let needleless_haystack: Rope = needleless_haystack.into();
        assert_eq!(get_ranges(
            needle.full_slice(),
            &needleless_haystack,
            None,
            None
        ).len(), 0);
    }
}

proptest! {
    #[test]
    fn get_ranges_impl_matches_previous_impl(
        (needle, haystack) in arb::needle_and_haystack()
    ) {
        let needle: Rope = needle.into();
        let haystack: Rope = haystack.into();
        assert_eq!(
            get_ranges_impl(needle.full_slice(), haystack.full_slice(), None),
            get_ranges_previous_impl(needle.full_slice(), haystack.full_slice(), None)
        );
    }
}

/// This version is slow but has previously passed the `get_ranges_returns_only_locations_that_match_the_needle` test.
fn get_ranges_previous_impl(
    needle: RopeSlice,
    haystack: RopeSlice,
    max_needed: Option<std::num::NonZeroUsize>,
) -> Vec<(CharOffset, CharOffset)> {
    // Two-way string matching based on http://www-igm.univ-mlv.fr/~lecroq/string/node26.html

    macro_rules! max_suffix {
        ($name: ident, >) => {
            max_suffix!($name, a, b, a > b)
        };
        ($name: ident, <) => {
            max_suffix!($name, a, b, a < b)
        };
        ($name: ident, $a: ident, $b: ident, $test: expr) => {
            fn $name(rope: &RopeSlice) -> Option<(isize, isize)> {
                perf_viz::record_guard!(stringify!($name));
                let len = rope.len_chars().0 as isize;
                let mut ms: isize = -1;
                let mut j: isize = 0;
                let mut p: isize = 1;
                let mut k: isize = p;

                while j + k < len {
                    let $a = rope.char(CharOffset((j + k) as usize))?;
                    let $b = rope.char(CharOffset((ms + k) as usize))?;
                    if $test {
                        j += k;
                        k = 1;
                        p = j - ms;
                    } else {
                        if $a == $b {
                            if k != p {
                                k += 1;
                            } else {
                                j += p;
                                k = 1;
                            }
                        } else {
                            ms = j;
                            j = ms + 1;
                            p = 1;
                            k = p;
                        }
                    }
                }

                Some((ms, p))
            }
        };
    }

    /* Computing of the maximal suffix for <= */
    max_suffix!(max_suffix_le, <);
    /* Computing of the maximal suffix for >= */
    max_suffix!(max_suffix_ge, >);

    let mut output = Vec::new();
    let needle_len = needle.len_chars().0 as isize;
    let haystack_len = haystack.len_chars().0 as isize;
    macro_rules! push {
        ($start_offset: expr) => {
            let start_offset = $start_offset as usize;
            let end_offset = start_offset + needle_len as usize;

            output.push((CharOffset(start_offset), CharOffset(end_offset)));
            if let Some(max_needed) = max_needed {
                if max_needed.get() >= output.len() {
                    return output;
                }
            }
        };
    }

    /* Two Way string matching algorithm. */
    //void TW(char *x, int m, char *y, int n) {
    let ell;
    let mut period;
    {
        /* Preprocessing */
        match (max_suffix_le(&needle), max_suffix_ge(&needle)) {
            (Some((i, p)), Some((j, q))) => {
                if i > j {
                    ell = i;
                    period = p;
                } else {
                    ell = j;
                    period = q;
                }
            }
            (le, ge) => {
                if cfg!(debug_assertions) {
                    panic!("le {:?}, ge {:?}", le, ge);
                } else {
                    return output;
                }
            }
        }
    }

    macro_rules! opt_iters_match_once {
        ($method: ident : $op_iter1: expr, $op_iter2: expr) => {{
            perf_viz::record_guard!("opt_iters_match_once");
            match (&mut $op_iter1, &mut $op_iter2) {
                (Some(ref mut i1), Some(ref mut i2)) => match (i1.$method(), i2.$method()) {
                    (Some(e1), Some(e2)) if e1 == e2 => true,
                    (None, None) => true,
                    _ => false,
                },
                (None, None) => true,
                _ => false,
            }
        }};
    }

    /* Searching */
    let period_matches = {
        let mut start_chars = needle.chars();
        let mut period_chars = needle.chars().skip(period as usize);
        let mut matches = true;
        for _ in 0..(ell + 1) {
            match (start_chars.next(), period_chars.next()) {
                (Some(e1), Some(e2)) if e1 == e2 => {}
                (None, None) => {}
                _ => {
                    matches = false;
                    break;
                }
            }
        }
        matches
    };

    // this avoids trapping the ropey `Chars` in a `Skip` struct, so we can still call `prev`.
    macro_rules! get_chars_at {
        ($rope: expr, $n: expr) => {{
            perf_viz::record_guard!("get_chars_at");
            use std::convert::TryInto;
            $n.try_into()
                .ok()
                .map(CharOffset)
                .and_then(|offset| $rope.chars_at(offset))
        }};
    }

    let mut i: isize;
    let mut j: isize = 0;
    if period_matches {
        let mut memory: isize = -1;
        while j <= haystack_len - needle_len {
            i = std::cmp::max(ell, memory) + 1;
            {
                let mut n_chars = get_chars_at!(needle, i);
                let mut h_chars = get_chars_at!(haystack, i + j);
                while i < needle_len && opt_iters_match_once!(next: n_chars, h_chars) {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    let mut n_chars = get_chars_at!(needle, i + 1);
                    let mut h_chars = get_chars_at!(haystack, i + j + 1);
                    while i > memory && opt_iters_match_once!(prev: n_chars, h_chars) {
                        i -= 1;
                    }
                }
                if i <= memory {
                    push!(j);
                }
                j += period;
                memory = needle_len - period - 1;
            } else {
                j += i - ell;
                memory = -1;
            }
        }
    } else {
        perf_viz::record_guard!("!period_matches");
        period = std::cmp::max(ell + 1, needle_len - ell - 1) + 1;
        while j <= haystack_len - needle_len {
            i = ell + 1;
            {
                let mut n_chars = get_chars_at!(needle, i);
                let mut h_chars = get_chars_at!(haystack, i + j);
                while i < needle_len && opt_iters_match_once!(next: n_chars, h_chars) {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    let mut n_chars = get_chars_at!(needle, i + 1);
                    let mut h_chars = get_chars_at!(haystack, i + j + 1);

                    while i >= 0 && opt_iters_match_once!(prev: n_chars, h_chars) {
                        i -= 1;
                    }
                }
                if i < 0 {
                    push!(j);
                }
                j += period;
            } else {
                j += i - ell;
            }
        }
    }

    output
}

fn get_ranges_impl_matches_the_slow_impl_on(
    needle: &str,
    haystack: &str
) {
    let needle: Rope = needle.into();
    let haystack: Rope = haystack.into();
    assert_eq!(
        get_ranges_impl(needle.full_slice(), haystack.full_slice(), None),
        get_ranges_slow_impl(needle.full_slice(), haystack.full_slice(), None)
    );
}

proptest! {
    #[test]
    fn get_ranges_impl_matches_the_slow_impl(
        (needle, haystack) in arb::needle_and_haystack()
    ) {
        get_ranges_impl_matches_the_slow_impl_on(&needle, &haystack);
    }
}

#[test]
fn get_ranges_impl_matches_the_slow_impl_in_this_generated_case() {
    get_ranges_impl_matches_the_slow_impl_on("a", "ab");
}

/// This version is meant to be obviously correct, but willing to be slow to maintain that property.
fn get_ranges_slow_impl(
    needle: RopeSlice,
    haystack: RopeSlice,
    max_needed: Option<std::num::NonZeroUsize>,
) -> Vec<(CharOffset, CharOffset)> {
    let mut output = Vec::with_capacity(16);

    for i in 0..haystack.len_chars().0 {
        let mut needle_chars = needle.chars();
        let mut haystack_chars = haystack.chars_at(CharOffset(i)).unwrap();

        // zip would end early and we don't want that.
        loop { 
            match dbg!(needle_chars.next(), haystack_chars.next()) {
                (None, _) => { 
                    output.push((CharOffset(i), CharOffset(i + needle.len_chars().0)));
                    break;
                }
                (first, second) => if first != second {
                    break;
                }
            }
        }
    }

    if let Some(max_needed) = max_needed {
        output.truncate(max_needed.get());
    }

    output
}

fn get_ranges_slow_returns_only_locations_that_match_the_needle_on(needle: &str, haystack: &str) {
    given_get_ranges_returns_only_locations_that_match_the_needle_on(get_ranges_slow_impl, needle, haystack);
}

proptest! {
    #[test]
    fn get_ranges_slow_returns_only_locations_that_match_the_needle(
        (needle, haystack) in arb::needle_and_haystack()
    ) {
        get_ranges_slow_returns_only_locations_that_match_the_needle_on(&needle, &haystack);
    }
}

#[test]
fn get_ranges_slow_returns_only_locations_that_match_the_needle_in_this_generated_unicode_case() {
    let (needle, haystack) = ("ȺȺ", "{�=𐆠𑤐�gqš\'=.¥⿵â🯳ȺȺȺ𝒪𑖮�𐠼෴=ῥ");
    get_ranges_slow_returns_only_locations_that_match_the_needle_on(&needle, &haystack);
}

