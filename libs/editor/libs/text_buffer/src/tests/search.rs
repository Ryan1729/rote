use super::*;

fn get_search_ranges_works_on(needle: &str, haystack: &str) {
    // precondition
    assert!(needle.len() > 0);

    let needle = t_b!(needle);
    let haystack = t_b!(haystack);

    let ranges = get_search_ranges(needle.rope.full_slice(), &haystack.rope, None, None);

    assert!(ranges.len() > 0);

    let mut chars = haystack.chars();
    let mut previously_used = AbsoluteCharOffset(0);
    for (start, end) in ranges {
        let start_offset = pos_to_char_offset(&haystack.rope, &start).unwrap();
        let end_offset = pos_to_char_offset(&haystack.rope, &end).unwrap();

        for _ in previously_used.0..start_offset.0 {
            chars.next().unwrap();
        }

        let mut n_chars = needle.chars();
        for _ in start_offset.0..end_offset.0 {
            super::assert_eq!(chars.next(), n_chars.next());
        }

        previously_used = end_offset;
    }
}

proptest! {
    #[test]
    fn get_search_ranges_works(
        (needle, haystack) in arb::needle_and_haystack()
    ) {
        get_search_ranges_works_on(&needle, &haystack);
    }
}

proptest! {
    #[test]
    fn get_search_ranges_does_not_make_stuff_up(
        needle in "[0-9]+",
        needleless_haystack in "[a-z]+"
    ) {
        let needle = t_b!(needle);
        let needleless_haystack = t_b!(needleless_haystack);
        super::assert_eq!(get_search_ranges(
            needle.rope.full_slice(),
            &needleless_haystack.rope,
            None,
            None
        ).len(), 0);
    }
}

#[test]
fn get_search_ranges_works_on_this_identical_needle_and_haystack() {
    get_search_ranges_works_on("0 ", "0 ");
}

#[test]
fn get_search_ranges_works_on_this_generated_example() {
    get_search_ranges_works_on("0 ", "0 A");
}

#[test]
fn get_search_ranges_works_on_this_second_generated_example() {
    get_search_ranges_works_on("0 ", "A0 ");
}

#[test]
fn get_search_ranges_works_on_this_late_found_generated_example() {
    get_search_ranges_works_on("0¡", "::0¡");
}

#[test]
fn get_search_ranges_works_on_this_mutation_of_the_late_found_generated_example() {
    get_search_ranges_works_on("0A", "::0A");
}

proptest! {
    #[test]
    fn get_search_ranges_impl_matches_refernce_impl(
        (needle, haystack) in arb::needle_and_haystack()
    ) {
        let needle = r!(needle);
        let haystack = r!(haystack);
        super::assert_eq!(
            get_search_ranges_impl(needle.full_slice(), haystack.full_slice(), None),
            get_search_ranges_reference_impl(needle.full_slice(), haystack.full_slice(), None)
        );
    }
}

/// This version is slow (`skip_manually` in particular) but has previously passed the `get_search_ranges_works` test.
fn get_search_ranges_reference_impl(
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

    macro_rules! opts_match {
        ($op1: expr, $op2: expr, $match_case: block else $else_case: block) => {
            match ($op1, $op2) {
                (Some(e1), Some(e2)) if e1 == e2 => $match_case,
                (None, None) => $match_case,
                _ => $else_case,
            }
        };
    }

    /* Searching */
    let period_matches = {
        let mut start_chars = needle.chars();
        let mut period_chars = needle.chars().skip(period as usize);
        let mut matches = true;
        for _ in 0..(ell + 1) {
            opts_match!(dbg!(start_chars.next()), dbg!(period_chars.next()), {} else {
                matches = false;
                break;
            });
        }
        matches
    };

    // this avoids trapping the ropey `Chars` in a `Skip` struct, so we can still call `prev`.
    macro_rules! skip_manually {
        ($iter: expr, $n: expr) => {{
            let mut iter = $iter;
            for _ in 0..($n as usize) {
                if iter.next().is_none() {
                    break;
                }
            }
            iter
        }};
    }

    let mut i: isize;
    let mut j: isize = 0;
    if period_matches {
        let mut memory: isize = -1;
        while j <= haystack_len - needle_len {
            i = std::cmp::max(ell, memory) + 1;
            {
                let mut n_chars = skip_manually!(needle.chars(), i);
                let mut h_chars = skip_manually!(haystack.chars(), i + j);
                while i < needle_len
                    && opts_match!(n_chars.next(), h_chars.next(), {true} else {false})
                {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    let mut n_chars = skip_manually!(needle.chars(), needle_len - i + 1);
                    let mut h_chars = skip_manually!(haystack.chars(), haystack_len - (i + j) + 1);
                    while i > memory
                        && opts_match!(dbg!(n_chars.prev()), dbg!(h_chars.prev()), {true} else {false})
                    {
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
                let mut n_chars = skip_manually!(needle.chars(), i);
                let mut h_chars = skip_manually!(haystack.chars(), i + j);
                while i < needle_len
                    && opts_match!(n_chars.next(), h_chars.next(), {true} else {false})
                {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    let mut n_chars = skip_manually!(needle.chars(), i + 1);
                    let mut h_chars = skip_manually!(haystack.chars(), i + j + 1);

                    while i >= 0 && opts_match!(n_chars.prev(), h_chars.prev(), {true} else {false})
                    {
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
