use macros::{d};
use panic_safe_rope::{Rope, RopeSlice, RopeSliceTrait};
use editor_types::{Position, CharOffset, AbsoluteCharOffset};
use rope_pos::{char_offset_to_pos, AbsoluteCharOffsetRange};

#[derive(Clone, Debug, Default)]
pub struct SearchResults {
    pub needle: String,
    pub ranges: Vec<(Position, Position)>,
    pub current_range: usize,
}

impl SearchResults {
    pub fn new(needle: RopeSlice, haystack: &Rope) -> SearchResults {
        let ranges = get_ranges(
            needle,
            haystack,
            d!(),
            d!(),
        );

        SearchResults {
            needle: needle.into(),
            ranges,
            current_range: 0,
        }
    }


    pub fn refresh(&mut self, needle: RopeSlice, haystack: &Rope) {
        perf_viz::record_guard!("SearchResults::refresh");
        
        let mut new = Self::new(needle, haystack);

        let old_range = self.ranges.get(self.current_range);
        let overlapping_range = old_range.map(|(old_start, old_end)| {
            let mut overlapping_range = None;
            for (new_start, new_end) in new.ranges.iter() {
                if (old_start <= new_start && new_start <= old_end)
                || (old_start <= new_end && new_end <= old_end) {
                    overlapping_range = Some((new_start, new_end));
                    break;
                }
            }

            overlapping_range
        });

        // We want to keep the current place whenever it would still be accurate.
        if overlapping_range.is_some() {
            new.current_range = self.current_range
        }

        *self = new
    }
}

/// A `haystack_range` of `None` means use the whole haystack. AKA no limit.
/// A `max_needed` of `None` means return all the results. AKA no limit.
pub fn get_ranges(
    needle: RopeSlice,
    haystack: &Rope,
    haystack_range: Option<AbsoluteCharOffsetRange>,
    max_needed: Option<std::num::NonZeroUsize>,
) -> Vec<(Position, Position)> {
    perf_viz::record_guard!("get_ranges");

    // This turns out to be a rather large optimization on large haystacks.
    // TODO Understand why.
    if needle.len_bytes() == 0 {
        return d!();
    }

    let (min, slice) = haystack_range
        .and_then(|r| haystack.slice(r.range()).map(|s| (r.min().0, s)))
        .unwrap_or_else(|| (0, haystack.full_slice()));

    

    let offsets = get_ranges_impl(needle, slice, max_needed);

    perf_viz::start_record!("offsets.into_iter() char_offset_to_pos");
    let output = offsets
        .into_iter()
        .filter_map(|(o_min, o_max)| {
            let a_o_min = AbsoluteCharOffset(o_min.0 + min);
            let a_o_max = AbsoluteCharOffset(o_max.0 + min);

            char_offset_to_pos(haystack, a_o_min)
                .and_then(|p_min| char_offset_to_pos(haystack, a_o_max).map(|p_max| (p_min, p_max)))
        })
        .collect();
    perf_viz::end_record!("offsets.into_iter() char_offset_to_pos");
    output
}

fn get_ranges_impl(
    needle: RopeSlice,
    haystack: RopeSlice,
    max_needed: Option<std::num::NonZeroUsize>,
) -> Vec<(CharOffset, CharOffset)> {
    perf_viz::record_guard!("get_ranges_impl");
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
        ($method: ident : $op_iter1: expr, $op_iter2: expr) => {
            match (&mut $op_iter1, &mut $op_iter2) {
                (Some(ref mut i1), Some(ref mut i2)) => match (i1.$method(), i2.$method()) {
                    (Some(e1), Some(e2)) if e1 == e2 => true,
                    (None, None) => true,
                    _ => false,
                },
                (None, None) => true,
                _ => false,
            }
        };
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

#[cfg(test)]
mod tests;