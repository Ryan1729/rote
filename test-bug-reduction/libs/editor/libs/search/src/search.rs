#![deny(unused_must_use)]
use macros::{d, SaturatingAdd, SaturatingSub};
use panic_safe_rope::{Rope, RopeSlice, RopeSliceTrait};
use editor_types::{Position, CharOffset, AbsoluteCharOffset};
use rope_pos::{char_offset_to_pos, AbsoluteCharOffsetRange};

#[derive(Clone, Debug, Default, Hash)]
pub struct SearchResults {
    pub needle: String,
    pub ranges: Vec<(Position, Position)>,
    pub current_range: usize,
}

impl SearchResults {
    #[perf_viz::record]
    pub fn new(needle: RopeSlice, haystack: &Rope) -> SearchResults {
        let ranges = get_ranges(
            needle,
            haystack,
            d!(),
            d!(),
        );

        SearchResults {
            needle: {
                perf_viz::record_guard!("SearchResults needle.into()");
                needle.into()
            },
            ranges,
            current_range: 0,
        }
    }

    #[perf_viz::record]
    pub fn refresh(&mut self, needle: RopeSlice, haystack: &Rope) {
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

// TODO benchmark with previous version used in tests
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

    use std::convert::TryInto;
    macro_rules! get_chars_at {
        ($rope: expr, $n: expr) => {{
            perf_viz::record_guard!("get_chars_at");
            
            $n.try_into()
                .ok()
                .map(AbsoluteCharOffset)
                .and_then(|offset| CharsAt::new_at(&$rope, offset))
            
        }};
    }

    macro_rules! set_chars_at_to {
        ($chars_at: expr, $n: expr) => {{
            perf_viz::record_guard!("set_chars_at_to");
            match (
                $n.try_into().map(AbsoluteCharOffset),
                $chars_at.as_mut()
            ) {
                (Ok(offset), Some(chars_at)) => {
                    chars_at.set_to(offset);
                },
                _ => {}
            }
        }};
    }

    let mut i: isize;
    let mut j: isize = 0;
    if period_matches {
        let mut memory: isize = -1;
        i = std::cmp::max(ell, memory) + 1;
        let mut n_chars = get_chars_at!(needle, i);
        let mut h_chars = get_chars_at!(haystack, i + j);

        while j <= haystack_len - needle_len {
            i = std::cmp::max(ell, memory) + 1;
            {
                set_chars_at_to!(n_chars, i);
                set_chars_at_to!(h_chars, i + j);
                while i < needle_len && opt_iters_match_once!(next: n_chars, h_chars) {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    set_chars_at_to!(n_chars, i + 1);
                    set_chars_at_to!(h_chars, i + j + 1);
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
        i = ell + 1;
        let mut n_chars = get_chars_at!(needle, i);
        let mut h_chars = get_chars_at!(haystack, i + j);
        period = std::cmp::max(ell + 1, needle_len - ell - 1) + 1;
        while j <= haystack_len - needle_len {
            i = ell + 1;
            {
                set_chars_at_to!(n_chars, i);
                set_chars_at_to!(h_chars, i + j);
                while i < needle_len && opt_iters_match_once!(next: n_chars, h_chars) {
                    i += 1;
                }
            }
            if i >= needle_len {
                i = ell;
                {
                    set_chars_at_to!(n_chars, i + 1);
                    set_chars_at_to!(h_chars, i + j + 1);
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

mod chars_at {
    use super::*;
    // A wrapper around a `panic_safe_rope::Chars` that suppports setting the iterator to a given char offset.
    pub struct CharsAt<'rope> {
        iter: panic_safe_rope::Chars<'rope>,
        offset: AbsoluteCharOffset,
    }

    impl <'rope> CharsAt<'rope> {
        #[perf_viz::record]
        pub fn new_at(rope: &'rope RopeSlice<'rope>, offset: AbsoluteCharOffset) -> Option<Self> {
            rope.chars_at(offset.into()).map(|iter| {
                CharsAt {
                    iter,
                    offset,
                }
            })
            
        }

        #[perf_viz::record]
        pub fn set_to(&mut self, offset: AbsoluteCharOffset) {
            if self.offset > offset {
                let o: AbsoluteCharOffset = self.offset.saturating_sub(offset.0);
                for _ in 0..o.0 {
                    self.prev();
                }
            } else {
                let o: AbsoluteCharOffset = offset.saturating_sub(self.offset.0);
                for _ in 0..o.0 {
                    self.next();
                }
            }

            self.offset = offset;
        }

        #[perf_viz::record]
        pub fn prev(&mut self) -> Option<char> {
            let output = self.iter.prev();

            if output.is_some() {
                self.offset = self.offset.saturating_sub(CharOffset(1));
            }

            output
        }
    }

    impl<'a> Iterator for CharsAt<'a> {
        type Item = char;
    
        #[perf_viz::record]
        fn next(&mut self) -> Option<char> {
            let output = self.iter.next();

            if output.is_some() {
                self.offset = self.offset.saturating_add(CharOffset(1));
            }

            output
        }
    
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.iter.size_hint()
        }
    }
}
use chars_at::CharsAt;

#[cfg(test)]
mod tests;