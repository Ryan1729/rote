use text_pos::{CharOffset};
use macros::{
    d, fmt_debug, fmt_display, SaturatingAdd, SaturatingSub,
};

pub use panic_safe_rope::{RopeSlice, RopeSliceTrait, ByteIndex};

/// We might change this later, but it will always be an integer of some sort.
pub type SpanKindRaw = u8;

/// We want to allow different kinds of span classifiers to have 
/// different sets of span kinds, and to be able to invent new ones
/// without needing to list them all here. Additionally we want 
/// deciding what to do when presented with values of this type to 
/// be up to individual clients of the `editor` crate, while also 
/// allowing at least some form of backward compatibility. For 
/// example, a client should be allowed to conflate different
/// SpanKinds up to and including ones that were not known at the 
/// time that client was written. All that leads us to allowing all
/// values of the structs size as possible values, rather than an
/// enum where only certain values are allowed.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SpanKind(SpanKindRaw);

#[macro_export]
macro_rules! sk {
    () => {
        sk!(0)
    };
    (PLAIN) => {
        sk!(0)
    };
    (COMMENT) => {
        sk!(1)
    };
    (STRING) => {
        sk!(2)
    };
    // When adding a new one of these, increment 
    // the value below for each new one.
    (FIRST_UNASSIGNED_RAW) => {
        3
    };
    ($kind_val: expr) => {
        SpanKind::new($kind_val)
    }
}

fmt_display!(for SpanKind: k in "{}", match *k {
    SpanKind::PLAIN   => "  PLAIN".to_owned(),
    SpanKind::COMMENT => "COMMENT".to_owned(),
    SpanKind::STRING  => " STRING".to_owned(),
    _ => format!("{}", k.0),
});

impl SpanKind {
    pub const fn new(byte: u8) -> Self { 
        SpanKind(byte)
    }

    /// The justification for using all values of a given size
    /// notwithstanding, it is still useful to have clear 
    /// conventions, (which can be ignored as necessary,)
    /// hence these constants.
    pub const PLAIN: SpanKind = sk!(PLAIN);
    pub const COMMENT: SpanKind = sk!(COMMENT);
    pub const STRING: SpanKind = sk!(STRING);

    /// Given we have conventions, we want to be able to 
    /// conform with them, but also allow new conventions
    /// to be created. This value represents the smallest 
    /// value that does not have a conventional meaning.
    /// all the values of a `SpanKindRaw` will not have a
    /// conventional meaning, so different span 
    /// classifiers can assign those values whatever 
    /// meaning they wish.
    pub const FIRST_UNASSIGNED_RAW: SpanKindRaw = sk!(FIRST_UNASSIGNED_RAW);

    pub fn get_byte(&self) -> u8 {
        self.0
    }
}
d!(for SpanKind: SpanKind::PLAIN);

#[derive(Copy, Clone, Default, PartialEq)]
pub struct SpanView {
    /// The index of the byte one past the last byte that belongs to this span.
    /// We store only this index because in a list of `Spanview`s the start index
    /// for the first span is zero, and start of the each other span is simply the 
    /// value of the previous span's `one_past_end` field.
    /// See EWD831 for a further argument as to why we use this instead of the 
    /// last byte of the span.
    pub one_past_end: ByteIndex,
    /// Which kind of span this is, available here for highlighting purposes.
    pub kind: SpanKind,
}

#[macro_export]
macro_rules! sv {
    (i $index: literal $(,)? k $($tokens: tt)+) => {
        SpanView { 
            one_past_end: ByteIndex($index),
            kind: sk!($($tokens)+)
        }
    };
    (i $index: literal) => {
        SpanView { 
            one_past_end: ByteIndex($index),
            kind: sk!()
        }
    };
    (i $index: expr, k $($tokens: tt)+) => {
        SpanView { 
            one_past_end: ByteIndex($index),
            kind: sk!($($tokens)+)
        }
    }
}

fmt_debug!(for SpanView: s in "sv!(i {} k {})", s.one_past_end, s.kind);

/// This struct keeps a private ordered collection of `SpanView`s so we can ensure
/// that the spans are in ascending order.
#[derive(Clone, Default, Debug, PartialEq)]
pub struct Spans {
    spans: Vec<SpanView>,
}

/// This macro asserts that the passes `$spans`, (either a `Spans` or a 
/// `Vec<SpanView>`) satisifies the invaraints that the Spans type ensures.
#[macro_export]
macro_rules! spans_assert {
    ($spans: expr) => {
        spans_assert!($spans, "");
    };
    ($spans: expr, $suffix: expr) => {
        spans_assert!($spans, $suffix, 0);
    };
    ($spans: expr, $suffix: expr, skip kind) => {
        spans_assert!($spans, $suffix, 1);
    };
    // internal
    ($spans: expr, $suffix: expr, $mode: expr) => {
        let spans = $spans;
        let mode = $mode;

        let mut previous_kind = None;
        let mut previous_one_past_end = ByteIndex::default();
        for (i, s) in spans.clone().into_iter().enumerate() {
            assert_ne!(
                s.one_past_end,
                ByteIndex::default(),
                "the span at index {}, {:?} has an one_past_end of 0. This indicates a useless 0 length span, and so it should be removed.",
                i,
                s
            );

            match mode {
                1 => {}
                _ => {
                    if let Some(prev) = previous_kind {
                        assert_ne!(
                            s.kind,
                            prev,
                            "at index {} in spans was {:?} which has the same kind as the previous span: {:?}", 
                            i,
                            s,
                            spans,
                        );
                    }
                    previous_kind = Some(s.kind);

                    assert_ne!(
                        s.one_past_end,
                        previous_one_past_end,
                        "at index {} in spans was {:?} which has the same one_past_end as the previous span: {:?}", 
                        i,
                        s,
                        spans,
                    );
                }
            }
            
            assert!(
                previous_one_past_end <= s.one_past_end,
                "{} > {} {}\n\n{:?}",
                previous_one_past_end,
                s.one_past_end,
                $suffix,
                spans
            );
            previous_one_past_end = s.one_past_end;
        }
    }
}

impl From<Vec<SpanView>> for Spans {
    fn from(spans: Vec<SpanView>) -> Self {
        if cfg!(feature = "debug_assertions") 
        || cfg!(feature = "invariant-checking") {
            spans_assert!(&spans);
        }
        Spans { spans } 
    }
}

impl From<Spans> for Vec<SpanView> {
    fn from(spans: Spans) -> Self {
        spans.spans
    }
}

impl IntoIterator for Spans {
    type Item = SpanView;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.spans.into_iter()
    }
}

impl PartialEq<Vec<SpanView>> for Spans {
    fn eq(&self, other: &Vec<SpanView>) -> bool {
        self.spans == *other
    }
}

impl Spans {
    pub fn len(&self) -> usize {
        self.spans.len()
    }

    pub fn is_empty(&self) -> bool {
        self.spans.is_empty()
    }
}

#[derive(Debug, PartialEq)]
pub struct LabelledSlice<'text> {
    pub slice: RopeSlice<'text>,
    pub kind: SpanKind,
}

impl <'text, 'spans> Spans {
    pub fn labelled_slices(
        &'spans self,
        slice: RopeSlice<'text>
    ) -> LabelledSlices<'text, 'spans> {
        LabelledSlices {
            prev: d!(),
            slice,
            spans_iter: self.spans.iter(),
        }
    }
}

pub struct LabelledSlices<'text, 'spans> {
    prev: ByteIndex,
    slice: RopeSlice<'text>,
    spans_iter: core::slice::Iter<'spans, SpanView>,
}

impl <'text, 'spans> Iterator for LabelledSlices<'text, 'spans> {
    type Item = LabelledSlice<'text>;

    fn next(&mut self) -> Option<Self::Item> {
        self.spans_iter
            .next()
            .map(move |s| {
                let start_index = self.slice.byte_to_char(self.prev)
                    .expect("byte_to_char failed on prev");

                let end_byte_index = s.one_past_end;

                let current_slice = self.slice.slice(
                    start_index
                    .. self
                        .slice
                        .byte_to_char(end_byte_index)
                        .expect("byte_to_char failed on end")
                )
                    .expect("span_slice had incorrect index!");

                let output = if cfg!(feature = "labelled-slice-trimming") {
                    let first_non_whitespace_index = {
                        let mut chars = current_slice.chars();
            
                        let mut char_offset = CharOffset(0);
                        while let Some(true) = chars.next().map(|c| c.is_whitespace()) {
                            char_offset = char_offset.saturating_add(1);
                        }
            
                        start_index + char_offset
                    };
    
                    let last_non_whitespace_index = {
                        let mut chars = current_slice.chars_at_end();
            
                        let mut char_offset = current_slice.len_chars();
                        while let Some(true) = chars.prev().map(|c| c.is_whitespace()) {
                            char_offset = char_offset.saturating_sub(1);
                        }
            
                        start_index + char_offset
                    };
    
                    let trimmed_slice = self
                        .slice
                        .slice(first_non_whitespace_index..last_non_whitespace_index)
                        .expect("trimming slice had incorrect index!");

                    LabelledSlice {
                        slice: trimmed_slice,
                        kind: s.kind,
                    }
                } else {
                    LabelledSlice {
                        slice: current_slice,
                        kind: s.kind,
                    }
                };

                self.prev = end_byte_index;

                output
            })
    }
}