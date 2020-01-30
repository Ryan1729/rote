use macros::d;
use platform_types::{SpanView, SpanKind};

#[derive(Clone, Copy, Debug)]
pub enum ParserKind {
    Plaintext,
    Rust
}
d!(for ParserKind: ParserKind::Plaintext);

#[derive(Debug)]
pub struct Parsers {

}

d!(for Parsers: Parsers{

});

impl Parsers {
    pub fn get_spans(&self, s: &str, kind: ParserKind) -> Vec<SpanView> {
        use ParserKind::*;
        use SpanKind::*;

        match kind {
            Plaintext => {
                vec![SpanView { kind: Plain, end_byte_index: s.len()}]
            }
            Rust => {
                // TODO integrate tree-sitter
                s.char_indices().map(|(i, _)| {
                    SpanView {
                        kind: match (i + 2) % 3 {
                            1 => Comment,
                            2 => String,
                            _ => Plain,
                        },
                        end_byte_index: i
                    }
                }).collect()
            }
        }
    }
}


