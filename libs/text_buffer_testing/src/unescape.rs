use std::borrow::Cow;

pub fn str(input: &str) -> Cow<str> {
    #[derive(Clone, Copy)]
    enum State {
        NotEscaped,
        Escaped,
    }
    use State::*;

    let mut state = State::NotEscaped;
    let mut string = None;
    let mut iter = input.char_indices();
    while let Some((i, ch)) = iter.next() {
        state = match (state, ch, string.as_mut()) {
            (NotEscaped, '\\', None) => {
                let mut s = String::with_capacity(input.len());
                s.push_str(&input[0..i]);
                string = Some(s);

                Escaped
            },
            (NotEscaped, _, None) => NotEscaped,
            (NotEscaped, '\\', Some(_)) => Escaped,
            (NotEscaped, _, Some(s)) => {
                s.push(ch);
                NotEscaped
            },
            (Escaped, '\\', Some(s)) => {
                s.push('\\');
                NotEscaped
            },
            (Escaped, 't', Some(s)) => {
                s.push('\t');
                NotEscaped
            },
            (Escaped, 'n', Some(s)) => {
                s.push('\n');
                NotEscaped
            },
            (Escaped, 'r', Some(s)) => {
                s.push('\r');
                NotEscaped
            },
            (Escaped, 'u', Some(s)) => {
                let mut number = 0;
                while let Some((_, c)) = iter.next() {
                    macro_rules! accum_digit {
                        ($digit: literal) => {{
                            number <<= 4;
                            number |= $digit;
                        }}
                    }

                    match c {
                        '{' => {}
                        '0' => accum_digit!(0),
                        '1' => accum_digit!(1),
                        '2' => accum_digit!(2),
                        '3' => accum_digit!(3),
                        '4' => accum_digit!(4),
                        '5' => accum_digit!(5),
                        '6' => accum_digit!(6),
                        '7' => accum_digit!(7),
                        '8' => accum_digit!(8),
                        '9' => accum_digit!(9),
                        'A'|'a' => accum_digit!(0xA),
                        'B'|'b' => accum_digit!(0xB),
                        'C'|'c' => accum_digit!(0xC),
                        'D'|'d' => accum_digit!(0xD),
                        'E'|'e' => accum_digit!(0xE),
                        'F'|'f' => accum_digit!(0xF),
                        '}' => {
                            s.push(
                                core::char::from_u32(number)
                                    .unwrap_or(core::char::REPLACEMENT_CHARACTER)
                            );
                            break;
                        }
                        _ => {
                            s.push(core::char::REPLACEMENT_CHARACTER);
                            break;
                        }
                    }
                }
                NotEscaped
            },
            (Escaped, _, Some(s)) => {
                s.push('\\');
                s.push(ch);
                NotEscaped
            },
            (Escaped, _, None) => unreachable!(),
        };
    }

    match string {
        Some(s) => Cow::Owned(s),
        None => Cow::Borrowed(input),
    }
    
}

#[test]
fn str_works_on_these_examples() {
    assert_eq!(str("").as_ref(), "");

    assert_eq!(str(r"\n").as_ref(), "\n");

    assert_eq!(str(r"\t").as_ref(), "\t");

    assert_eq!(str(r"\r").as_ref(), "\r");

    assert_eq!(str(r"\u{2029}").as_ref(), "\u{2029}");

    assert_eq!(str(r"\\").as_ref(), "\\");

    // All together now!
    assert_eq!(str(r"\n\t\r\u{2029}\\").as_ref(), "\n\t\r\u{2029}\\");
}