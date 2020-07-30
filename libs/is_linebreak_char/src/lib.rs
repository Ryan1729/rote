pub fn is_linebreak_char(c: char) -> bool {
    // ropey treats these as line breaks, so we do too.
    // See also https://www.unicode.org/reports/tr14/tr14-32.html
    (c >= '\u{a}' && c <= '\r')
        || c == '\u{0085}'
        || c == '\u{2028}'
        || c == '\u{2029}'
}
