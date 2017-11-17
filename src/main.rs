use std::io::{self, Read};

fn main() {
    let mut buffer = [0; 1];
    let mut stdin = io::stdin();

    while stdin.read_exact(&mut buffer).is_ok() {}
}
