extern crate libc;

use libc::{tcgetattr, tcsetattr, termios, ECHO, TCSAFLUSH};
use std::io::{self, Read};
use std::os::unix::io::AsRawFd;

fn enable_raw_mode() {
    unsafe {
        let mut raw: termios = std::mem::uninitialized();
        let stdin_fileno = io::stdin().as_raw_fd();
        let raw_ptr = &mut raw as *mut termios;
        tcgetattr(stdin_fileno, raw_ptr);

        raw.c_lflag &= !ECHO;

        tcsetattr(stdin_fileno, TCSAFLUSH, raw_ptr);
    }
}

fn main() {
    enable_raw_mode();

    let mut buffer = [0; 1];
    let mut stdin = io::stdin();

    while stdin.read_exact(&mut buffer).is_ok() && buffer[0] != b'q' {}
}
