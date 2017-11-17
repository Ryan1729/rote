extern crate libc;

use libc::{iscntrl, tcgetattr, tcsetattr, termios, ECHO, ICANON, ICRNL, IEXTEN, ISIG, IXON, OPOST,
           TCSAFLUSH};
use std::io::{self, Read};
use std::os::unix::io::AsRawFd;

// This is a reasonably nice way to have a "uninitialized/zeroed" global,
// given what is stable in Rust 1.21.0
static mut ORIG_TERMIOS: Option<termios> = None;

fn disable_raw_mode() {
    if let Some(orig_termios) = unsafe { ORIG_TERMIOS.as_mut() } {
        unsafe {
            tcsetattr(
                io::stdin().as_raw_fd(),
                TCSAFLUSH,
                orig_termios as *mut termios,
            );
        }
    }
}

fn enable_raw_mode() {
    unsafe {
        ORIG_TERMIOS = Some(std::mem::zeroed());
        if let Some(orig_termios) = ORIG_TERMIOS.as_mut() {
            let stdin_fileno = io::stdin().as_raw_fd();
            tcgetattr(stdin_fileno, orig_termios as *mut termios);

            let mut raw = *orig_termios;

            raw.c_iflag &= !(ICRNL | IXON);
            raw.c_oflag &= !(OPOST);
            raw.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);

            tcsetattr(stdin_fileno, TCSAFLUSH, &mut raw as *mut termios);
        }
    }
}

fn main() {
    enable_raw_mode();

    let mut buffer = [0; 1];
    let mut stdin = io::stdin();

    while stdin.read_exact(&mut buffer).is_ok() && buffer[0] != b'q' {
        let c = buffer[0] as char;
        if unsafe { iscntrl(c as _) } != 0 {
            println!("{}", c as u8);
        } else {
            println!("{} ('{}')", c as u8, c);
        }
    }

    disable_raw_mode();
}
