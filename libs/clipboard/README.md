# Modification Note

The [rust-clipboard](https://github.com/aweinstock314/rust-clipboard) has been modified to suit the needs of rote. In particular the initial copy was made from commit [07d080be58a361a5bbdb548fafe9449843d968be](https://github.com/aweinstock314/rust-clipboard/commit/07d080be58a361a5bbdb548fafe9449843d968be).

# rust-clipboard

rust-clipboard is a cross-platform library for getting and setting the contents of the OS-level clipboard.  
It has been tested on Windows, Mac OSX, GNU/Linux, and FreeBSD.
It is used in Mozilla Servo.

[![](http://meritbadge.herokuapp.com/clipboard)](https://crates.io/crates/clipboard)

## Prerequisites

On Linux you need the x11 library, install it with something like:

```bash
sudo apt-get install xorg-dev
```

## Example

```rust
extern crate clipboard;

use clipboard::ClipboardProvider;
use clipboard::ClipboardContext;

fn example() {
    let mut ctx: ClipboardContext = ClipboardProvider::new().unwrap();
    println!("{:?}", ctx.get_contents());
    ctx.set_contents("some string".to_owned()).unwrap();
}
```

## API

The `ClipboardProvider` trait has the following functions:

```rust
fn new() -> Result<Self, Box<Error>>;
fn get_contents(&mut self) -> Result<String, Box<Error>>;
fn set_contents(&mut self, String) -> Result<(), Box<Error>>;
```

`ClipboardContext` is a type alias for one of {`WindowsClipboardContext`, `OSXClipboardContext`, `X11ClipboardContext`, `NopClipboardContext`}, all of which implement `ClipboardProvider`. Which concrete type is chosen for `ClipboardContext` depends on the OS (via conditional compilation).

## License

`rust-clipboard` is dual-licensed under MIT and Apache2.
