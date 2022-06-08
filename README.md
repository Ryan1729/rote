# rote - Ryan's Own Text Editor

# Designed for Me
If you want to try using it, go ahead, but I am not promising any form of support. 

This is a text editor, designed for my own use. It's at the point that I can and do use it to edit its own source code, but there's loads more to do yet.

![screenshot](/screenshot.png?raw=true "image of a text editor with syntax highlighting of some weird rust code")
([The link in the pic so you don't need to type it](https://github.com/rust-lang/rust/blob/master/src/test/ui/weird-exprs.rs))

## Plan
* implement everything in [TODO.md](./design/TODO.md) and [MVP-FEATURES.md](./design/MVP-FEATURES.md).
* Think of more useful features?

____

## Building

```
cargo build --release
``` 

Tested on Ubuntu Linux, and Microsoft Windows. We aim for portability, so it might work elsewhere.

### Known build issues

```
  running: "cc" "-O3" "-ffunction-sections" "-fdata-sections" "-fPIC" "-m64" "-I" "nativefiledialog/src/include" "-Wall" "-Wextra" "-o" "/home/ryan/code/rust/rote/target/release/build/nfd-0327472a67a84b11/out/nativefiledialog/src/nfd_gtk.o" "-c" "nativefiledialog/src/nfd_gtk.c"
  cargo:warning=nativefiledialog/src/nfd_gtk.c:10:10: fatal error: gtk/gtk.h: No such file or directory
  cargo:warning=   10 | #include <gtk/gtk.h>
  cargo:warning=      |          ^~~~~~~~~~~
  cargo:warning=compilation terminated.
  exit status: 1
```

or

```
  = note: /usr/bin/ld: cannot find -lxcb-shape
          /usr/bin/ld: cannot find -lxcb-xfixes
          collect2: error: ld returned 1 exit status
```

We use `nativefiledialog` which on linux relies on some the development versions of libraries which are not always installed.

On `Ubuntu` based Linux distros these can be installed with:

```
sudo apt-get install libgtk-3-dev libxcb-composite0-dev
```

____


Licensed under MIT and Apache 2.0
The OpenGL platform layer is licensed under Apache 2.0
