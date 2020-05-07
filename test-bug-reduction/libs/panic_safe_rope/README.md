A wrapper around [ropey's](https://github.com/cessen/ropey) [`Rope` type](https://docs.rs/ropey/1.0.1/ropey/struct.Rope.html) that checks the panic conditions at runtime and
changes the return type of some methods with the aim of preventing panics.

Licensed under MIT