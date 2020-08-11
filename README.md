
run 
```
cargo test
```
to get output including the following:

```
thread 'rustc' panicked at 'no entry found for key', src/librustc_mir_build/build/mod.rs:345:9
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

error: internal compiler error: unexpected panic

note: the compiler unexpectedly panicked. this is a bug.

note: we would appreciate a bug report: https://github.com/rust-lang/rust/blob/master/CONTRIBUTING.md#bug-reports

note: rustc 1.45.2 (d3fb005a3 2020-07-31) running on x86_64-unknown-linux-gnu

note: compiler flags: -C embed-bitcode=no -C debuginfo=2 -C incremental

note: some of the compiler flags provided by cargo are hidden
```