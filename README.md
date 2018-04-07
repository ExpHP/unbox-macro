`cargo` 5310 repro branch
=========================

reproduction test case for the behavior I mentioned in [cargo issue 5310](https://github.com/rust-lang/cargo/issues/5310),
which might be totally normal, or it might be a bug in rustdoc or rustc or
who the hell knows

```
git clone https://github.com/ExpHP/unbox-macro -b minimize-cargo-5310
cd unbox-macro
cargo doc
```

**On nightly 48fa6f963 (2018-04-05):**

```
$ cargo doc 
 Documenting unbox-impl v0.1.0 (file:///home/lampam/cpp/throwaway/unbox-macro/impl)
   Compiling unbox-impl v0.1.0 (file:///home/lampam/cpp/throwaway/unbox-macro/impl)
 Documenting unbox v0.3.0 (file:///home/lampam/cpp/throwaway/unbox-macro)
thread '<unnamed>' panicked at 'librustc_metadata/creader.rs:514: proc-macro crate not dylib', librustc/session/mod.rs:1320:26
note: Run with `RUST_BACKTRACE=1` for a backtrace.
error: Could not document `unbox`.

Caused by:
  process didn't exit successfully: `rustdoc --crate-name unbox lib.rs -o /home/lampam/cpp/throwaway/unbox-macro/target/doc -L dependency=/home/lampam/cpp/throwaway/unbox-macro/target/debug/deps --extern unbox_impl=/home/lampam/cpp/throwaway/unbox-macro/target/debug/deps/libunbox_impl-c547cc1942118cc3.rmeta` (exit code: 101)
```

**On stable 1.25:**

```
$ cargo doc
   Compiling unbox-impl v0.1.0 (file:///home/lampam/cpp/throwaway/unbox-macro/impl)
 Documenting unbox-impl v0.1.0 (file:///home/lampam/cpp/throwaway/unbox-macro/impl)
 Documenting unbox v0.3.0 (file:///home/lampam/cpp/throwaway/unbox-macro)
thread 'rustc' panicked at 'librustc_metadata/creader.rs:526: proc-macro crate not dylib', librustc/session/mod.rs:1180:26
note: Run with `RUST_BACKTRACE=1` for a backtrace.

error: internal compiler error: unexpected panic

note: the compiler unexpectedly panicked. this is a bug.

note: we would appreciate a bug report: https://github.com/rust-lang/rust/blob/master/CONTRIBUTING.md#bug-reports

note: rustc 1.25.0 (84203cac6 2018-03-25) running on x86_64-unknown-linux-gnu

error: Could not document `unbox`.

Caused by:
  process didn't exit successfully: `rustdoc --crate-name unbox lib.rs -o /home/lampam/cpp/throwaway/unbox-macro/target/doc -L dependency=/home/lampam/cpp/throwaway/unbox-macro/target/debug/deps --extern unbox_impl=/home/lampam/cpp/throwaway/unbox-macro/target/debug/deps/libunbox_impl-2689fc645b84b13c.rmeta` (exit code: 101)
```
