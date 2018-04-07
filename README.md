`cargo` 5310 repro branch
=========================

reproduction test case for the behavior I mentioned in cargo issue 5310,
which might be totally normal, or it might be a bug in rustdoc or rustc or
who the hell knows

```
git clone https://github.com/ExpHP/unbox-macro -b minimize-cargo-5310
cd unbox-macro
cargo doc
```
