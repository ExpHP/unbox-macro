
# Design concerns/constraints

* (X-PROC-NO-MULTI) `proc-macro-hack` doesn't support multiple calls to an item macro
* (X-PROC-ERR-SPAN) `proc-macro-hack` can't do errors with spans; all you can do is panic, producing a span with the entire invocation.
* (X-CONFIG) it should be possible to configure trait paths and method names


# Design axes

* (A-MULTIPLE-FNS) encouraging multiple unboxed closures in one call
  - **Pro:** (X-PROC-NO-MULTI)
  - **Pro:** (X-CONFIG)
  - **Con:** (X-PROC-ERR-SPAN)

* (A-STRUCT) making struct a separate item
  - **Pro:** parsing the struct line is trivial, and all syntax is supported
    -  attributes: `#[derive(Debug, PartialEq, Eq, Default)]`, `#[repr(transparent)]`
  - **Pro:**
  - **Con if optional:** confusion over whether name should appear in impl item
    - (possible solution: diversify syntax for one-liners and two-liners. Require `impl` keyword in two-liners)
* (A-FOR) 

# Ideas

## `in mod mod_name` as sugar for workarounds to `proc-macro-hack` limitations

Basically, wrap the `proc-macro-hack` hack with another and add an extension to the syntax that supports 

```rust
unbox!{
    in mod add_const {
        struct AddConst(i32);
        impl Fn(&self, a: i32) -> i32 { a + self.0 }
    }
}
```

desugars to

```rust
pub use self::add_const::*;
unbox!{
    mod add_const {
        // note: Visibility::Inherited is changed to pub(super)
        pub(super) struct AddConst(pub(super) i32);
        impl Fn(&self, a: i32) -> i32 { a + self.0 }
    }
}
```
