#[macro_use]
extern crate proc_macro_hack;

#[allow(unused_imports)]
#[macro_use]
extern crate unbox_impl;
#[doc(hidden)]
pub use unbox_impl::*;

proc_macro_item_decl! {
    unbox_hack! => unbox_hack_impl
}

proc_macro_item_decl! {
    unbox_hack_inmod! => unbox_hack_inmod_impl
}

/// Eh, what's up docs?
///
/// FIXME
///
/// # Syntax
///
/// FIXME
///
/// # Scope directives
///
/// FIXME
#[macro_export]
macro_rules! unbox {
    (
        #![unbox(mod_scope($mod_name:ident))]
        $($tt:tt)*
    ) => {
        #[allow(unused)]
        pub use self::$mod_name::*;
        mod $mod_name {
            #[allow(unused)]
            use super::*;

            unbox_hack_inmod!{ $($tt)* }

            // "error: A non-empty glob must import something with the glob's visibility"
            // ...yeah. I don't know.
            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn __a_pub_thing_you_shouldnt_worry_about() {}
        }
    };

    // For defining an unboxed closure without wrapping the call in a module.
    // Needed inside fn bodies.
    (
        #![unbox(block_scope)]
        $($tt:tt)*
    ) => {
        #[allow(non_snake_case)]
        let __unbox_block_scope_is_only_for_use_in_fn_bodies__sorry = ();

        unbox_hack!{ $($tt)* }
    };

    (
        $($tt:tt)*
    ) => {
        // This is the cost of procedural macros pre-macros 2.0, guys.
        // Grin and bear it.
        compile_error!{r#"\
            Any call to unbox! must \
            begin with a `mod_scope` or `block_scope` directive. \
            Please see the documentation of `unbox!` for more details \
            (under "Scope directives").\
        "#}
    };
}
