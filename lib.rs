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
#[macro_export]
macro_rules! unbox {
    (
        in mod $mod_name:ident {
            $($tt:tt)*
        }
    ) => {
        pub use self::$mod_name::*;
        mod $mod_name {
            use super::*;

            // FIXME: use unbox_hack_inmod!{ $($tt)* }
            unbox_hack!{ $($tt)* }
        }
    };
    ($($tt:tt)*) => {
        unbox_hack!{ $($tt)* }
    };
}
