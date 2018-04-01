#![allow(unused)]

#[macro_use]
extern crate unbox;

fn main() {
    pub trait FnOnce<Args> {
        type Output;

        fn call_once(self, args: Args) -> Self::Output;
    }

    // Desparate times call for desparate measures.
    // If you are in an fn body, then `#![unbox(mod_scope(name))]` won't work.
    //
    // Instead, you must use `block_scope`:
    unbox!{
        #![unbox(block_scope)]
        FnOnce Foo() {}
    }

    // Just one thing: You CANNOT use `unbox` with block_scope twice in the same scope.
    // This is a limitation of proc-macro-hack item macros.
    {
        // this is a new scope, so this is okay
        unbox!{
            #![unbox(block_scope)]
            FnOnce Bar() {}
        }

        // also, hey, lookitthat, they work
        Foo.call_once(());
        Bar.call_once(());
    }

    // // THIS IS NOT OKAY
    // // You'll get error[E0428]: the name `ProcMacroHack` is defined multiple times
    // unbox!{
    //     #![unbox(block_scope)]
    //     FnOnce Baz() {}
    // }

    // it ain't called proc-macro-hack for nothing
}
