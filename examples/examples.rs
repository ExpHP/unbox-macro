#[macro_use]
extern crate unbox;

use std::ops::Add;

// Shadow the Fn traits with ones that we're allowed to implement,
// so that these examples work on stable.
//
// You can also use configuration annotations to tell unbox! what traits
// to implement (FIXME show how this is done), but you need to annotate
// every invocation and that can get annoying; so it is easier to just
// shadow them when possible.
pub trait FnOnce<Args> {
    type Output;

    fn call_once(self, args: Args) -> Self::Output;
}

pub trait FnMut<Args>: FnOnce<Args> {
    fn call_mut(&mut self, args: Args) -> Self::Output;
}

pub trait Fn<Args>: FnMut<Args> {
    fn call(&self, args: Args) -> Self::Output;
}

// * Single argument.
// * Each call is wrapped in a module to work around limitations
//   of proc-macro-hack.
// * In the future, sugar may be added for this.
use self::add_ten::AddTen;
mod add_ten {
    use super::*;
    unbox!{
        pub Fn AddTen(a: i32) -> i32 { a + 10 }
    }
}

// * Desugared to show implicitly generated struct
mod add_ten_2 {
    use super::*;
    unbox!{
        pub(super) struct AddTen2;
        Fn(a: i32) -> i32 { a + 10 }
    }
}

mod add_ab {
    use super::*;
    unbox!{
        // * Multiple arguments.
        Fn AddAB(a: i32, b: i32) -> i32 { a + b }
    }
}

mod add_abc {
    use super::*;
    unbox!{
        // * A public function.
        pub Fn AddABC(a: i32, b: i32, c: i32) -> i32 { a + b + c }
    }
}

mod add_const {
    use super::*;
    unbox!{
        // * An explicit struct.
        // * Explicit &self.
        struct AddConst(i32);
        Fn AddConst(&self, a: i32) -> i32 { a + self.0 }
    }
}

mod im_so_pub {
    use super::*;
    unbox!{
        // * Public modifiers on an explicit struct.
        //
        //   (note: when there's an explicit struct, the visibility modifiers MUST
        //   go there rather than on the Fn item)
        pub struct ImSoPub(pub i32);
        Fn(&self, a: i32) -> i32 { a + self.0 }
    }
}

mod sum_it {
    use super::*;
    unbox!{
        // * A FnOnce.
        // * A not-tuple struct.
        // * This also shows how the name on the Fn item can be omitted when there
        //   is an explicit struct. I mean, who wants to write everything twice?
        struct SumIt {
            pub vec: Vec<i32>
        }
        FnOnce(self) -> i32 { self.vec.into_iter().fold(0, |a, b| a + b) }
    }
}

mod double {
    use super::*;
    unbox!{
        // * Type params/bounds on the Fn item.
        //   These params and bounds will appear **only on the impl,** not on the struct.
        //   (you can think of the following unboxed closure as a function which
        //    implements `for<T: Clone + Add<Output=T>> Fn(T) -> T`)
        Fn Double<T: Add<Output=T>>(a: T) -> T
        where T: Clone,
        { a.clone() + a }
    }
}

mod push {
    use super::*;
    unbox!{
        // * Type params/bounds on an explicit struct.
        struct Push<'a, T>(&'a mut Vec<T>) where T: 'a;
        FnMut(&mut self, x: T) { self.0.push(x) }
    }
}

mod id_const {
    use super::*;
    unbox!{
        // * And *of course* we support trailing commas, silly!
        Fn Id<T,>(x: T,) -> T { x }

        // * You can have multiple closures in one invocation, although the
        //   spans printed by error messages might not be the most useful...
        struct Const<T,>(T,);
        FnOnce Const(self,) -> T { self.0 }
    }
}

mod contains {
    use super::*;
    unbox!{
        // * Now look at this; we've got lifetimes and type parameters in both places,
        //   and they all get correctly assembled into an impl where all of the
        //   lifetime parameters come first! Thanks, synom!
        pub struct Contains<'a, T>(&'a Vec<T>) where T: 'a;

        Fn <'b, U: 'b>(&self, u: &'b U) -> bool
        where T: PartialEq<U>,
        { self.0.iter().any(|t| t == u) }
    }
}

// // alternative syntax under consideration:
// // - diversifies the syntax for explicit struct vs implicit struct.
// //   - implicit struct will still look like a Fn item
// //   - explicit struct will be changed to have the Fn item resemble an impl
// // - this reduces confusion about inclusion of struct name and/or visibility modifiers
// //   on the impl item
// mod contains {
//     unbox!{
//         pub struct Contains<'a, T>(&'a Vec<T>) where T: 'a;
//
//         impl for<'b, U: 'b> Fn(&self, u: &'b U) -> bool
//         where T: PartialEq<U>,
//         { self.0.iter().any(|t| t == u) }
//     }
// }

mod uncurry {
    use super::*;
    unbox!{
        // README example
        struct Uncurry<F>(F);

        FnMut <A,B,C>(&mut self, tuple: (A, B)) -> C
        where F: FnMut<(A, B), Output=C>,
        { self.0.call_mut(tuple) }
    }
}

// Example of how it could be used in a return type.
type YouCanNameThisType =
    ::std::iter::Map<
        ::std::vec::IntoIter<i32>,
        AddTen,
    >;

fn main() { }
