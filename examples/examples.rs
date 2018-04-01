#![allow(unused)]

#[macro_use]
extern crate unbox;

use std::ops::Add;

use self::alternate_fn_traits::*;
mod alternate_fn_traits {
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
}

unbox!{
    #![unbox(mod_scope(add_ten))]
    // * `mod_scope` is a workaround to limitations of proc-macro-hack.
    //   It transparently creates a module for the items, but for virtually
    //   all purposes you may treat the module as not being there.
    //   (for instance, pub(restricted) visibilities are interpreted as
    //    relative to the enclosing module)
    // * Single argument function.
    Fn AddTen(a: i32) -> i32 { a + 10 }
}

// Example of how it could be used in a return type.
// (FIXME: misleading example. You can write this type, but you won't be able
//         to *use* it since `Map` doesn't support our custom Fn traits!)
type YouCanNameThisType =
    ::std::iter::Map<
        ::std::vec::IntoIter<i32>,
        AddTen,
    >;

unbox!{
    #![unbox(mod_scope(add_ten_2))]
    // * Alternate form consisting of a struct and a special "impl" item.
    struct AddTen2;
    impl Fn(a: i32) -> i32 { a + 10 }
}

unbox!{
    #![unbox(mod_scope(add_ab))]
    // * Multiple arguments.
    Fn AddAB(a: i32, b: i32) -> i32 { a + b }
}

unbox!{
    #![unbox(mod_scope(add_abc))]
    // * A public function.
    pub Fn AddABC(a: i32, b: i32, c: i32) -> i32 { a + b + c }
}

unbox!{
    #![unbox(mod_scope(add_const))]
    // * Explicit &self.
    struct AddConst(i32);
    impl Fn(&self, a: i32) -> i32 { a + self.0 }
}

unbox!{
    #![unbox(mod_scope(im_so_pub))]
    // * Public modifiers on an explicit struct.
    pub struct ImSoPub(pub i32);
    impl Fn(&self, a: i32) -> i32 { a + self.0 }
}

unbox!{
    #![unbox(mod_scope(sum_it))]
    // * A FnOnce.
    // * A not-tuple struct.
    struct SumIt {
        pub vec: Vec<i32>
    }
    impl FnOnce(self) -> i32 { self.vec.into_iter().fold(0, |a, b| a + b) }
}

unbox!{
    #![unbox(mod_scope(overly_restricted))]
    // * You can specify the `Fn` kind for the Fn item sugar as well, I guess.
    //   Though, um... I'm really not sure why you *would*.
    FnOnce SingleUseTwelve() -> i32 { 12 }
}

unbox!{
    #![unbox(mod_scope(double))]
    // * Type params/bounds on the Fn sugar.
    // * These can include where bounds.
    Fn Double<T: Add<Output=T>>(a: T) -> T
    where T: Clone,
    { a.clone() + a }
}

unbox!{
    #![unbox(mod_scope(double_2))]
    // * Desugared form of the above.
    // * The placement of the generics was chosen to reduce the amount
    //   of work involved in transforming between the Fn and impl forms.
    //   (consider yourself lucky I didn't go for `impl for<T> Fn(a: T)`...)
    struct Double2;

    impl Fn <T: Add<Output=T>>(a: T) -> T
    where T: Clone,
    { a.clone() + a }
}

unbox!{
    #![unbox(mod_scope(push))]
    // * Type params and where bounds on an explicit struct.
    // * These are different from bounds on the impl/in the Fn sugar!
    struct Push<'a, T>(&'a mut Vec<T>) where T: 'a;

    impl FnMut(&mut self, x: T) { self.0.push(x) }
}

unbox!{
    #![unbox(mod_scope(id_const))]
    // * *Of course* we support trailing commas, silly!
    Fn Id<T,>(x: T,) -> T { x }

    // * You can have multiple closures in one invocation, although the
    //   spans printed by error messages might not be the most useful...
    struct Const<T,>(T,);
    impl FnOnce(self,) -> T { self.0 }
}

// * If you're really desparate you can impl more traits on the thing yourself.
//   For instance, Const can be reused if T is Copy.
impl<T: Copy> FnMut<()> for Const<T> {
    fn call_mut(&mut self, (): ()) -> T { self.0 }
}
impl<T: Copy> Fn<()> for Const<T> {
    fn call(&self, (): ()) -> T { self.0 }
}


unbox!{
    #![unbox(mod_scope(contains))]
    // * Now look at this; we've got lifetimes, type parameters, and
    //   where clauses in both places, and they all get correctly assembled
    //   into an impl where all of the lifetime parameters come first.
    //   Thanks, synom!
    pub struct Contains<'a, T>(&'a [T]) where T: 'a;

    impl Fn <'b, U: 'b>(&self, u: &'b U) -> bool
    where T: PartialEq<U>,
    { self.0.iter().any(|t| t == u) }
}

fn main() {
    // silence all the unused warnings
    // -er, I mean, test the functions.
    assert_eq!(12, AddTen.call((2,)));
    assert_eq!(12, AddTen2.call((2,)));
    assert_eq!(12, AddAB.call((2, 10)));
    assert_eq!(12, AddABC.call((2, 10, 0)));
    assert_eq!(12, AddConst(3).call((9,)));
    assert_eq!(12, ImSoPub(4).call((8,)));
    assert_eq!(12, SumIt { vec: vec![13, -1] }.call_once(()));
    assert_eq!(12, SingleUseTwelve.call_once(()));
    assert_eq!(12, Double.call_mut((6,)));
    assert_eq!(12, Double2.call_mut((6,)));
    assert_eq!(12, {
        let mut vec = vec![];
        {
            let mut f = Push(&mut vec);
            f.call_mut(("1",));
            f.call_mut(("2",));
        }
        vec.concat().parse().unwrap()
    });
    assert_eq!(12, Id.call_once((Id.call_mut((Id.call((12,)),)),)));
    assert_eq!(12, Const(12).call(()));
    assert_eq!(12, 12 + 765 * (Contains(&[12]).call((&11,)) as bool as i32));
}
