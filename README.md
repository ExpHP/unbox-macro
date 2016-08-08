unbox-macro
===========

Provides an `unbox!` macro for creating unboxed closures
which are nameable in the type system.

Is `unbox_macro` right for me?
------------------------------

Possibly!  Do you:

* Hate future compatibility?
* Hate backwards compatibility?
* Get frustrated by seeing useful error messages?
* Enjoy long walks through [300-lines of `macro_rules!`][1]?
* Want nobody to use your library?
* Have a long-standing grudge against yourself, just in general?

Then you're in exactly the right place!

[1] https://github.com/ExpHP/unbox-macro/blob/master/src/lib.rs#L22-L331


Sounds great! What is it for?
-----------------------------

In today's Rust, sometimes, small abstractions can take a lot of code.
For instance, suppose you have written a iterator adaptor function that
requires the use of `Iterator::map`.  Something like:

    pub fn zip_with<I,J,F,A,B,C>(iter: I, jitter: J, func: F)
     -> ZipWith<I,J,F>
     where I: IntoIterator<Item=A>,
           J: IntoIterator<Item=B>,
           F: FnMut(A,B) -> C,
    {
        iter.into_iter().zip(jitter.into_iter()).map(|(a,b)| func(a,b))
    }

Okey dokey.  So what's the return type of that?

    pub use std::iter::{Zip,Map};
    
    pub type ZipWith<I,J,F> = Map<Zip<I,J>,F>; // :D

Yeah!  Er... not quite.

    error: mismatched types [--explain E0308]
     --> <anon>:7:9
    7 |>         iter.into_iter().zip(jitter.into_iter()).map(|(a,b)| f(a,b))
      |>         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expected type parameter, found associated type
    note: expected type `std::iter::Map<std::iter::Zip<I, J>, F>`
    note:    found type `std::iter::Map<std::iter::Zip<<I as std::iter::IntoIterator>::IntoIter, <J as std::iter::IntoIterator>::IntoIter>, [closure@<anon>:7:54: 7:68]>`

Take a close look again at the definition,
because *there's a closure in there*;
a closure that takes a 2-value tuple and calls a two-argument function.
This is extremely unfortunate for us,
because, as one might imagine, `[closure@<anon>:7:54: 7:68]` is not exactly
a valid name in rust.
It is impossible to express the type of this function in any manner!

Possible solution number 1 is to **write a lot of code:**

    use std::iter::Zip;

    #[must_use = "iterator adaptors are lazy and do nothing unless consumed"]
    #[derive(Debug,Clone)]
    struct ZipWith<I,J,F>{ iter: Zip<I,J>, func: F }

    impl<I,J,F,C> Iterator for ZipWith<I,J,F>
     where I: Iterator, J: Iterator,
           F: FnMut(I::Item, J::Item) -> C,
    {
        type Item = C;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next().map(|(a,b)| (&mut self.func)(a,b))
        }
        #[inline(always)]
        fn size_hint(&self) -> (usize, Option<usize>) {
            self.iter.size_hint()
        }
    }

    impl<I,J,F,C> ExactSizeIterator for ZipWith<I,J,F>
     where I: ExactSizeIterator,
           J: ExactSizeIterator,
           F: FnMut(I::Item,J::Item) -> C,
    { }

    impl<I,J,F,C> DoubleEndedIterator for ZipWith<I,J,F>
     where I: DoubleEndedIterator + ExactSizeIterator,
           J: DoubleEndedIterator + ExactSizeIterator,
           F: FnMut(I::Item,J::Item) -> C,
    {
        #[inline(always)]
        fn next_back(&mut self) -> Option<Self::Item> {
            self.iter.next().map(|(a,b)| (&mut self.func)(a,b))
        }
    }

    pub fn zip_with<I,J,F,C>(iter: I, jtre: J, func: F) -> ZipWith<I,J,F>
     where I: Iterator, J: Iterator,
           F: FnMut(I::Item, J::Item) -> C,
    { ZipWith { iter: iter.zip(jtre), func: func } }

(psssst... I hid a bug in there.  Can you find it?)

Possible solution number 2 is to **enable a bunch of nightly features,
and then write a lot of code:**

    #![feature(unboxed_closures)]
    #![feature(fn_traits)]
    #![feature(non_ascii_idents)]

    use std::iter::{Zip,Map};

    /// Hmm, this name made a lot more sense in Haskell...
    pub struct Uncurry<F>{ func: F }
    pub type ZipWith<I,J,F> = Map<Zip<I,J>, Uncurry<F>>;

    impl<F,A,B,C> FnOnce<((A,B),)> for Uncurry<F>
     where F: FnMut(A,B) -> C,
    {
        type Output = C;

        extern "rust-call"
        fn call_once(mut self, args: ((A,B),)) -> C
        { self.call_mut(args) }
    }

    impl<F,A,B,C> FnMut<((A,B),)> for Uncurry<F>
     where F: FnMut(A,B) -> C,
    {
        extern "rust-call"
        fn call_mut(&mut self, ((a,b),): ((A,B),)) -> C
        { (&mut self.func)(a,b) }
    }

    pub fn zip_with<I,J,F,C>(iter: I, ジーター: J, func: F)
     -> ZipWith<I,J,F>
     where I: Iterator, J: Iterator,
           F: FnMut(I::Item, J::Item) -> C,
    { iter.zip(ジーター).map(Uncurry{func: func}) }

This crate provides you with a third option: **enable a bunch of
nightly features, import a crate which contains a lot of code,
and then write a smaller(?) amount of code:**

    #![feature(unboxed_closures)]
    #![feature(fn_traits)]
    #[macro_use]
    extern crate unbox_macro;

    use std::iter::{Map,Zip};

    pub type ZipWith<I,J,F> = Map<Zip<I,J>, Uncurry<F>>;
    unbox!{
        pub Generic({F}) Struct(F)
        For({A,B,C} where F: FnMut(A,B) -> C)
        FnMut Uncurry(&mut self, tuple: (A,B)) -> C {
            (&mut self.0)(tuple.0, tuple.1)
        }
    }

    pub fn zip_with<I,J,F,C>(iter: I, jydr: J, func: F)
     -> ZipWith<I,J,F>
     where I: Iterator, J: Iterator,
           F: FnMut(I::Item, J::Item) -> C,
    { iter.zip(jydr).map(Uncurry(func)) }

### Woah woah woah, what's up with this `Generic` and `Struct`... this has its own mini-language I have to learn!?

Yes. The original plan was really simple:

    // I won't show any examples of my original plan because somebody
    // quickly scanning the page for examples might see them and think
    // they are real examples.

Beautifully simple, right? But then I discovered various flaws:

* By design, it wasn't able to specify type bounds.
  This was a fatal mistake; without the ability to specify `T:'a`,
  you can't create a struct that contains `&'a T`, and thus either
  (a) you're stuck with `FnOnce`, or (b) every closed-over type
  must be `Copy`.
* In fact, **two** sets of type variables and bounds are needed
  for it to be even *remotely* useful in most applications.
  You need one set of type variables for the struct, and another
  for the `impl`s; without that second set, you can't take a
  `&'b T` argument. (this is why HRTB exists!)
* The only reliable way to delimit arbitrary token streams is
  with `[]`, `()`, or `{}`.  I quickly ran out of usable syntax!
  (notice that `<>` are not on this list, and with good reason!)

And before I knew it, I had a mini-lanugage.

### Can you explain it?

Okey dokey.

## `unwrap!` mini-language

Here's a simple example, showing the bits that most-closely
resemble function syntax.

	unbox!{ pub Fn AddAB(a:i32, b:i32) -> i32 { a + b } }

The visibility qualifier is optional and supports `pub(restricted)`.

`Fn` is the most specialized trait in the `Fn` heirarchy we want to
implement; impls will be derived for its supertraits, `FnMut` and
`FnOnce`. (similarly, `FnOnce` would be derived if we wrote `FnMut`).

There are a number of optional fields; the above is equivalent to this
**expanded form**:

    unbox!{
        pub Generic({}) Struct For({}) Fn AddAB(&self, a:i32, b:i32) -> i32 { a + b }
    }

Here you see that all unboxed functions can have an **explicit self**,
but you can omit it as long as you don't need to refer to `self` inside the closure
(in which case the `self` arg would be needed to properly resolve hygiene).
To help avoid confusion in the function implementation,
the `self` pattern must be consistent with the `Fn` trait:

 |   Trait  | `self` arg |
 | -------- | -----------|
 | `FnOnce` |   `self`   |
 | `FnMut`  |`&mut self` |
 | `Fn`     |  `&self`   |

Returning to the above, you will note three special "keywords" `Generic`, `Struct`,
and `For`. Note that **these three terms must always be specified in this order**.
Who are these three stooges, exactly? Let's tackle them in order of decreasing
importance:

**Struct** lets you specify the fields of the struct (i.e. the variables it is to
be closed over):

 * `Struct` is unit-like.
 * `Struct(A,B,...,Z)` is a tuple struct.
 * `Struct{a:A, b:B, ..., z:Z}` is a... um, struct struct.

**Generic** lets you specify type parameters and bounds for the struct.
The type parameters go inside the curly braces, optionally followed by where bounds:

                                 creates
    Generic({T}) Struct(T,T)     ======>   struct Name<T>(T,T);
    
    Generic({'a,T} where T:'a)   creates
        Struct(&'a T)            ======>   struct Name<'a,T>(&'a T) where T:'a;
    
    Generic({'a,T,U,V} where     creates
     T:Add<U,Output=V> + Copy,   ======>   <exactly what you think>
     U:Hash) Struct(whatever)

The reason for the name is because it makes your Struct a *Generic Struct*.

**For** lets you specify bounds *on the impl*.  The difference may seem subtle
at first, but you must be aware of it; these are parameters which describe
the *arguments to the function* (while the parameters in `Generic` describe its
closed-over environment).

Consider the difference between a function which always produces a single value,
versus a function which always returns its argument (no matter the type):

	// always produces same value
	unbox!{ Generic({T}) Struct(T) FnOnce Const(self) -> T { self.0 } }
	// always returns argument
	unbox!{ For({T}) Fn Id(x:T) -> T { x } }

All bounds on the struct are also automatically included on the impl.
In general, `For` should only need to contain bounds which involve at least
one of the type parameters newly introduced inside the `For`.

The reason for the name is as a parallel to HRTB syntax `for<'a>`,
as some of the bounds it lets you introduce are of a similar nature.

Here's something it *would* let you do, if it wasn't **terribly,
horribly broken right now:**

	unbox!{
		Generic({'a,T} where T:'a, T:PartialEq)
		  Struct(&'a Vec<T>)
		For({'b} where T:'b)
		Fn Contains(&self, x: &'b T) -> bool { self.0.contains(x) }
	}

Which brings us to the next section.

## Why is it so terrible!?

MACROS ARE HARD GUYS

Let's face it.  Rust's current macro system is pretty gosh darned weak,
and basically seems to be something to hold people over until the
fabled "macros v2".
This macro is perhaps too ambitious for Rust's macro parser, and is
perhaps better suited to ~a plugin~--er, I mean procedural macro.

Half of what this macro does now was not even possible two weeks prior
to me writing this; it was only at the end of July 2016 that [a fix had
finally been committed for the funny business with `tt` tokens][2].
Prior to that fix, I could not have even *dreamed* of supporting trait
bounds (as it would have entailed actually *parsing* them!).

[2] https://github.com/rust-lang/rust/pull/34908

### Have you seen https://xkcd.com/1205/ ?

yes, go away

### I could do so much better!

Please do!  I'm begging you!

### I mean, like, I made a typo and ended up with a 30-line error message? What is that?

YES I'M SORRY OKAY


Comparison to alternatives
--------------------------

### What about `Box<Iterator>` or `&Iterator`?

Yes, what about them?

### ...uh... okay, what about the accepted `[impl Trait][1]` RFC?  Soon, we won't need nameable types!

In its current form, that RFC doesn't let you do this:

    let x = my_cool_object.iter();
    println!("{}", x.rev().len());

because it provides no provision for forwarding the `DoubleEndedIterator` and
`ExactSizeIterator` traits.

[1] https://github.com/rust-lang/rfcs/pull/1522

The end
-------
    09:34:24 <toby_s>   macro_rules! over_you_all { }
