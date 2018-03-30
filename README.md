`unbox-macro`: Proc macro branch
================================

Welcome to the experimental proc-macro branch of `unbox-macro`!

This is an effort towards updating this old piece of trash into a crate suitable for publication on crates.io. The updated crate will be suitable for use on stable (for deriving any triple of traits that *resemble* the Fn heirarchy).

**What follows is the original README.md, of which approximately 0% still applies to this branch.**

---

unbox-macro
===========

Provides an `unbox!` macro for creating unboxed closures
which are nameable in the type system.

  * [unbox-macro](#unbox-macro)
    * [Is unbox_macro right for me?](#is-unbox_macro-right-for-me)
    * [Sounds great! What is it for?](#sounds-great-what-is-it-for)
    * [unbox! mini-language](#unbox-mini-language)
    * [Examples?](#examples)
    * [Known limitations/bugs](#known-limitationsbugs)
    * [Why is it so terrible!?](#why-is-it-so-terrible)
    * [Comparison to alternatives](#comparison-to-alternatives)


Is `unbox_macro` right for me?
------------------------------

Possibly!  Do you:

* Hate future compatibility?
* Hate backwards compatibility?
* Get frustrated by seeing useful error messages?
* Enjoy long walks through [300-lines of `macro_rules!`](https://github.com/ExpHP/unbox-macro/blob/master/src/lib.rs#L22-L331)?
* Want nobody to use your library?
* Have a long-standing grudge against yourself, just in general?

Then you're in exactly the right place!


Sounds great! What is it for?
-----------------------------

In today's Rust, sometimes, small abstractions can take a lot of code.
For instance, suppose you have written a iterator adaptor function that
requires the use of `Iterator::map`.  Something like:

```rust
pub fn zip_with<I,J,F,A,B,C>(iter: I, jitter: J, func: F)
 -> ZipWith<I,J,F>
 where I: IntoIterator<Item=A>,
       J: IntoIterator<Item=B>,
       F: FnMut(A,B) -> C,
{
    iter.into_iter().zip(jitter.into_iter()).map(|(a,b)| func(a,b))
}
```

Okey dokey.  So what's the return type of that?

```rust
pub use std::iter::{Zip,Map};

pub type ZipWith<I,J,F> = Map<Zip<I,J>,F>; // :D
```

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

Long and short: **It is impossible to express the return type of this function in any manner!**

Possible solution number 1 is to **write a lot of code:**

```rust
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
```

(psssst... I hid a bug in there.  Can you find it?)

Possible solution number 2 is to **enable a bunch of nightly features,
and then write a lot of code:**

```rust
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
```

This crate provides you with a third option: **enable a bunch of
nightly features, import a crate which contains a lot of code,
and then write a smaller(?) amount of code:**

```rust
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
```
    
### Woah woah woah, what's up with this `Generic` and `Struct`... this has its own mini-language I have to learn!?

Yes. The original plan was really simple:

```rust
// I won't show any examples of my original plan because somebody
// quickly scanning the page for examples might see them and think
// they are real examples.
```

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

And before I knew it, in order to be able to fit the bare minimum
set of features necessary to make it even marginally useful,
it ended up with its own mini-lanugage.

### Can you explain it?

Okey dokey:

## [`unbox!` mini-language](https://github.com/ExpHP/unbox-macro/blob/master/doc/Grammar.md)

(go click that)

## Examples?

[Compile tests?](https://github.com/ExpHP/unbox-macro/blob/master/src/lib.rs#L339)

## Known limitations/bugs

* Only one closure definition per macro invocation.
* Unhelpful error messages.
* Did you hear it has its own mini-language?
* The arguments can only be simple identifiers and not patterns.
  (there was an RFC with the grammar change necessary to make this possible,
  [but that particular bit got cut](https://github.com/rust-lang/rfcs/pull/1494#issuecomment-181740601))

```rust
unbox!{ Fn Naughty((a,b): (i32,i32)) -> i32 { a + b } }
unbox!{ Fn Nice(tuple: (i32,i32)) -> i32 { tuple.0 + tuple.1 } }
```

* If the closure struct type has any non-lifetime type parameters,
  the arguments cannot have lifetimes.  **Fixing this will require
  another revision to the spec.**

## Why is it so terrible!?

MACROS ARE HARD GUYS

Let's face it.  Rust's current macro system is pretty gosh darned weak,
and basically seems to be something to hold people over until the
fabled "macros v2".
This macro is perhaps too ambitious for Rust's macro parser,
and may be better suited to ~~a plugin~~ I mean procedural macro.

Half of what this macro does now was not even possible two weeks prior
to me writing this; it was only at the end of July 2016 that [a fix had
finally been committed for the funny business with `tt` fragments](https://github.com/rust-lang/rust/pull/34908).
Prior to that fix, I could not have even *dreamed* of supporting trait
bounds (as it would have entailed actually *parsing* them!).

### Have you seen https://xkcd.com/1205/ ?

go away

### I could do so much better!

Please do!  I'm begging you!

### I mean, like, I made a typo and ended up with a 30-line error message? What is that?

YES I'M SORRY OKAY


Comparison to alternatives
--------------------------

### What about the accepted `impl Trait` RFC?  Soon, we won't need nameable types!

In its accepted form,
[the minimal `impl Trait` RFC](https://github.com/rust-lang/rfcs/pull/1522)
doesn't let you do this:

```rust
// Somewhere, in the City of Townsville...
let zipped = zip_with(0..3, 0..4, |x,y| {x+y});
assert_eq!(zipped.rev().len(), 3);

// Meanwhile, in the Town of Citysburg...
let zipped = zip_with(0.., 7.., |x,y| {x+y});
assert_eq!(zipped.next(), Some(7));
```

because it has no provision for
forwarding conditionally-implemented traits
such as the `DoubleEndedIterator` and `ExactSizeIterator` traits
seen on many iterator adaptors.
With `unbox_macro`, the above snippet works
because `zip_with`'s return type can be
explicitly defined in terms of `Zip` and `Map`.

### What about `Box<Iterator>` or `&Iterator`?

Yes, what about them?

---------------------------------------------------------------

    09:34:24 <toby_s>   macro_rules! over_you_all { }
