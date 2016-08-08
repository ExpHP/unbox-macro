# `unwrap!` mini-language

Here's a simple example, showing the bits that most-closely
resemble function syntax.

    unbox!{ pub Fn AddAB(a:i32, b:i32) -> i32 { a + b } }

**Note:** Only one function is allowed per `unbox!` invocation.

The **visibility qualifier** works as usual (omitted = private)
and supports `pub(restricted)`.

You get to select which **Fn trait** is the primary implementation.
Because `Fn` is the most specialized trait in the `Fn` heirarchy,
impls will be derived for its supertraits, `FnMut` and `FnOnce`.
(similarly, `FnOnce` would be derived if we wrote `FnMut`).

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

 |   Trait  | `self` arg  |
 | -------- | ----------- |
 | `FnOnce` | `self`      |
 | `FnMut`  | `&mut self` |
 | `Fn`     | `&self`     |

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

