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

