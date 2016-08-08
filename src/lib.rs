
//#![feature(trace_macros)]
#![feature(fn_traits)]
#![feature(unboxed_closures)]

//! Provides a macro to generate unboxed closures nameable in the type system.
//trace_macros!(true);

// Hi there! :D
// Please make your window at least this big:
// [==============================================================================================]
//
// (this assumes a fixed width font without those silly "fi" and "ff" digraphs,
//  and tabs no longer than 4 spaces)


#[macro_export]
macro_rules! unbox {
	($($r:tt)*) => {__unbox_parse!{a0 [$($r)*]}};
}

#[macro_export]
macro_rules! __unbox_parse {
	// The layout of the arguments is as follows: (square brackets are literal)
	//
	//    <label> [<unparsed remainder>] [<parsed #1>][<parsed #2>]...
	//
	// Each step consumes a portion of the unparsed remainder and appends
	//  zero or more square-bracketed entries to the end of the list of
	//  "parsed" objects.
	//
	// Once an object is added to the "parsed" list, this macro never looks at
	//  it again (it would be difficult to do so in a manner that isn't
	//  extremely fragile). All of that is saved for the __build step.
	//
	// The labels are simple terminal tokens to help the compiler find its way.
	// Labels are chosen so that it is hopefully not too difficult to insert
	// a new step in the middle without a huge diff.
	// (but then it ends up looking awful, so usually a second commit is needed
	//  to update the labels anyways :f)

	// Parse visibility modifier
	(a0 [pub($($vis:tt)*) $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{b0 [$($r)*]$($d)* [pub($($vis)*)] }};

	(a0 [pub              $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{b0 [$($r)*]$($d)* [pub          ] }};

	(a0 [                 $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{b0 [$($r)*]$($d)* [             ] }};

	// Parse struct type parameters
	(b0 [Generic({$($par:tt)*} where $($bnd:tt)*) $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{b5 [$($r)*]$($d)* [$($par)*][$($bnd)*] }};

	(b0 [Generic({$($par:tt)*}                  ) $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{b5 [$($r)*]$($d)* [$($par)*][        ] }};

	(b0 [                                         $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{b5 [$($r)*]$($d)* [        ][        ] }};

	// Read field list
	// NOTE: don't blindly rearrange; must always ensure first mismatch is a nonterminal
	(b5 [Struct(           ) $($r:tt)*]$($d:tt)*) => {__unbox_parse!{c0 [$($r)*]$($d)*[ unitlike ]}};
	(b5 [Struct($($fld:tt)+) $($r:tt)*]$($d:tt)*) => {__unbox_parse!{c0 [$($r)*]$($d)*[[$($fld)+]]}};
	(b5 [Struct{$($fld:tt)*} $($r:tt)*]$($d:tt)*) => {__unbox_parse!{c0 [$($r)*]$($d)*[{$($fld)*}]}};
	(b5 [Struct              $($r:tt)*]$($d:tt)*) => {__unbox_parse!{c0 [$($r)*]$($d)*[ unitlike ]}};
	(b5 [                    $($r:tt)*]$($d:tt)*) => {__unbox_parse!{c0 [$($r)*]$($d)*[ unitlike ]}};

	// Parse impl type parameters
	(c0 [For({$($par:tt)*} where $($bnd:tt)*) $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{d0 [$($r)*]$($d)* [$($par)*][$($bnd)*] }};

	(c0 [For({$($par:tt)*}                  ) $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{d0 [$($r)*]$($d)* [$($par)*][        ] }};

	(c0 [                                     $($r:tt)*]$($d:tt)*) =>
		{__unbox_parse!{d0 [$($r)*]$($d)* [        ][        ] }};

	// Validate Fn trait
	(d0 [FnOnce $($r:tt)*]$($d:tt)*) => {__unbox_parse!{e0 [$($r)*]$($d)*[FnOnce]}};
	(d0 [FnMut  $($r:tt)*]$($d:tt)*) => {__unbox_parse!{e0 [$($r)*]$($d)*[FnMut ]}};
	(d0 [Fn     $($r:tt)*]$($d:tt)*) => {__unbox_parse!{e0 [$($r)*]$($d)*[Fn    ]}};

	// Read struct name
	(e0 [$name:ident $($r:tt)*]$($d:tt)*) => {__unbox_parse!{f0 [$($r)*]$($d)*[$name]}};

	//======================
	// Parsing the args:  A whole ball game in itself.
	//
	// NOTE: This does not currently support self arguments written in pat:ty form
	//       (like "self: Self") because this macro uses some gymnastics to extract
	//       the self ident, and I don't feel I understand the rules around self
	//       arguments enough to handle them properly.
	//       (also, waste of time)

	// Two representations of `self` are extracted, if it is provided:
	//    - The self pattern (e.g. `&mut self`), for validation.
	//    - The `self` identifier, with correct hygiene.

	// First, duplicate the arglist in a temporary workspace.
	(f0 [($($arg:tt)*) $($r:tt)*]$($d:tt)*) => {__unbox_parse!{f1 {($($arg)*)($($arg)*)}[$($r)*]$($d)*}};
	(f0 [($($arg:tt)*) $($r:tt)*]$($d:tt)*) => {__unbox_parse!{f1 {($($arg)*)($($arg)*)}[$($r)*]$($d)*}};

	// Match against terminals in one copy to determine how to extract the
	//  `self` ident in the second copy. (for the time being, the unparsed
	//  remainder [$($r)*] has been tucked away into $d)
	(f1 {(&mut self $($xxx:tt)*) (&mut $slf:ident $($arg:tt)*)}$($d:tt)*) =>
		{__unbox_parse!{f2 {$($arg)*}$($d)*[&mut self][$slf]}};

	(f1 {(&self     $($xxx:tt)*) (&$slf:ident     $($arg:tt)*)}$($d:tt)*) =>
		{__unbox_parse!{f2 {$($arg)*}$($d)*[&self    ][$slf]}};

	(f1 {(self      $($xxx:tt)*) ($slf:ident      $($arg:tt)*)}$($d:tt)*) =>
		{__unbox_parse!{f2 {$($arg)*}$($d)*[self     ][$slf]}};

	(f1 {(          $($xxx:tt)*) (                $($arg:tt)*)}$($d:tt)*) =>
		{__unbox_parse!{f2 {$($arg)*}$($d)*[wildcard][self]}};

	// The comma after self was not parsed in the previous step because it
	//  would require mantaining yet another three additional cases.
	// Stripping it now is much easier; though as a result, there is an expected,
	// minor bug that an initial comma with no self is accepted. (e.g. `(, x: i32)`)
	(f2 {,$($arg:tt)*}$($d:tt)*) => {__unbox_parse!{f3 {$($arg)*}$($d)*}};
	(f2 { $($arg:tt)*}$($d:tt)*) => {__unbox_parse!{f3 {$($arg)*}$($d)*}};

	// Generate a pattern (a,b,c,) and a type (A,B,C,) for the arg tuple.
	// NOTE: `:` is not in the follow set for `pat` so we must settle for simple `ident` args
	(f3 {}                          $($d:tt)*) => {__unbox_parse!{f4 $($d)*[()][()]}};
	(f3 {$($arg:ident : $Arg:ty),+ }$($d:tt)*) => {__unbox_parse!{f4 $($d)*[($($arg,)+)][($($Arg,)+)]}};
	(f3 {$($arg:ident : $Arg:ty),+,}$($d:tt)*) => {__unbox_parse!{f4 $($d)*[($($arg,)+)][($($Arg,)+)]}};

	// Done with the args (whew!). Moving on, then...
	(f4 $($d:tt)*) => {__unbox_parse!{g0 $($d)*}};

	//======================

	// return type and code block, in one step because tt cannot follow ty
	(g0 [-> $out:ty $code:block]$($d:tt)*) => {__unbox_parse!{j0 []$($d)* [$out][$code] }};
	(g0 [           $code:block]$($d:tt)*) => {__unbox_parse!{j0 []$($d)* [ () ][$code] }};

	// done parsing
	(j0 [] $($d:tt)*) => {__unbox_build!{[] $($d)*}};
}

#[macro_export]
macro_rules! __unbox_build {
	// Expect a bunch of preparsed content delimited by square brackets
	// (the initial empty list serves as a label)
	([] $([$($all:tt)*])*) => {__unbox_build!{a0 $([$($all)*])*}};

	// NOTE: This is the most fragile point in the code!
	//       (in part because its sole purpose is to make the rest of the
	//        code LESS fragile)

	// Match all of the parsed fragments produced by __parse...
	(a0
	 $vis:tt  // `pub`, `pub(restricted)`, <nothing>
	 $spar:tt // struct type/lifetimes parameters
	 $sbnd:tt // struct bounds
	 $fld:tt  // [tuple fields], {struct fields}, or `unitlike`
	 $ipar:tt // impl type/lifetimes parameters
	 $ibnd:tt // impl bounds
	 $Fn:tt   // `Fn`, `FnOnce`, `FnMut`
	 $name:tt // struct name
	 $slfpat:tt // `self`, `&self`, `&mut self`, `wildcard` (if not provided)
	 $slf:tt  // `self` ident (for $code)
	 $pat:tt  // pattern for args tuple (for $code)
	 $Args:tt // Args type
	 $out:tt  // return type
	 $code:tt // code block
	) => {
		// ...and create groups for them based on how they are to be used.
		__unbox_build!{b0
			// groups that require post-processing
			[$Fn $slfpat]
			[$spar $ipar]
			[$sbnd $ibnd]
			// incomplete (!) lists of args for:
			[$fld $vis $name]                      // struct definition
			[$Fn $name $slf $pat $Args $out $code] // impl definition
		}
	};

	// Validate the sigil on self against the Fn trait.
	//  (this was too difficult to do inside the __parse macro)
	(b0 [[FnOnce]      [self]] $($r:tt)*) => {__unbox_build!{c0 $($r)*}};
	(b0 [[FnMut]  [&mut self]] $($r:tt)*) => {__unbox_build!{c0 $($r)*}};
	(b0 [[Fn]         [&self]] $($r:tt)*) => {__unbox_build!{c0 $($r)*}};
	(b0 [$Fn:tt    [wildcard]] $($r:tt)*) => {__unbox_build!{c0 $($r)*}};
	// FIXME The error message for a bad `self` arg points to "b0", making
	//       this restriction significantly less helpful than I intended...

	// push two things onto the end:
	//   - the struct parameter list
	//   - the full parameter list
	// we build the latter through a painful match.
	(c0 [[] []]             $($r:tt)*) =>
		{__unbox_build!{c1 $($r)* [] [] }};

	(c0 [[] [$($ipar:tt)+]] $($r:tt)*) =>
		{__unbox_build!{c1 $($r)* [] [$($ipar)+] }};

	(c0 [[$($spar:tt)+] []] $($r:tt)*) =>
		{__unbox_build!{c1 $($r)* [$($spar)+] [$($spar)+] }};

	(c0 [[$($spar:tt)+] [$($ipar:tt)+]] $($r:tt)*) =>
		{__unbox_build!{c1 $($r)* [$($spar)+] [$($spar)+ , $($ipar)+] }};

	// push two more things:
	//    - struct bounds
	//    - full bounds
	// We also take care of the `where` here, because an empty `where` clause is invalid.
	(c1 [[] []]             $($r:tt)*) =>
		{__unbox_build!{c2 $($r)* [] [] }};

	(c1 [[] [$($ibnd:tt)+]] $($r:tt)*) =>
		{__unbox_build!{c2 $($r)* [] [where $($ibnd)+] }};

	(c1 [[$($sbnd:tt)+] []] $($r:tt)*) =>
		{__unbox_build!{c2 $($r)* [where $($sbnd)+] [where $($sbnd)+] }};

	(c1 [[$($sbnd:tt)+] [$($ibnd:tt)+]] $($r:tt)*) =>
		{__unbox_build!{c2 $($r)* [where $($sbnd)+] [where $($sbnd)+ , $($ibnd)+] }};

	// Done pre-processing
	(c2 $($d:tt)*) => {__unbox_build!{d0 $($d)*}};

	// |============
	// | REST AREA =
	// |============
	// |
	// | Here you can wait a short for your eyes to stop crossing.
	// |
	//&&&--------------------------------------------------------------

	// Forward the aaprpriate fragments to more dedicated macros.
	(d0 $sfrags:tt $ifrags:tt $spar:tt $fpar:tt $swhr:tt $fwhr:tt) => {
		// Recall that:
		//   $sfrags = [$fld $vis $name]
		//   $ifrags = [$Fn $name $slf $pat $Args $out $code]
		// ...assuming that I remembered to update this comment.
		__unbox_build_struct!{$sfrags [$spar $swhr]}
		__unbox_build_impls! {$ifrags [$spar $fpar $fwhr]}
	};
}

// This defines the struct.
#[macro_export]
macro_rules! __unbox_build_struct {
	// switch based on $fld
	([[unitlike        ] [$($vis:tt)*] [$name:ident]]
	 [[$($par:tt)*] [$($whr:tt)*]]) => {
		$($vis)* struct $name<$($par)*> $($whr)*;
	};
	([[[$($fld:tt)+]] [$($vis:tt)*] [$name:ident]]
	 [[$($par:tt)*] [$($whr:tt)*]]) => {
		$($vis)* struct $name<$($par)*>($($fld)+) $($whr)*;
	};
	([[{$($fld:tt)*}] [$($vis:tt)*] [$name:ident]]
	 [[$($par:tt)*] [$($whr:tt)*]]) => {
		$($vis)* struct $name<$($par)*>{$($fld)*} $($whr)*
	};
}


// This generates the Fn{Mut,Once,} impls.
#[macro_export]
macro_rules! __unbox_build_impls {
	([[$Fn:ident] $name:tt $slf:tt $pat:tt $Args:tt $out:tt $code:tt]
	 [$spar:tt $fpar:tt $fwhr:tt]) => {
		__unbox_build_impls!{All $Fn
			[$name $spar $fpar $slf $pat $Args $out $fwhr $code] // args for primary impls
			[$name $spar $fpar           $Args $out $fwhr      ] // args for derived impls
		}
	};
	(All FnOnce $pfrags:tt $dfrags:tt) => {
		__unbox_build_impls!{Primary FnOnce $pfrags}
	};
	(All FnMut $pfrags:tt $dfrags:tt) => {
		__unbox_build_impls!{Derived FnOnce $dfrags}
		__unbox_build_impls!{Primary FnMut  $pfrags}
	};
	(All Fn $pfrags:tt $dfrags:tt) => {
		__unbox_build_impls!{Derived FnOnce $dfrags}
		__unbox_build_impls!{Derived FnMut  $dfrags}
		__unbox_build_impls!{Primary Fn     $pfrags}
	};

	// "All of that, just for this," you ask?
	// YOU GODDAMN BET, SON.
	(Primary FnOnce [[$name:ident][$($spar:tt)*][$($ipar:tt)*][$slf:ident]
	                 [$pat:pat][$Args:ty][$out:ty][$($whr:tt)*][$code:block]]) => {
		impl<$($ipar)*> FnOnce<$Args> for $name<$($spar)*> $($whr)* {
			type Output = $out;

			#[allow(unused_mut)]
			extern "rust-call"
			fn call_once(mut $slf, $pat: $Args) -> $out $code
		}
	};
	(Primary FnMut  [[$name:ident][$($spar:tt)*][$($ipar:tt)*][$slf:ident]
	                 [$pat:pat][$Args:ty][$out:ty][$($whr:tt)*][$code:block]]) => {
		impl<$($ipar)*> FnMut<$Args> for $name<$($spar)*> $($whr)* {
			extern "rust-call"
			fn call_mut(&mut $slf, $pat: $Args) -> $out $code
		}
	};
	(Primary Fn     [[$name:ident][$($spar:tt)*][$($ipar:tt)*][$slf:ident]
	                 [$pat:pat][$Args:ty][$out:ty][$($whr:tt)*][$code:block]]) => {
		impl<$($ipar)*> Fn<$Args> for $name<$($spar)*> $($whr)* {
			extern "rust-call"
			fn call(&$slf, $pat: $Args) -> $out $code
		}
	};

	(Derived FnOnce [[$name:ident][$($spar:tt)*][$($ipar:tt)*]
	                 [$Args:ty][$out:ty][$($whr:tt)*]]) => {
		impl<$($ipar)*> FnOnce<$Args> for $name<$($spar)*> $($whr)* {
			type Output = $out;

			extern "rust-call"
			fn call_once(mut self, args: $Args) -> $out
			{ self.call_mut(args) }
		}
	};
	(Derived FnMut  [[$name:ident][$($spar:tt)*][$($ipar:tt)*]
	                 [$Args:ty][$out:ty][$($whr:tt)*]]) => {
		impl<$($ipar)*> FnMut<$Args> for $name<$($spar)*> $($whr)* {
			extern "rust-call"
			fn call_mut(&mut self, args: $Args) -> $out
			{ self.call(args) }
		}
	};
}

//-------------------------------------------------

pub mod compiletests {

	use std::ops::Add;

	// Single arguments.
	unbox!{
		Fn AddTen(a:i32) -> i32 { a + 10 }
	}

	// Multiple arguments.
	unbox!{
		Fn AddAB(a:i32, b:i32) -> i32 { a + b }
	}

	// A tuple struct.
	// Explicit &self.
	unbox!{
		Struct(i32)
		Fn AddConst(&self, a:i32) -> i32 { a + self.0 }
	}

	// Public stuff.
	unbox!{
		pub Struct(pub i32)
		Fn ImSoPub(&self, a:i32) -> i32 { a + self.0 }
	}

	// An FnOnce.
	// A... non-tuple struct. (Why did I add this?)
	unbox!{
		Struct{pub vec: Vec<i32>}
		FnOnce SumIt(self) -> i32
		{ self.vec.into_iter().fold(0, |a,b| a+b) }
	}

	// Type params/bounds on impl.
	unbox!{
		For({T} where T:Add<Output=T>)
		Fn AddGeneric(a:T, b:T) -> T { a + b }
	}

	// Type params/bounds on struct.
	unbox!{
		Generic({'a,T} where T: 'a) Struct(&'a mut Vec<T>)
		FnMut Push(&mut self, x: T) { self.0.push(x) }
	}

	// Where-less for no bounds.
	// A terminal comma.
	unbox!{ For({T}) Fn Id(x:T,) -> T { x } }
	unbox!{ Generic({T}) Struct(T) FnOnce Const(self) -> T { self.0 } }

	// Now make me one with everything.
	// FIXME: error: lifetime parameters must be declared prior to type parameters
	//   ASIHGAEgEWJTOWAPTJE{[g AHHHHHHHHHHHHHHHHHHH
	#[cfg(nope)]
	unbox!{
		Generic({'a,T} where T:'a, T:PartialEq) Struct(&'a Vec<T>)
		For({'b} where T:'b)
		Fn Contains(&self, x: &'b T) -> bool { self.0.contains(x) }
	}

	// README example
	unbox!{
		pub Generic({F}) Struct(F)
		For({A,B,C} where F: FnMut(A,B) -> C)
		FnMut Uncurry(&mut self, tuple: (A,B)) -> C {
			(&mut self.0)(tuple.0, tuple.1)
		}
	}

	// Example of how it could be used in a return type.
	type YouCanNameThisType =
		::std::iter::Map<
			::std::vec::IntoIter<i32>,
			AddTen>;
}

// >_>
// <_<
pub use self::compiletests as examples;
