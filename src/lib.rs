
//! Provides a macro to generate unboxed closures nameable in the type system.

// Hi there! :D
// Please make your window at least this big:
// [==============================================================================================]
//
// (this assumes a fixed width font without those silly "fi" and "ff" digraphs,
//  and tabs no longer than 4 spaces)


#[macro_export]
macro_rules! unbox {
	($($r:tt)*) => {__parse!{a0 [$($r)*]}};
}

//FIXME: Due to a... slight oversight, this currently cannot support functions
//       that take arguments with lifetimes.  (think closures with HRTB)
//
// I'm fairly certain support for this can be added, but it would involve:
//   * A way to specify extra type parameters for the impl which are not on
//     the type.
//   * A way to specify bounds on the impl which are not on the struct.
//     (N.B. the struct still needs its own separate bounds list because
//      `struct Foo<'a,T>(&'a T)` requires a `T:'a` bound on declaration)
//   * Codegen to combine the two lists of bounds (to my best estimate this
//     involves a literal `+` and lots of prayer)
//   * Crying

macro_rules! __parse {
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
	//  a new step in the middle.

	// Parse visibility modifier
	(a0 [pub($($vis:tt)*) $($r:tt)*]$($d:tt)*) =>
		{__parse!{b0 [$($r)*]$($d)* [pub($($vis)*)] }};

	(a0 [pub              $($r:tt)*]$($d:tt)*) =>
		{__parse!{b0 [$($r)*]$($d)* [pub          ] }};

	(a0 [                 $($r:tt)*]$($d:tt)*) =>
		{__parse!{b0 [$($r)*]$($d)* [             ] }};

	// Validate Fn trait
	(b0 [FnOnce $($r:tt)*]$($d:tt)*) => {__parse!{c0 [$($r)*]$($d)*[FnOnce]}};
	(b0 [FnMut  $($r:tt)*]$($d:tt)*) => {__parse!{c0 [$($r)*]$($d)*[FnMut ]}};
	(b0 [Fn     $($r:tt)*]$($d:tt)*) => {__parse!{c0 [$($r)*]$($d)*[Fn    ]}};

	// Read optional type parameter list;
	//  to support lifetimes it is $($par:tt)* instead of $($par:ident),*
	(c0 [[$($par:tt)*] $($r:tt)*]$($d:tt)*) => {__parse!{d0 [$($r)*]$($d)*[$($par)*]}};
	(c0 [              $($r:tt)*]$($d:tt)*) => {__parse!{d0 [$($r)*]$($d)*[        ]}};

	// Read struct name
	(d0 [$name:ident $($r:tt)*]$($d:tt)*) => {__parse!{e0 [$($r)*]$($d)*[$name]}};

	// Read field list
	// NOTE: don't blindly rearrange; must always ensure first mismatch is a nonterminal
	(e0 [{$($fld:item),*} $($r:tt)*]$($d:tt)*) => {__parse!{f0 [$($r)*]$($d)*[{$($fld),*}]}};
	(e0 [[              ] $($r:tt)*]$($d:tt)*) => {__parse!{f0 [$($r)*]$($d)*[  unitlike ]}};
	(e0 [[$($fld:item),+] $($r:tt)*]$($d:tt)*) => {__parse!{f0 [$($r)*]$($d)*[[$($fld),+]]}};
	(e0 [                 $($r:tt)*]$($d:tt)*) => {__parse!{f0 [$($r)*]$($d)*[  unitlike ]}};

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
	(f0 [($($arg:tt)*) $($r:tt)*]$($d:tt)*) => {__parse!{f1 {($($arg)*)($($arg)*)}[$($r)*]$($d)*}};
	(f0 [($($arg:tt)*) $($r:tt)*]$($d:tt)*) => {__parse!{f1 {($($arg)*)($($arg)*)}[$($r)*]$($d)*}};

	// Match against terminals in one copy to determine how to extract the
	//  `self` ident in the second copy. (for the time being, the unparsed
	//  remainder [$($r)*] has been tucked away into $d)
	// NOTE: The comma after self is not parsed in this step because it would appear
	//       to require mantaining yet another three additional cases.  As a result
	//       there is an expected, minor bug that an initial comma with no self is
	//       accepted. (e.g. `(, x: i32)`)
	(f1 {(&mut self $($xxx:tt)*) (&mut $slf:ident $($arg:tt)*)}$($d:tt)*) =>
		{__parse!{f2 {$($arg)*}$($d)*[&mut self][$slf]}};

	(f1 {(&self     $($xxx:tt)*) (&$slf:ident     $($arg:tt)*)}$($d:tt)*) =>
		{__parse!{f2 {$($arg)*}$($d)*[&self    ][$slf]}};

	(f1 {(self      $($xxx:tt)*) ($slf:ident      $($arg:tt)*)}$($d:tt)*) =>
		{__parse!{f2 {$($arg)*}$($d)*[self     ][$slf]}};

	(f1 {(          $($xxx:tt)*) (                $($arg:tt)*)}$($d:tt)*) =>
		{__parse!{f2 {$($arg)*}$($d)*[wildcard][self]}};

	// Generate a pattern (a,b,c,) and a type (A,B,C,) for the arg tuple.
	// FIXME: Haven't tested whether the yes/no terminating comma cases work (I doubt they do)
	(f2 { }$($d:tt)*) => {__parse!{f3 $($d)*[()][()]}};
	(f2 {,}$($d:tt)*) => {__parse!{f3 $($d)*[()][()]}};
	// NOTE: `:` is not in the follow set for `pat` so we must settle for simple `ident` args
	(f2 {,$($arg:ident : $Arg:ty),+ }$($d:tt)*) => {__parse!{f3 $($d)*[$($arg,)+][$($Arg,)+]}};
	(f2 {,$($arg:ident : $Arg:ty),+,}$($d:tt)*) => {__parse!{f3 $($d)*[$($arg,)+][$($Arg,)+]}};
	// Done with the args (whew!). Moving on, then...
	(f3 $($d:tt)*) => {__parse!{g0 $($d)*}};

	//======================

	// FIXME: This doesn't compile, because tt cannot follow ty.
	// No point trying to fix this because I need to come up with a new
	//  way to specify bounds anyways...

	// return type (optional)
	(g0 [-> $out:ty $($r:tt)*]$($d:tt)*) => {__parse!{h0 [$($r)*]$($d)*[$out]}};
	(g0 [           $($r:tt)*]$($d:tt)*) => {__parse!{h0 [$($r)*]$($d)*[ () ]}};

	// where clause (optional)
	(h0 [where [$($bnd:tt)*] $($r:tt)*]$($d:tt)*) => {__parse!{i0 [$($r)*]$($d)*[where $($bnd)*]}};
	(h0 [                    $($r:tt)*]$($d:tt)*) => {__parse!{i0 [$($r)*]$($d)*[              ]}};

	// code block
	(i0 [$code:block $($r:tt)*]$($d:tt)*) => {__parse!{j0 [$($r)*]$($d)*[$code]}};

	// done parsing; require that $r is empty
	(j0 [] $($d)*) => {__build!{[] $($d)*}};
}

macro_rules! __build {
	// Expect a bunch of preparsed content delimited by square brackets
	([] $([$($all:tt)*])*) => {__build!{a0 $([$($all:tt)*])*}};

	// NOTE: This is the most fragile point in the code!
	//       (in part because its sole purpose is to make the rest of the
	//        code LESS fragile)

	// Match all of the parsed fragments produced by __parse...
	(a0
	 $vis:tt  // `pub`, `pub(restricted)`, <nothing>
	 $Fn:tt   // `Fn`, `FnOnce`, `FnMut`
	 $par:tt  // struct type/lifetimes parameters (without bounds)
	 $name:tt // struct name
	 $fld:tt  // [tuple fields], {struct fields}, or `unitlike`
	 $slfpat:tt // `self`, `&self`, `&mut self`, `wildcard` (if not provided)
	 $slf:tt  // `self` ident (for $code)
	 $pat:tt  // pattern for args tuple (for $code)
	 $Args:tt // Args type
	 $out:tt  // return type
	 $bnd:tt  // struct bounds (`where <bounds>` or <nothing>)
	 $code:tt // code block
	) => {
		// ...and create groups for them based on how they are to be used.
		__build!{b0
			[$Fn $slfpat] // for validation
			[$fld $vis $name $bnd $par] // for defining struct
			[$Fn $name $par $slf $pat $Args $out $bnd $code] // for defining impls
		}
	};

	// Validate the sigil on self against the Fn trait.
	//  (this was too difficult to do inside the __parse macro)
	// NOTE: might be easier to match against [$slfpat $Fn] order,
	//       but then errors would misleadingly point at `Fn` instead of `self`
	// FIXME does the wildcard case work here? (I expect so)
	(b0 [[FnOnce]      [self]] $($r:tt)*) => {__build!{c0 $($r)*}};
	(b0 [[FnMut]  [&mut self]] $($r:tt)*) => {__build!{c0 $($r)*}};
	(b0 [[Fn]         [&self]] $($r:tt)*) => {__build!{c0 $($r)*}};
	(b0 [$Fn:tt    [wildcard]] $($r:tt)*) => {__build!{c0 $($r)*}};

	// Forward the other fragment groups to more dedicated macros.
	(c0 $sfrags:tt $ifrags:tt) => {
		__build_struct!{$sfrags}
		__build_impls!{$ifrags}
	};
}

// This defines the struct.
macro_rules! __build_struct {
	([[unitlike        ] [$($vis:tt)*] [$name:ident] [$($par:tt)*]]) => {
		#[derive(Debug,Clone)]
		$($vis)* struct $name<$($par)*>;
	};
	([[[$($fld:item),+]] [$($vis:tt)*] [$name:ident] [$($par:tt)*]]) => {
		#[derive(Debug,Clone)]
		$($vis)* struct $name<$($par)*>($($fld),*);
	};
	([[{$($fld:item),+}] [$($vis:tt)*] [$name:ident] [$($par:tt)*]]) => {
		#[derive(Debug,Clone)]
		$($vis)* struct $name<$($par)*>{$($fld),*}
	};
}


// This generates the Fn{Mut,Once,} impls.
macro_rules! __build_impls {
	([$Fn:tt $name:tt $par:tt $slf:tt $pat:tt $Args:tt $out:tt $bnd:tt $code:tt]) => {
		__build_struct!{All $Fn
			// NOTE: code beyond this point tries to account correctly for the
			//       distinction between parameters for the impl vs those for the type.
			// - First `$par` is, as intended, the struct params.
			// - Second `$par` is intended to be the full params (struct+impl).
			// - `$bnd` is intended to be the full bounds (struct+impl).
			[$name $par $par $slf $pat $Args $out $bnd $code] // for primary impls
			[$name $par $par           $Args $out $bnd      ] // for derived impls
		}
	};
	(All FnOnce $pfrags:tt $dfrags:tt) => {
		__unbox_impls!(Primary FnOnce $pfrags)
	};
	(All FnMut $pfrags:tt $dfrags:tt) => {
		__unbox_impls!(Derived FnOnce $dfrags)
		__unbox_impls!(Primary FnMut  $pfrags)
	};
	(All Fn $pfrags:tt $dfrags:tt) => {
		__unbox_impls!(Derived FnOnce $dfrags)
		__unbox_impls!(Derived FnMut  $dfrags)
		__unbox_impls!(Primary Fn     $pfrags)
	};

	// "All of that, just for this," you ask?
	// YOU GODDAMN BET, SON.
	(Primary FnOnce [[$name:ident][$($spar:tt)*][$($ipar:tt)*][$slf:ident]
	                 [$pat:pat][$Args:ty][$out:ty][$($bnd:tt)*][$code:block]]) => {
		impl<$($ipar)*> FnOnce<$Args> for $name<$($spar)*> $($bnd)* {
			type Output = $out;

			#[allow(unused_mut)]
			extern "rust-call"
			fn call_once(mut $slf, $pat: $Args) -> $out $func
		}
	};
	(Primary FnMut  [[$name:ident][$($spar:tt)*][$($ipar:tt)*][$slf:ident]
	                 [$pat:pat][$Args:ty][$out:ty][$($bnd:tt)*][$code:block]]) => {
		impl<$($ipar)*> FnMut<$Args> for $name<$($spar)*> $($bnd)* {
			extern "rust-call"
			fn call_mut(&mut $slf, $pat: $Args) -> $out $func
		}
	};
	(Primary Fn     [[$name:ident][$($spar:tt)*][$($ipar:tt)*][$slf:ident]
	                 [$pat:pat][$Args:ty][$out:ty][$($bnd:tt)*][$code:block]]) => {
		impl<$($ipar)*> Fn<$Args> for $name<$($spar)*> $($bnd)* {
			extern "rust-call"
			fn call(&$slf, $pat: $Args) -> $out $func
		}
	};

	(Derived FnOnce [[$name:ident][$($spar:tt)*][$($ipar:tt)*]
	                 [$Args:ty][$out:ty][$($bnd:tt)*]]) => {
		impl<$($ipar)*> FnOnce<$Args> for $name<$($spar)*> $($bnd)* {
			type Output = $out;

			extern "rust-call"
			fn call_once(mut self, args: $Args) -> $out
			{ self.call_mut(args) }
		}
	};
	(Derived FnMut  [[$name:ident][$($spar:tt)*][$($ipar:tt)*]
	                 [$Args:ty][$out:ty][$($bnd:tt)*]]) => {
		impl<$($ipar)*> FnMut<$Args> for $name<$($spar)*> $($bnd)* {
			extern "rust-call"
			fn call_mut(&mut self, args: $Args) -> $out
			{ self.call(args) }
		}
	};
}

// Some examples
unbox!{
	Fn AddTen(a:i32) -> i32 { a + 10 }
}

unbox!{
	Fn AddAB(a:i32, b:i32) -> i32 { a + b }
}

unbox!{
	Fn AddConst[i32](&self, a:i32) -> i32 { a + self.0 }
}

// NOTE: T in this example should technically be an impl parameter;
//       this probably doesn't compile.
unbox!{
	Fn[T] AddGeneric(a:T, b:T) -> T
	where[T: Add] { a + b }
}

unbox!{
	FnOnce SumIt{vec: Vec<i32>}(self) -> i32
	{ self.vec.into_iter().fold(0, |a,b| a+b) }
}

// NOTE: T in this example is (correctly) a struct parameter.
unbox!{
	FnMut['a,T] Push[&'a mut Vec<T>](&mut self, x: T)
	where[T: 'a]
	{ self.0.push(x) }
}

// Example of how it could be used in a return type.
type YouCanNameThisType =
	::std::iter::Map<
		::std::vec::IntoIter<i32>,
		AddTen>;
