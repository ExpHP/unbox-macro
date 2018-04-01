#[macro_use]
extern crate unbox;

mod nullary {
    pub trait MuOnce {
        type Output;

        fn goo_once(self) -> Self::Output;
    }

    pub trait MuMut: MuOnce {
        fn goo_mut(&mut self) -> Self::Output;
    }

    pub trait Mu: MuMut {
        fn goo(&self) -> Self::Output;
    }
}

mod unary {
    pub trait UnOnce<A> {
        type Ret;

        fn call_once(self, a: A) -> Self::Ret;
    }

    pub trait UnMut<A>: UnOnce<A> {
        fn call_mut(&mut self, a: A) -> Self::Ret;
    }

    pub trait Un<A>: UnMut<A> {
        fn call(&self, a: A) -> Self::Ret;
    }
}

mod binary {
    pub trait BinOnce<A, B> {
        type Output;

        fn zip_once(self, a: A, b: B) -> Self::Output;
    }

    pub trait BinMut<A, B>: BinOnce<A, B> {
        fn zip_mut(&mut self, a: A, b: B) -> Self::Output;
    }

    pub trait Bin<A, B>: BinMut<A, B> {
        fn zip(&self, a: A, b: B) -> Self::Output;
    }
}

unbox!{
    #![unbox(mod_scope(_nullary))]
    #![unbox(traits("::nullary::Mu", "::nullary::MuMut", "::nullary::MuOnce"))]
    #![unbox(methods("goo", "goo_mut", "goo_once"))]
    #![unbox(generics(none))]
    #![unbox(args_pat(none))]
    #![unbox(args_ty(none))]

    Fn HaveSomeMu() -> &'static str { "gai pan" }
}

#[test]
fn test_chinese_takeout() {
    assert_eq!(HaveSomeMu.goo(), "gai pan");
}

mod test_unary {
    use ::unary::{Un, UnMut, UnOnce};
    unbox!{
        #![unbox(mod_scope(_unary))]
        #![unbox(traits("::unary::Un", "::unary::UnMut", "::unary::UnOnce"))]
        #![unbox(output("Ret"))]
        #![unbox(generics(single))]
        #![unbox(args_pat(single))]
        #![unbox(args_ty(single))]

        Fn DoubleUn<T: Double>(x: T) -> T { x.double() }

        Fn ToStringUn<T: ToString>(x: T) -> String { x.to_string() }

        pub struct Of<F, G>{pub f: F, pub g: G}
        impl Fn<A, B, C>(&self, a: A) -> C
        where
            G: Un<A, Ret=B>,
            F: Un<B, Ret=C>,
        { self.f.call(self.g.call(a)) }
    }

    pub trait Double {
        fn double(self) -> Self;
    }
    impl Double for String {
        fn double(self) -> Self { self.clone() + &self }
    }
    impl Double for i32 {
        fn double(self) -> Self { self + self }
    }

    #[test]
    fn test_un() {
        assert_eq!(Of(DoubleUn, ToStringUn).call(12), "1212");
        assert_eq!(Of(ToStringUn, DoubleUn).call(12), "24");
    }
}

