macro_rules! bug {
    ($($t:tt)*) => { panic!( "{}:{}: BUG in unbox macro: {}", file!(), line!(), format_args!($($t)*)) }
}

macro_rules! error {
    ($($t:tt)*) => { panic!( "unbox macro: error: {}", format_args!($($t)*)) }
}

// parse_quote with debug info
macro_rules! parse_quote {
    ($($t:tt)*)
    => {{
        let tokens: ::proc_macro2::TokenStream = quote!{$($t)*}.into();
        ::syn::parse2(tokens.clone())
            .unwrap_or_else(|e| bug!("{}\nTOKENS: {}", e, tokens))
    }};
}
