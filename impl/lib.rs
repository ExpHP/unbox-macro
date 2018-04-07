extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro_derive(Derp)]
pub fn derp(_: TokenStream) -> TokenStream {
    "".parse().unwrap()
}
