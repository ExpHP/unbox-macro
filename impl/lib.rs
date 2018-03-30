#[macro_use] extern crate proc_macro_hack;
#[macro_use] extern crate quote;
#[macro_use] extern crate syn;
#[macro_use] extern crate enum_map;
extern crate proc_macro;
extern crate proc_macro2;

use syn::{Attribute, Expr, Generics, Ident, Item, Pat, Type, Visibility};
use syn::punctuated::Punctuated;
use syn::synom::Synom;

// sorry, Token![]. Wake me up when intellij-rust can parse type macros
use syn::token::{Comma, Paren};

use quote::ToTokens;

use enum_map::EnumMap;

#[allow(unused)]
const CONFIG_ATTR: &'static str = "unbox";

proc_macro_item_impl! {
    pub fn unbox_hack_impl(input: &str) -> String {
        // (put the definition outside this macro call for the sake of IDEs)
        do_unbox(input)
    }
}

proc_macro_item_impl! {
    pub fn unbox_hack_inmod_impl(input: &str) -> String {
        // should replace Visibility::Inherited with `pub(super)`
        unimplemented!();
        //do_unbox(input)
    }
}

// parse_quote with debug info
macro_rules! parse_quote {
    ($($t:tt)*)
    => {{
        let tokens: ::proc_macro2::TokenStream = quote!{$($t)*}.into();
        ::syn::parse2(tokens.clone())
            .unwrap_or_else(|e| panic!(
                "{}:{}: BUG in unbox macro: {}\nTOKENS: {}",
                file!(), line!(), e, tokens,
            ))
    }};
}

mod parse {
    use super::*;

    // ---------

    pub(crate) struct InvocationBody {
        pub(crate) config_attrs: Vec<Attribute>,
        pub(crate) items: Vec<UnboxedItems>,
    }

    impl Synom for InvocationBody {
        named!(parse -> Self, do_parse!(
            config_attrs: many0!(call!(Attribute::parse_inner)) >>
            items: many0!(syn!(_)) >>
            (InvocationBody { config_attrs, items })
        ));
    }

    // ---------

    pub(crate) struct UnboxedItems {
        pub(crate) klass: Option<syn::ItemStruct>,
        pub(crate) impul: ItemUnboxedImpl,
    }

    impl Synom for UnboxedItems {
        named!(parse -> Self, do_parse!(
            klass: option!(syn!(_)) >>
            impul: syn!(_) >>
            (UnboxedItems { klass, impul })
        ));
    }

    impl UnboxedItems {
        pub(crate) fn determine_vis(&self) -> Visibility {
            match *self {
                UnboxedItems {
                    klass: Some(syn::ItemStruct { ref vis, .. }),
                    impul: ItemUnboxedImpl { vis: Visibility::Inherited, .. },
                } => vis.clone(),

                UnboxedItems {
                    klass: None,
                    impul: ItemUnboxedImpl { ref vis, .. }
                } => vis.clone(),

                UnboxedItems {
                    klass: Some(_),
                    impul: _
                } => panic!("\
                    unbox macro: error: visibility on the Fn item is forbidden \
                    when a struct is provided.\
                "),
            }
        }

        pub(crate) fn find_ident(&self) -> &Ident {
            let klass_ident = self.klass.as_ref().map(|klass| &klass.ident);
            let fn_ident = self.impul.ident.as_ref();
            match (klass_ident, fn_ident) {
                (Some(ident), None) => ident,
                (None, Some(ident)) => ident,
                (Some(klass_ident), Some(fn_ident)) =>{
                    assert_eq!(
                        klass_ident, fn_ident,
                        "unbox macro: error: Inconsistent names",
                    );
                    klass_ident
                },
                (None, None) => panic!("unbox macro: error: No name provided"),
            }
        }
    }


    // ---------

    pub(crate) struct ItemUnboxedImpl {
        pub(crate) vis: Visibility,
        // pub(crate) struct_spec: Option<StructSpec>,
        // pub(crate) for_spec: Option<ForSpec>,
        pub(crate) fn_spec: FnSpec,
        pub(crate) ident: Option<Ident>,
        pub(crate) generics: Generics, // includes where clause
        #[allow(unused)]
        pub(crate) paren_token: Paren,
        pub(crate) inputs: Punctuated<syn::FnArg, Comma>,
        pub(crate) return_type: syn::ReturnType,
        pub(crate) body: syn::Block,
    }

    impl Synom for ItemUnboxedImpl {
        named!(parse -> Self, do_parse!(
            attrs:   many0!(call!(Attribute::parse_outer)) >>
            _i_lied: value!({
                if !attrs.is_empty() {
                    panic!("unbox macro: error: attributes on the Fn item are not supported")
                }
            }) >>

            vis:          syn!(_) >>

            fn_spec:      syn!(_) >>
            ident:        option!(syn!(_)) >>
            generics:     syn!(_) >>

            // FIXME retarded.  Is there really no way to just pattern match?
            _toople:      parens!(call!(Punctuated::parse_terminated)) >>
            paren_token:  value!(_toople.0) >>
            inputs:       value!(_toople.1) >>
            return_type:  syn!(_) >>

            where_clause: option!(syn!(_)) >>

            body:         syn!(_) >>

            generics:     value!({
                let mut generics: Generics = generics;
                // (it came from Synom, so it shouldn't have a `where` clause)
                assert!(generics.where_clause.is_none());
                generics.where_clause = where_clause;
                generics
            }) >>

            (ItemUnboxedImpl {
                vis, fn_spec, ident, generics,
                paren_token, inputs, return_type, body,
            })
        ));
    }

    // ---------

    /// `Fn`, `FnMut`, or `FnOnce`
    pub(crate) struct FnSpec {
        #[allow(unused)]
        pub(crate) ident: Ident,
        pub(crate) kind: FnKind,
    }

    impl Synom for FnSpec {
        named!(parse -> Self, do_parse!(
            ident: syn!(_) >>
            kind:  alt!(
                cond_reduce!(ident == "Fn") => { |()| FnKind::Fn }
                |
                cond_reduce!(ident == "FnMut") => { |()| FnKind::FnMut }
                |
                cond_reduce!(ident == "FnOnce") => { |()| FnKind::FnOnce }
            ) >>
            (FnSpec { ident, kind })
        ));
    }

    // ---------

}

// `Ord` reflects capabilities provided to the caller;  `FnOnce` is the least.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[derive(EnumMap)]
enum FnKind { FnOnce, FnMut, Fn }

fn do_unbox(input: &str) -> String
{
    let tokens = input.parse().expect("couldn't parse token stream");
    let invocation = syn::parse(tokens).unwrap_or_else(|e| {
        panic!("unbox macro: syntax error: {}", e);
    });

    let mut items = vec![];
    handle_invocation(&mut items, invocation);

    quote!(#(#items)*).to_string()
}

//--------------------------------------------------------------

fn handle_invocation(
    out: &mut Vec<Item>,
    invocation: parse::InvocationBody,
) {
    let parse::InvocationBody { config_attrs, items } = invocation;
    assert_eq!(
        config_attrs.len(), 0,
        "Sorry, config attributes for unbox! are not yet implemented.",
    );

    let config = Default::default();
    for in_item in items {
        handle_one(out, &config, in_item);
    }
}

fn handle_one(
    out: &mut Vec<Item>,
    config: &Config,
    user_items: parse::UnboxedItems,
) {
    // Tear the user input apart into orthogonal pieces meaningful for output.
    // Do it in a block so useless things can fall out of scope.
    let vis = user_items.determine_vis();
    let struct_name = user_items.find_ident().clone();

    let parse::UnboxedItems {
        klass: struct_item,
        impul: parse::ItemUnboxedImpl {
            fn_spec: parse::FnSpec {
                kind,
                ident: _,
            },
            inputs,
            body,
            return_type,
            generics: fn_generics,
            vis: _, // already accounted for
            ident: _, // already accounted for
            paren_token: _,
        }
    } = user_items;

    let struct_item = struct_item.unwrap_or_else(|| {
        parse_quote!{ #vis struct #struct_name; }
    });

    let params_info = inputs.into();
    let generics_info = generics_info::Input {
        fn_generics,
        struct_generics: struct_item.generics.clone(),
    }.compute();

    out.push(struct_item.into());

    // Generate the trait impls
    for impl_kind in vec![FnKind::FnOnce, FnKind::FnMut, FnKind::Fn] {
        generate_impl_if_applicable(
            out,
            config,
            kind,
            &struct_name,
            &params_info,
            &generics_info,
            &return_type,
            &body,
            impl_kind,
        )
    }
}

//--------------------------------------------------------------

// split_for_info is useless when there are additional type parameters in the trait;
// we'll have to roll our own

use self::generics_info::GenericsInfo;
mod generics_info {
    use super::*;

    pub(crate) struct Input {
        pub(crate) fn_generics: Generics,
        pub(crate) struct_generics: Generics,
    }

    pub(crate) struct GenericsInfo {
        total_generics: Generics,
        struct_generics: Generics,
    }

    impl Input {
        pub(crate) fn compute(self) -> GenericsInfo {
            let Input { fn_generics, struct_generics } = self;
            let total_generics = merge_generics(struct_generics.clone(), fn_generics);
            GenericsInfo { total_generics, struct_generics }
        }
    }

    impl GenericsInfo {
        /// Like `Generics::split_for_impl` except the TypeGenerics do not necessarily include
        /// all of the type parameters (just those that actually belong to the type!)
        pub(crate) fn split_for_impl(
            &self,
        ) -> (syn::ImplGenerics, syn::TypeGenerics, Option<&syn::WhereClause>)
        {
            let (impl_generics, _, where_clause) = self.total_generics.split_for_impl();
            let (_, self_generics, _) = self.struct_generics.split_for_impl();
            (impl_generics, self_generics, where_clause)
        }

        /// Like the standard `GenericsInfo::split_for_impl`, without the adjustments to TypeGenerics.
        pub(crate) fn total_split_for_impl(
            &self,
        ) -> (syn::ImplGenerics, syn::TypeGenerics, Option<&syn::WhereClause>)
        { self.total_generics.split_for_impl() }
    }
}

//--------------------------------------------------------------

fn merge_generics(a: Generics, b: Generics) -> Generics {
    Generics {
        lt_token: a.lt_token.or(b.lt_token),
        gt_token: a.gt_token.or(b.gt_token),
        params: {
            let mut params = a.params;
            params.extend(b.params);
            params
        },
        where_clause: match (a.where_clause, b.where_clause) {
            (None, None) => None,
            (None, Some(c)) => Some(c),
            (Some(c), None) => Some(c),
            (Some(mut c), Some(other)) => {
                c.predicates.extend(other.predicates);
                Some(c)
            }
        },
    }
}

//--------------------------------------------------------------

pub(crate) struct ParamsInfo {
    pub(crate) self_param: Option<syn::FnArg>,
    pub(crate) args_pat: Pat,
    pub(crate) args_ty: Type,
}

impl From<Punctuated<syn::FnArg, Comma>> for ParamsInfo {
    fn from(params: Punctuated<syn::FnArg, Comma>) -> Self {
        use std::iter::once;

        let mut pairs = params.into_iter();
        let (self_param, pairs) = {
            match pairs.next() {
                None => (None, vec![]),

                Some(first) => match first {
                    first@syn::FnArg::SelfRef(_) |
                    first@syn::FnArg::SelfValue(_) => (Some(first), pairs.collect()),

                    first => (None, once(first).chain(pairs).collect()),
                },
            }
        };
        let (pats, tys): (Vec<_>, Vec<_>) = {
            pairs.into_iter()
                .map(|arg| match arg {
                    syn::FnArg::Captured(arg) => (arg.pat, arg.ty),
                    arg => panic!("unbox macro: error: bad argument: {}", arg.into_tokens()),
                })
                .unzip()
        };
        let args_pat = parse_quote!{ ( #(#pats,)* ) };
        let args_ty  = parse_quote!{ ( #(#tys,)*  ) };
        ParamsInfo { self_param, args_pat, args_ty }
    }
}

//--------------------------------------------------------------

fn generate_impl_if_applicable(
    out: &mut Vec<Item>,
    config: &Config,
    primary_kind: FnKind,
    struct_name: &Ident,
    params_info: &ParamsInfo,
    generics_info: &GenericsInfo,
    return_type: &syn::ReturnType,
    block: &syn::Block,
    impl_kind: FnKind,
) {
    use ::std::cmp::Ordering;

    let assoc_method_item = match impl_kind.cmp(&primary_kind) {
        Ordering::Less => secondary_impl_method_item(
            config, primary_kind, params_info, return_type, impl_kind,
        ),
        Ordering::Equal => primary_impl_method_item(
            config, primary_kind, params_info, generics_info, return_type, block,
        ),
        Ordering::Greater => {
            return; // the unboxed closure does not implement this trait
        },
    };

    let assoc_ty_item: Option<syn::ImplItemType> = match impl_kind {
        FnKind::FnOnce => {
            let ty = match *return_type {
                syn::ReturnType::Default => parse_quote!{ () },
                syn::ReturnType::Type(_, ref ty) => *ty.clone(),
            };
            let ident = &config.output.0;
            Some(parse_quote!{ type #ident = #ty; })
        },
        _ => None,
    };

    let (impl_generics, self_generics, where_clause) = generics_info.split_for_impl();
    let ParamsInfo { ref args_ty, .. } = *params_info;
    let impl_trait_path = &config.traits.0[impl_kind];
    out.push(parse_quote!{
        impl #impl_generics #impl_trait_path<#args_ty> for #struct_name #self_generics
        #where_clause
        {
            #assoc_ty_item

            #assoc_method_item
        }
    })
}

fn primary_impl_method_item(
    config: &Config,
    kind: FnKind,
    params_info: &ParamsInfo,
    generics_info: &GenericsInfo,
    return_type: &syn::ReturnType,
    block: &syn::Block,
) -> syn::ImplItem
{
    let impl_method_name = &config.methods.0[kind];

    let ParamsInfo { ref self_param, ref args_pat, ref args_ty } = *params_info;

    match *self_param {
        // if the user specifies a `self` arg, use it so that it gets type-checked
        Some(ref self_param) => {
            parse_quote!{
                fn #impl_method_name ( #self_param, #args_pat: #args_ty ) #return_type
                #block
            }
        },
        // Otherwise, generate a `self` arg, and make it inaccessible to the fn body
        None => {
            let self_param: syn::FnArg = match kind {
                FnKind::FnOnce => parse_quote!{ self },
                FnKind::FnMut  => parse_quote!{ &mut self },
                FnKind::Fn     => parse_quote!{ &self },
            };
            let (impl_generics, generic_args, where_clause) = generics_info.total_split_for_impl();
            let turbofish = generic_args.as_turbofish();
            parse_quote!{
                fn #impl_method_name ( #self_param, args: #args_ty ) #return_type {
                    // Simulate hygiene with an inner fn item to hide `self`.
                    // Generic parameters are shadowed with identical copies defined
                    //  on the function.  Fingers crossed, mates...
                    fn inner #impl_generics (#args_pat: #args_ty) #return_type
                    #where_clause
                    #block

                    // supply a turbofish to dodge "type annotations needed" errors
                    // which would otherwise not occur
                    // FIXME: This needs a test case...
                    inner #turbofish (args)
                }
            }
        },
    }
}

fn secondary_impl_method_item(
    config: &Config,
    delegated_kind: FnKind,
    params_info: &ParamsInfo,
    return_type: &syn::ReturnType,
    impl_kind: FnKind,
) -> syn::ImplItem
{
    let (signature_self, delegated_self): (syn::FnArg, Expr) = {
        match (impl_kind, delegated_kind) {
            (FnKind::FnMut,  FnKind::Fn) =>    (parse_quote!{ &mut self }, parse_quote!{ self }),
            (FnKind::FnOnce, FnKind::Fn) =>    (parse_quote!{ self },      parse_quote!{ &self }),
            (FnKind::FnOnce, FnKind::FnMut) => (parse_quote!{ mut self },  parse_quote!{ &mut self }),
            _ => unreachable!(),
        }
    };
    let delegated_path = config.trait_method_qualified_path(delegated_kind);

    let impl_method_name = &config.methods.0[impl_kind];
    let ParamsInfo { ref args_ty, .. } = *params_info;

    parse_quote!{
        fn #impl_method_name ( #signature_self, args: #args_ty ) #return_type
        { #delegated_path ( #delegated_self, args) }
    }
}

//--------------------------------------------------------------

#[derive(Default)]
struct Config {
    pub(crate) traits: cfg::Traits,
    pub(crate) methods: cfg::Methods,
    pub(crate) output: cfg::Output,
}

impl Config {
    /// Not full-on UFCS, but close.  (`Trait::method`)
    pub fn trait_method_qualified_path(&self, kind: FnKind) -> syn::Path
    {
        let mut path = self.traits.0[kind].clone();
        path.segments.push(self.methods.0[kind].clone().into());
        path
    }
}

mod cfg {
    use super::*;

    pub(crate) struct Traits(pub(crate) EnumMap<FnKind, syn::Path>);
    pub(crate) struct Methods(pub(crate) EnumMap<FnKind, Ident>);
    pub(crate) struct Output(pub(crate) Ident);

    impl Default for Traits {
        fn default() -> Self
        { Traits(enum_map![
            // the defaults use locally-resolved names rather than absolute paths
            // in order to reduce the need to supply this configuration.
            // (if the defaults used `::std::ops` prefixes then you'd be required
            //  to configure this in every call to have any hope of using it on stable)
            FnKind::Fn     => parse_quote!{Fn},
            FnKind::FnMut  => parse_quote!{FnMut},
            FnKind::FnOnce => parse_quote!{FnOnce},
        ])}
    }

    impl Default for Methods {
        fn default() -> Self
        { Methods(enum_map![
            FnKind::Fn     => "call".into(),
            FnKind::FnMut  => "call_mut".into(),
            FnKind::FnOnce => "call_once".into(),
        ])}
    }

    impl Default for Output {
        fn default() -> Self
        { Output(Ident::from("Output")) }
    }
}

//--------------------------------------------------------------
