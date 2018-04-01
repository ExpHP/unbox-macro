#[macro_use] extern crate proc_macro_hack;
#[macro_use] extern crate quote;
#[macro_use] extern crate syn;
#[macro_use] extern crate enum_map;
extern crate strfmt;
extern crate proc_macro;
extern crate proc_macro2;

use self::cfg::Config;

use syn::{Attribute, Expr, FnArg, Generics, Ident, Item, Pat, Type, Visibility};
use syn::punctuated::Punctuated;
use syn::synom::Synom;

// sorry, Token![]. Wake me up when intellij-rust can parse type macros
use syn::token::Comma;

use quote::ToTokens;

proc_macro_item_impl! {
    pub fn unbox_hack_impl(input: &str) -> String {
        do_unbox(false, input)
    }
}

proc_macro_item_impl! {
    pub fn unbox_hack_inmod_impl(input: &str) -> String {
        do_unbox(true, input)
    }
}

#[macro_use]
mod macros;
mod cfg;
mod util;

mod parse {
    use super::*;

    // ---------

    pub(crate) struct InvocationBody {
        pub(crate) config_attrs: Vec<Attribute>,
        pub(crate) items: Vec<UnboxedItemsKind>,
    }

    impl Synom for InvocationBody {
        named!(parse -> Self, do_parse!(
            config_attrs: many0!(call!(Attribute::parse_inner)) >>
            items: many0!(syn!(_)) >>
            (InvocationBody { config_attrs, items })
        ));
    }

    // ---------

    pub(crate) enum UnboxedItemsKind {
        Shuga(UnboxedItemsShuga),
        Spyce(UnboxedItemsSpyce),
    }

    impl Synom for UnboxedItemsKind {
        named!(parse -> Self, alt!(
            syn!(_) => { UnboxedItemsKind::Shuga }
            |
            syn!(_) => { UnboxedItemsKind::Spyce }
        ));
    }

    // ---------

    pub(crate) struct UnboxedItemsShuga {
        // `Fn` item
        pub(crate) attrs: Vec<Attribute>,
        pub(crate) vis: Visibility,
        pub(crate) fn_spec: FnSpec,
        pub(crate) ident: Ident,
        pub(crate) generics: Generics, // includes where clause
        #[allow(unused)]
        pub(crate) paren_token: syn::token::Paren,
        pub(crate) inputs: Punctuated<FnArg, Comma>,
        pub(crate) return_type: syn::ReturnType,
        pub(crate) body: syn::Block,
    }

    pub(crate) struct UnboxedItemsSpyce {
        pub(crate) struct_item: syn::ItemStruct,

        // impl item
        pub(crate) attrs: Vec<Attribute>,
        #[allow(unused)]
        pub(crate) impl_token: syn::token::Impl,
        #[allow(unused)]
        pub(crate) generics: Generics, // includes where clause
        pub(crate) fn_spec: FnSpec,
        #[allow(unused)]
        pub(crate) paren_token: syn::token::Paren,
        pub(crate) inputs: Punctuated<FnArg, Comma>,
        pub(crate) return_type: syn::ReturnType,
        pub(crate) body: syn::Block,
    }

    impl Synom for UnboxedItemsShuga {
        named!{parse -> Self, do_parse!(
            attrs:        many0!(call!(Attribute::parse_outer)) >>

            vis:          syn!(_) >>

            fn_spec:      syn!(_) >>
            ident:        syn!(_) >>
            generics:     syn!(_) >>

            // FIXME retarded.  Is there really no way to just pattern match?
            _toople:      parens!(call!(Punctuated::parse_terminated)) >>
            paren_token:  value!(_toople.0) >>
            inputs:       value!(_toople.1) >>
            return_type:  syn!(_) >>

            where_clause: option!(syn!(_)) >>

            body:         syn!(_) >>

            generics:     value!(supply_missing_where(generics, where_clause)) >>

            (UnboxedItemsShuga {
                attrs, vis, fn_spec, ident, generics,
                paren_token, inputs, return_type, body,
            })
        )}
    }

    impl Synom for UnboxedItemsSpyce {
        named!{parse -> Self, do_parse!(
            struct_item:  syn!(_) >>

            attrs:        many0!(call!(Attribute::parse_outer)) >>

            impl_token:   syn!(_) >>

            fn_spec:      syn!(_) >>

            generics:     syn!(_) >>

            // FIXME retarded.  Is there really no way to just pattern match?
            _toople:      parens!(call!(Punctuated::parse_terminated)) >>
            paren_token:  value!(_toople.0) >>
            inputs:       value!(_toople.1) >>
            return_type:  syn!(_) >>

            where_clause: option!(syn!(_)) >>

            body:         syn!(_) >>

            generics:     value!(supply_missing_where(generics, where_clause)) >>

            (UnboxedItemsSpyce {
                struct_item, attrs, impl_token, generics,
                fn_spec, paren_token, inputs, return_type, body,
            })
        )}
    }

    fn supply_missing_where(
        generics_from_synom: Generics,
        where_clause: Option<syn::WhereClause>,
    ) -> Generics
    {
        let mut generics = generics_from_synom;
        // generics from synom never have a where clause
        assert!(generics.where_clause.is_none());
        generics.where_clause = where_clause;
        generics
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

//--------------------------------------------------------------

// `Ord` reflects capabilities provided to the caller;  `FnOnce` is the least.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[derive(EnumMap)]
enum FnKind { FnOnce, FnMut, Fn }

fn do_unbox(is_inmod: bool, input: &str) -> String
{
    let tokens = input.parse().expect("couldn't parse token stream");
    let invocation = syn::parse(tokens).unwrap_or_else(|e| error!("{}", e));

    let mut items = vec![];
    handle_invocation(&mut items, is_inmod, invocation);

    quote!(#(#items)*).to_string()
}

//--------------------------------------------------------------

fn handle_invocation(
    out: &mut Vec<Item>,
    is_inmod: bool,
    invocation: parse::InvocationBody,
) {
    let parse::InvocationBody { config_attrs, items } = invocation;
    let config = Config::from_inner_attrs(&config_attrs);
    for in_item in items {
        handle_one(out, &config, is_inmod, in_item);
    }
}

fn handle_one(
    out: &mut Vec<Item>,
    config: &Config,
    is_inmod: bool,
    user_items: parse::UnboxedItemsKind,
) {

    let panic_on_attrs = |name, attrs: &[_]| {
        if !attrs.is_empty() {
            error!("attributes on {} are not supported", name);
        }
    };

    let stuff = match user_items {
        parse::UnboxedItemsKind::Shuga(
            parse::UnboxedItemsShuga {
                generics: impl_generics,
                attrs, vis, ident,
                fn_spec, inputs, return_type, body,
                ..
            }
        ) => {
            panic_on_attrs("a Fn item", &attrs);
            let struct_item = parse_quote!{ #vis struct #ident; };
            (struct_item, impl_generics, fn_spec, inputs, return_type, body)
        },
        parse::UnboxedItemsKind::Spyce(
           parse::UnboxedItemsSpyce {
               generics: impl_generics,
               attrs, struct_item,
               fn_spec, inputs, return_type, body,
               ..
           }
        ) => {
            panic_on_attrs("the impl item", &attrs);
            (struct_item, impl_generics, fn_spec, inputs, return_type, body)
        },
    };
    let (struct_item, impl_generics, fn_spec, inputs, return_type, body) = stuff;

    let struct_item = match is_inmod {
        true => inmod::fix_visibilities(struct_item),
        false => struct_item,
    };

    let params_info = ParamsInfo::from_params(config, inputs);
    let generics_info = generics_info::Input {
        impl_generics,
        struct_generics: struct_item.generics.clone(),
    }.compute();

    let struct_name = struct_item.ident.clone();

    out.push(struct_item.into());

    // Generate the trait impls
    for impl_kind in vec![FnKind::FnOnce, FnKind::FnMut, FnKind::Fn] {
        generate_impl_if_applicable(
            out,
            config,
            fn_spec.kind,
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
        pub(crate) impl_generics: Generics,
        pub(crate) struct_generics: Generics,
    }

    pub(crate) struct GenericsInfo {
        total_generics: Generics,
        struct_generics: Generics,
    }

    impl Input {
        pub(crate) fn compute(self) -> GenericsInfo {
            let Input { impl_generics, struct_generics } = self;
            let total_generics = merge_generics(struct_generics.clone(), impl_generics);
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
    pub(crate) self_param: Option<FnArg>,
    pub(crate) trait_args: Vec<Type>,
    pub(crate) args_pats: Vec<Pat>,
    pub(crate) args_tys: Vec<Type>,
}

impl ParamsInfo {
    fn from_params(config: &Config, params: Punctuated<FnArg, Comma>) -> Self {
        use std::iter::once;

        let mut pairs = params.into_iter();
        let (self_param, pairs) = {
            match pairs.next() {
                None => (None, vec![]),

                Some(first) => match first {
                    first@FnArg::SelfRef(_) |
                    first@FnArg::SelfValue(_) => (Some(first), pairs.collect()),

                    first => (None, once(first).chain(pairs).collect()),
                },
            }
        };
        let (arg_pats, arg_tys): (Vec<_>, Vec<_>) = {
            pairs.into_iter()
                .map(|arg| match arg {
                    FnArg::Captured(arg) => (arg.pat, arg.ty),
                    arg => error!("bad argument: {}", arg.into_tokens()),
                })
                .unzip()
        };
        let trait_generics = config.generics.gen_tys(&arg_tys);
        let args_tys = config.args_ty.gen_tys(&arg_tys);
        let args_pats = config.args_pat.gen_pats(&arg_pats);
        if args_tys.len() != args_pats.len() {
            error!(
                "The specified configs for '{}' and '{}' produced different numbers of arguments!",
                cfg::CFG_NODE_ARGS_TY, cfg::CFG_NODE_ARGS_PAT,
            )
        }
        ParamsInfo { self_param, trait_args: trait_generics, args_pats, args_tys }
    }
}

//--------------------------------------------------------------

mod inmod {
    use super::*;

    /// Updates visibilities in `inmod` to work as if the nested module were not there.
    ///
    /// This is NOT idempotent.
    pub(crate) fn fix_visibilities(item: syn::ItemStruct) -> syn::ItemStruct {
        use ::syn::fold::Fold;

        struct Folder;
        impl Fold for Folder {
            fn fold_visibility(&mut self, i: Visibility) -> Visibility { update_visibility(i) }

            fn fold_item_mod(&mut self, _i: syn::ItemMod) -> syn::ItemMod {
                // I guess inner modules should have visibilites reinterpreted as relative to the
                // path where they appear after reexport. Unless... uh, I don't know.
                panic!("\
                    unbox! does not support nested modules in the struct item, \
                    but bravo on making this error message appear!\
                ")
            }
        }

        Folder.fold_item_struct(item)
    }

    fn update_visibility(vis: Visibility) -> Visibility {
        match vis {
            Visibility::Inherited => parse_quote!{ pub(super) },
            Visibility::Restricted(mut vis) => {
                let mut path = vis.path;
                vis.path = match path.global() {
                    true => path,
                    false => {
                        let old = ::std::mem::replace(&mut path.segments, Default::default());

                        // simplified from a lengthier but equivalent implementation;
                        // there's a surprising number of edge cases hidden in here,
                        // but I'll let the unit tests speak for themselves.
                        path.segments.push(parse_quote!{super});
                        path.segments.extend(old.into_iter().filter(|seg| seg.ident != "self"));
                        path
                    },
                };
                // `pub(super::super)` without `in` is invalid
                vis.in_token.get_or_insert_with(Default::default);
                vis.into()
            },
            vis@Visibility::Public(_) |
            vis@Visibility::Crate(_) => vis,
        }
    }

    #[test]
    fn test_update_visibility() {
        macro_rules! vis { ($($t:tt)*) => { let v: Visibility = parse_quote!{$($t)*}; v }; }
        let f = update_visibility;
        assert_eq!(f(vis!{pub(self)}),          vis!{pub(in super)});
        assert_eq!(f(vis!{pub(in self)}),       vis!{pub(in super)});
        assert_eq!(f(vis!{pub(in self::a::b)}), vis!{pub(in super::a::b)});

        assert_eq!(f(vis!{pub(super)}),           vis!{pub(in super::super)});
        assert_eq!(f(vis!{pub(in super)}),        vis!{pub(in super::super)});
        assert_eq!(f(vis!{pub(in super::a)}),     vis!{pub(in super::super::a)});
        assert_eq!(f(vis!{pub(in super::super)}), vis!{pub(in super::super::super)});

        assert_eq!(f(vis!{pub(in a::b)}),   vis!{pub(in a::b)});
        assert_eq!(f(vis!{pub(in ::a::b)}), vis!{pub(in a::b)});

        assert_eq!(f(vis!{}),           vis!{pub(super)});
        assert_eq!(f(vis!{pub(crate)}), vis!{pub(crate)});
        assert_eq!(f(vis!{pub}),        vis!{pub});
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
    let ParamsInfo { ref trait_args, .. } = *params_info;
    let impl_trait_path = &config.traits.0[impl_kind];
    out.push(parse_quote!{
        impl #impl_generics #impl_trait_path<#(#trait_args),*> for #struct_name #self_generics
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

    let ParamsInfo { ref self_param, ref args_pats, ref args_tys, trait_args: _ } = *params_info;

    // args with patterns as the user wrote them...
    let true_args = {
        if args_pats.len() != args_tys.len() {
            bug!("mismatch in args count for pats/tys (shouldn't this have been caught earlier?)");
        }
        args_pats.iter().zip(args_tys.iter())
            .map(|(pat, ty)| syn::ArgCaptured {
                pat: pat.clone(),
                ty: ty.clone(),
                colon_token: Default::default(),
            }).collect::<Vec<_>>()
    };

    match *self_param {
        // if the user specifies a `self` arg, use it so that it gets type-checked
        Some(ref self_param) => {
            parse_quote!{
                fn #impl_method_name ( #self_param #(,#true_args)* ) #return_type
                #block
            }
        },
        // Otherwise, generate a `self` arg, and make it inaccessible to the fn body
        None => {
            let self_param: FnArg = match kind {
                FnKind::FnOnce => parse_quote!{ self },
                FnKind::FnMut  => parse_quote!{ &mut self },
                FnKind::Fn     => parse_quote!{ &self },
            };

            // total_split_for_impl surprisingly carries its weight here;
            // this trick declares a fn item with all of the type params,
            //   and to call it we need a turbofish with all of the params.
            let (impl_generics, generic_args, where_clause) = generics_info.total_split_for_impl();
            let turbofish = generic_args.as_turbofish();

            // args with simple ident patterns (and expressions) for forwarding
            let (signature_args, delegated_args) = unhygienic_args_from_types(args_tys);

            parse_quote!{
                fn #impl_method_name ( #self_param #(, #signature_args)* ) #return_type {
                    // Simulate hygiene with an inner fn item to hide `self`.
                    // Generic parameters are shadowed with identical copies defined
                    //  on the function.  Fingers crossed, mates...
                    fn inner #impl_generics ( #(#true_args),* ) #return_type
                    #where_clause
                    #block

                    // supply a turbofish to dodge "type annotations needed" errors
                    // FIXME: This needs a test case...
                    inner #turbofish ( #(#delegated_args),* )
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
    let (signature_self, delegated_self): (FnArg, Expr) = {
        match (impl_kind, delegated_kind) {
            (FnKind::FnMut,  FnKind::Fn) =>    (parse_quote!{ &mut self }, parse_quote!{ self }),
            (FnKind::FnOnce, FnKind::Fn) =>    (parse_quote!{ self },      parse_quote!{ &self }),
            (FnKind::FnOnce, FnKind::FnMut) => (parse_quote!{ mut self },  parse_quote!{ &mut self }),
            _ => unreachable!(),
        }
    };
    let delegated_path = config.trait_method_qualified_path(delegated_kind);

    let impl_method_name = &config.methods.0[impl_kind];
    let ParamsInfo { ref args_tys, .. } = *params_info;

    let (signature_args, delegated_args) = unhygienic_args_from_types(args_tys);

    parse_quote!{
        fn #impl_method_name ( #signature_self #(,#signature_args)* ) #return_type
        { #delegated_path ( #delegated_self #(,#delegated_args)*) }
    }
}

// suitable for use only where nothing else could possibly see the bindings.
fn unhygienic_args_from_types(types: &[Type]) -> (Vec<FnArg>, Vec<Expr>) {
    let prefixes = &["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];
    let suffixes = &["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];

    let max_len = prefixes.len() * suffixes.len();
    if max_len < types.len() {
        error!(
            "Your method has {} arguments.  That's an awful lot of arguments, right?\
            Please try to bring it down to... say... around {}.",
            types.len(), max_len,
        )
    }

    let idents: Vec<_> = {
        prefixes.iter()
            .flat_map(|p| suffixes.iter().map(move |s| Ident::from(format!("{}{}", p, s))))
            .take(types.len())
            .collect()
    };
    let exprs = idents.iter().map(|id| parse_quote!(#id)).collect();
    let fn_args = {
        idents.iter()
            .zip(types)
            .map(|(name, ty)| {
                syn::ArgCaptured {
                    pat: syn::PatIdent {
                        ident: name.clone(),
                        by_ref: None,
                        subpat: None,
                        mutability: None,
                    }.into(),
                    colon_token: Default::default(),
                    ty: ty.clone()
                }.into()
            })
            .collect()
    };
    (fn_args, exprs)
}

//--------------------------------------------------------------
