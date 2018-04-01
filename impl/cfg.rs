use ::FnKind;
use ::util::ShowToks;

use ::std::collections::HashMap;
use ::syn::{self, Attribute, Meta, NestedMeta, Ident, Pat, Type, Lit};
use ::syn::synom::Synom;
use ::syn::punctuated::Punctuated;
use ::syn::token::Comma;
use ::proc_macro2::TokenStream;
use ::quote::{Tokens, ToTokens};
use ::enum_map::EnumMap;


#[derive(Default)]
pub(crate) struct Config {
    /// Specifies the traits to be implemented.
    pub(crate) traits: Traits,
    /// Specifies the associated method names.
    pub(crate) methods: Methods,
    /// Specifies the associated Output type.
    pub(crate) output: Output,
    /// Formatting scheme to produce the trait type arguments from the function args.
    pub(crate) generics: Args,
    /// Formatting scheme to produce the associated method arg pats from the function args.
    pub(crate) args_pat: Args,
    /// Formatting scheme to produce the associated method arg types from the function args.
    pub(crate) args_ty: Args,
}

pub(crate) struct Traits(pub(crate) EnumMap<FnKind, syn::Path>);
pub(crate) struct Methods(pub(crate) EnumMap<FnKind, Ident>);
pub(crate) struct Output(pub(crate) Ident);

pub(crate) const ATTR: &'static str = "ATTR";

pub(crate) const CFG_NODE_TRAITS: &'static str = "traits";
pub(crate) const CFG_NODE_METHODS: &'static str = "methods";
pub(crate) const CFG_NODE_OUTPUT: &'static str = "output";
pub(crate) const CFG_NODE_ARGS_PAT: &'static str = "args_pat";
pub(crate) const CFG_NODE_ARGS_TY: &'static str  = "args_ty";
pub(crate) const CFG_NODE_GENERICS: &'static str = "generics";

pub(crate) const ARGS_STYLE_NONE: &'static str = "none";
pub(crate) const ARGS_STYLE_SINGLE: &'static str = "single";
pub(crate) const ARGS_STYLE_MULTIPLE: &'static str = "multiple";
pub(crate) const ARGS_STYLE_TUPLE: &'static str = "tuple";
pub(crate) const ARGS_STYLE_FORMATTED: &'static str = "formatted";
#[allow(unused)]
pub(crate) const ARGS_STYLES: &'static [&'static str] = &[
    ARGS_STYLE_NONE,
    ARGS_STYLE_SINGLE,
    ARGS_STYLE_MULTIPLE,
    ARGS_STYLE_TUPLE,
    ARGS_STYLE_FORMATTED,
];

pub(crate) const FORMATTED_KW_DELIM: &'static str = "delim";
pub(crate) const FORMATTED_KW_FORMAT: &'static str = "fmt";
pub(crate) const FORMATTED_KW_STYLE: &'static str = "style";
#[allow(unused)]
pub(crate) const FORMATTED_KWS: &'static [&'static str] = &[
    FORMATTED_KW_DELIM,
    FORMATTED_KW_FORMAT,
    FORMATTED_KW_STYLE,
];

pub(crate) const DELIM_STYLE_TUPLE: &'static str = "tuple";
pub(crate) const DELIM_STYLE_SEPARATED: &'static str = "separated";
pub(crate) const DELIM_STYLE_TERMINATED: &'static str = "terminated";
#[allow(unused)]
pub(crate) const DELIM_STYLES: &'static [&'static str] = &[
    DELIM_STYLE_TUPLE,
    DELIM_STYLE_SEPARATED,
    DELIM_STYLE_TERMINATED,
];

impl Config {
    /// Not full-on UFCS, but close.  (`Trait::method`)
    pub(crate) fn trait_method_qualified_path(&self, kind: FnKind) -> syn::Path
    {
        let mut path = self.traits.0[kind].clone();
        path.segments.push(self.methods.0[kind].clone().into());
        path
    }

    pub(crate) fn from_inner_attrs(attrs: &[Attribute]) -> Config {
        let mut traits = Default::default();
        let mut methods = Default::default();
        let mut args_pat = Default::default();
        let mut args_ty  = Default::default();
        let mut generics = Default::default();
        let mut output = Default::default();
        for attr in attrs {
            if path_eq(&attr.path, &syn::Path::from(Ident::from(ATTR))) {
                error!("Only #![{}(...)] attributes are supported at the beginning of unbox!{{}}", ATTR);
            }
            let meta = attr.interpret_meta()
                .unwrap_or_else(|| error!("invalid syntax in config attribute: {}", ShowToks(attr)));

            for meta in expect_meta_list(&attr, meta) {
                match meta {
                    NestedMeta::Literal(lit) => {
                        error!(
                            "did not expect to see the literal {} at toplevel in a config attribute",
                            ShowToks(lit),
                        );
                    },
                    NestedMeta::Meta(meta) => match meta.name().as_ref() {
                        CFG_NODE_TRAITS   => { traits   = Traits::read(attr, meta); },
                        CFG_NODE_METHODS  => { methods  = Methods::read(attr, meta); },
                        CFG_NODE_OUTPUT   => { output   = Output::read(attr, meta); },
                        CFG_NODE_ARGS_PAT => { args_pat = Args::read(attr, meta); },
                        CFG_NODE_ARGS_TY  => { args_ty  = Args::read(attr, meta); },
                        CFG_NODE_GENERICS => { generics = Args::read(attr, meta); },
                        _ => {},
                    },
                }
            }
        }
        Config { traits, methods, args_pat, args_ty, generics, output }
    }
}

impl Traits {
    fn read(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), CFG_NODE_TRAITS, "bug");
        let die = || {
            error!(r#"\
                config attr {conf} expects 3 literal strings containing paths, \
                e.g. #![{main}({conf}("Func", "FuncMut", "FuncOnce"))]\
                "#, main = ATTR, conf = CFG_NODE_TRAITS,
            );
        };
        let metas = expect_meta_list(enclosing_attr, meta);
        if metas.len() != 3 {
            die();
        }

        let mut items = metas.into_iter().map(|meta| parse_lit_str_or_else(meta, || die()));
        Traits(KindMapData {
            for_ref:  items.next().unwrap(),
            for_mut:  items.next().unwrap(),
            for_once: items.next().unwrap(),
        }.into())
    }
}

impl Methods {
    fn read(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), CFG_NODE_METHODS, "bug");
        let die = || {
            error!(r#"\
                config attr {conf} expects 3 literal strings containing idents, \
                e.g. #![{main}({conf}("call", "call_mut", "call_once"))]\
                "#, main = ATTR, conf = CFG_NODE_METHODS,
            );
        };
        let metas = expect_meta_list(enclosing_attr, meta);
        if metas.len() != 3 {
            die();
        }

        let mut items = metas.into_iter().map(|meta| parse_lit_str_or_else(meta, || die()));
        Methods(KindMapData {
            for_ref:  items.next().unwrap(),
            for_mut:  items.next().unwrap(),
            for_once: items.next().unwrap(),
        }.into())
    }
}

impl Output {
    fn read(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), CFG_NODE_OUTPUT, "bug");
        let die = || {
            error!(r#"\
                config attr {conf} expects a literal string containing an ident, \
                e.g. #![{main}({conf}("Output"))]\
                "#, main = ATTR, conf = CFG_NODE_OUTPUT,
            );
        };
        let metas = expect_meta_list(enclosing_attr, meta);
        if metas.len() != 1 {
            die();
        }
        Output(parse_lit_str_or_else(metas.into_iter().next().unwrap(), die))
    }
}

impl Args {
    fn read(enclosing_attr: &Attribute, meta: Meta) -> Self {
        let node_name = meta.name();
        let nested = expect_meta_list(enclosing_attr, meta);
        if nested.len() != 1 {
            error!(
                "config attr {conf} expects a single meta",
                conf = node_name,
            );
        }
        let only = nested.into_iter().next().unwrap();

        let die_bad_choice = || {
            error!(
                "first token inside #![{attr}({conf}(...))]) must be one of: [{choices}]",
                attr = ATTR, conf = node_name,
                choices = comma_join(ARGS_STYLES),
            );
        };

        match only {
            NestedMeta::Meta(meta) => match meta.name().as_ref() {
                ARGS_STYLE_NONE => Args::inner_read_none(enclosing_attr, meta),
                ARGS_STYLE_SINGLE => Args::inner_read_single(enclosing_attr, meta),
                ARGS_STYLE_MULTIPLE => Args::inner_read_multiple(enclosing_attr, meta),
                ARGS_STYLE_TUPLE => Args::inner_read_tuple(enclosing_attr, meta),
                ARGS_STYLE_FORMATTED => Args::inner_read_formatted(enclosing_attr, meta),
                _ => die_bad_choice(),
            },
            NestedMeta::Literal(_) => die_bad_choice(),
        }
    }

    fn inner_read_none(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), ARGS_STYLE_NONE, "bug");
        expect_meta_is_word(enclosing_attr, meta);
        Args::AsWritten(Some(0))
    }

    fn inner_read_single(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), ARGS_STYLE_SINGLE, "bug");
        expect_meta_is_word(enclosing_attr, meta);
        Args::AsWritten(Some(1))
    }

    fn inner_read_multiple(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), ARGS_STYLE_MULTIPLE, "bug");
        expect_meta_is_word(enclosing_attr, meta);
        Args::AsWritten(None)
    }

    fn inner_read_tuple(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), ARGS_STYLE_TUPLE, "bug");
        expect_meta_is_word(enclosing_attr, meta);
        Args::new_tuple()
    }

    fn inner_read_formatted(enclosing_attr: &Attribute, meta: Meta) -> Self {
        assert_eq!(meta.name(), ARGS_STYLE_FORMATTED, "bug");
        let mut kvs = KvList::from_meta_kv_list(enclosing_attr, meta);
        let format = kvs.pop_required_str(FORMATTED_KW_FORMAT).value();
        let delim = {
            kvs.pop_optional_str(FORMATTED_KW_DELIM)
                .map(|lit| lit.value())
                .unwrap_or_else(|| ",".into())
        };
        let style = {
            kvs.pop_optional_str(FORMATTED_KW_STYLE)
                .map(|lit| match lit.value().as_ref() {
                    DELIM_STYLE_SEPARATED => DelimStyle::Separated,
                    DELIM_STYLE_TERMINATED => DelimStyle::Terminated,
                    DELIM_STYLE_TUPLE => DelimStyle::Tuple,
                    style => {
                        error!(
                            "unrecognized args delimiter style '{}'. Choices are: [{}]",
                            style, comma_join(DELIM_STYLES),
                        );
                    }
                })
                .unwrap_or(DelimStyle::Separated)
        };
        let _ = kvs.expect_empty();

        Args::Formatted { format, delim, style }
    }
}

// (more convenient for EnumMap construction if you want to use impure
//  expressions and ensure they run in the order they are written)
struct KindMapData<T> {
    for_ref: T,
    for_mut: T,
    for_once: T,
}

impl<T> Into<EnumMap<FnKind, T>> for KindMapData<T> {
    fn into(self) -> EnumMap<FnKind, T> {
        let mut for_ref = Some(self.for_ref);
        let mut for_mut = Some(self.for_mut);
        let mut for_once = Some(self.for_once);
        enum_map![
            FnKind::Fn     => for_ref.take().unwrap(),
            FnKind::FnMut  => for_mut.take().unwrap(),
            FnKind::FnOnce => for_once.take().unwrap(),
        ]
    }
}

fn expect_meta_list(enclosing_attr: &Attribute, meta: Meta) -> Punctuated<NestedMeta, Comma> {
    match meta {
        Meta::List(meta) => meta.nested,
        meta => error!(
            "expected open paren after '{}' in: {}",
            ShowToks(meta.name()), ShowToks(enclosing_attr),
        ),
    }
}

fn expect_meta_kv(enclosing_attr: &Attribute, meta: Meta) -> syn::MetaNameValue {
    match meta {
        Meta::NameValue(meta) => meta,
        meta => error!(
            "expected '=' token after '{}' in: {}",
            ShowToks(meta.name()), ShowToks(enclosing_attr),
        ),
    }
}

fn expect_meta_is_word(enclosing_attr: &Attribute, meta: Meta) {
    match meta {
        Meta::Word(_) => {},
        meta => error!(
            "expected closing parenthesis after '{}' in: {}",
            ShowToks(meta.name()), ShowToks(enclosing_attr),
        ),
    }
}

fn parse_lit_str_or_else<T: Synom, F: FnMut() -> T>(meta: NestedMeta, mut die: F) -> T {
    match meta {
        NestedMeta::Literal(lit) => match lit {
            Lit::Str(lit) => lit.parse().unwrap_or_else(|_| die()),
            _ => die(),
            }
        _ => die(),
    }
}

// -----------------------------------

struct KvList {
    map: HashMap<Ident, Lit>,
    node_name: Ident,
}

impl KvList {
    fn from_meta_kv_list(enclosing_attr: &Attribute, meta: Meta) -> Self {
        let node_name = meta.name();
        let mut pairs = {
            expect_meta_list(enclosing_attr, meta).into_iter()
                .map(|m| match m {
                    NestedMeta::Literal(lit) => {
                        error!(
                            "unexpected literal {} in '{}' config node",
                            ShowToks(lit), node_name,
                        );
                    }
                    NestedMeta::Meta(m) => m,
                })
                .map(|m| expect_meta_kv(enclosing_attr, m))
                .map(|m| (m.ident, m.lit))
                .collect::<Vec<_>>()
        };

        pairs.sort_by(|a, b| a.0.cmp(&b.0));
        for (a, b) in pairs.iter().zip(pairs[1..].iter()) {
            if a.0 == b.0 {
                error!(
                    "key {} specified multiple times in '{}' config node",
                    ShowToks(a.0), node_name,
                )
            }
        }
        let map = pairs.into_iter().collect();
        KvList { map, node_name }
    }

    fn pop_required_str(&mut self, key: &str) -> syn::LitStr {
        self.pop_optional_str(key).unwrap_or_else(|| {
            error!(
                "missing required key '{}' in '{}' config node",
                key, self.node_name,
            );
        })
    }

    fn pop_optional_str(&mut self, key: &str) -> Option<syn::LitStr> {
        self.map.remove(&Ident::from(key)).map(|lit| match lit {
            Lit::Str(lit) => lit,
            lit@_ => error!(
                "value of '{}' in '{}' config node should be a string, not {}",
                key, self.node_name, ShowToks(lit),
            ),
        })
    }

    fn expect_empty(self) {
        if let Some((key, _)) = self.map.into_iter().next() {
            error!(
                "unrecognized key '{}' in '{}' config node",
                key, self.node_name,
            )
        }
    }
}


// -----------------------------------

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

impl Default for Args {
    fn default() -> Self
    { Self::new_tuple() }
}

// -----------------------------------

pub(crate) enum Args {
    // optional checked size
    AsWritten(Option<usize>),
    Formatted {
        delim: String,
        format: String,
        style: DelimStyle,
    },
}

pub(crate) enum DelimStyle {
    Separated,
    Terminated,
    Tuple,
}

impl Args {
    fn new_tuple() -> Self
    { Args::Formatted {
        delim: ",".into(),
        format: "({args})".into(),
        style: DelimStyle::Tuple,
    }}

    pub(crate) fn gen_tys(&self, tys: &[Type]) -> Vec<Type>
    { self.gen("type", tys) }

    pub(crate) fn gen_pats(&self, pats: &[Pat]) -> Vec<Pat>
    { self.gen("pat", pats) }

    fn gen<T: Synom + ToTokens>(&self, nt_name: &str, tys: &[T]) -> Vec<T> {
        match *self {
            Args::AsWritten(check_size) => {
                if let Some(check_size) = check_size {
                    if check_size != tys.len() {
                        error!("the chosen arg-style only supports {}-arg functions", check_size);
                    }
                }
                tys.iter().map(|t| parse_quote!(#t)).collect()
            },
            Args::Formatted { ref delim, ref style, ref format }=> {
                // (it'd be nice to use syn's Punctuated, but we can't since the delimiter
                //  is only known at runtime)

                let delimited = {
                    let delim_as_tokens: TokenStream = {
                        ::syn::parse_str(&delim)
                            .expect("unbox error: Could not parse separator as tokens")
                    };
                    let mut s = Tokens::new();
                    to_tokens_sep(&mut s, &delim_as_tokens, tys);

                    let terminate = match *style {
                        DelimStyle::Separated => false,
                        DelimStyle::Terminated => true,
                        DelimStyle::Tuple => tys.len() == 1,
                    };
                    if terminate {
                        delim_as_tokens.to_tokens(&mut s);
                    }
                    s.to_string()
                };

                let mut vars = vec![
                    ("args".into(), delimited)
                ].into_iter().collect();

                // FIXME: aaaand there goes all of our span info.
                // (not that we *could* use it currently, being a proc-macro-hack macro;
                //  but it's I think it's nice to be prepared...)
                let formatted = ::strfmt::strfmt(&format, &vars)
                    .unwrap_or_else(|e| error!("could not format args {}: {}", nt_name, e));

                let only_arg = ::syn::parse_str(&formatted)
                    .unwrap_or_else(|_| bug!("could not reparse args {} as tokens", nt_name));

                vec![only_arg]
            },
        }
    }
}

fn to_tokens_sep<A, B, As>(tokens: &mut Tokens, sep: B, items: As)
where
    A: ToTokens,
    B: ToTokens,
    As: IntoIterator<Item=A>,
{
    let mut items = items.into_iter();
    match items.next() {
        Some(first) => first.to_tokens(tokens),
        None => return,
    }
    for item in items {
        sep.to_tokens(tokens);
        item.to_tokens(tokens);
    }
}

fn comma_join<Ts>(iter: Ts) -> String
where
    Ts: IntoIterator,
    Ts::Item: ToString,
{
    iter.into_iter()
        .map(|x| x.to_string())
        .fold(String::new(), |acc, s| { acc + ", " + &s[..] })
}

fn path_eq(a: &syn::Path, b: &syn::Path) -> bool
{
    // FIXME
    // Something is either very wrong, or I am REALLY tired.
    // docs.rs very clearly says that syn::Path implements PartialEq
    //  in the version of syn that I am using, so why on earth does the
    //  compiler complain that it does not?!
    //
    // a == b // "binary operation '==' cannot be applied to syn::Path"

    a.into_tokens() == b.into_tokens()
}
