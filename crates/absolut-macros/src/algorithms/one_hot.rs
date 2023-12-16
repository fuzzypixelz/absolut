extern crate proc_macro;

use std::{
    collections::{HashMap, HashSet},
    iter,
};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Error, Expr, ItemEnum, Lit, LitByte, Token, Variant,
};

use quote::quote;

const VARIANT_ATTR_MATCHES: &str = "matches";
const VARIANT_ATTR_WILDCARD: &str = "wildcard";
const LANES: usize = 16;

pub fn one_hot(_args: TokenStream, tokens: TokenStream) -> TokenStream {
    let item = parse_macro_input!(tokens as ItemEnum);

    let mut table = HashMap::with_capacity(item.variants.len());
    let mut wildcard = None;

    for Variant {
        attrs,
        ident,
        fields: _,
        discriminant,
    } in &item.variants
    {
        let class = if let Some((_, expr)) = discriminant {
            let Expr::Lit(lit) = expr else {
                return Error::new_spanned(expr, "variant values can only be literals")
                    .into_compile_error()
                    .into();
            };

            let value = match &lit.lit {
                Lit::Byte(byte) => byte.value(),
                Lit::Int(int) => match int.base10_parse::<u8>() {
                    Ok(byte) => byte,
                    Err(err) => return err.into_compile_error().into(),
                },
                _ => {
                    return Error::new_spanned(
                        lit,
                        "variant values can only be byte literals or integer literals",
                    )
                    .into_compile_error()
                    .into()
                }
            };

            Class::with_value(ident.to_string(), value)
        } else {
            Class::new(ident.to_string())
        };

        assert_eq!(attrs.len(), 1); // FIXME
        let attr = attrs.first().unwrap();

        if attr.path().is_ident(VARIANT_ATTR_MATCHES) {
            let bytes = match attr.parse_args_with(Bytes::parse) {
                Ok(bytes) => bytes,
                Err(err) => return err.into_compile_error().into(),
            };

            table.insert(class, bytes);
        } else if attr.path().is_ident(VARIANT_ATTR_WILDCARD) {
            if wildcard.is_some() {
                return Error::new_spanned(attr, "attribute wildcard can only be set once")
                    .into_compile_error()
                    .into();
            }

            wildcard = Some(class)
        } else {
            return Error::new_spanned(attr, "invalid attribute")
                .into_compile_error()
                .into();
        }
    }

    let Some(wildcard) = wildcard else {
        return Error::new_spanned(item, "variant values can only be literals")
            .into_compile_error()
            .into();
    };

    let input2 = SimdTableInput {
        table,
        wildcard,
        ident: item.ident.clone(),
    };

    let Some(syntax) = input2.generate() else {
        return Error::new_spanned(item, "table is unsatisfiable")
            .into_compile_error()
            .into();
    };

    let vis = item.vis;
    let attrs = item.attrs;

    quote!(#(#attrs)* #[repr(u8)] #vis #syntax).into()
}

#[derive(Debug, Clone)]
enum Bytes {
    Singleton(u8),
    List(Vec<u8>),
    /// Inclusive range of bytes, i.e. `start..=end`.
    Range {
        start: u8,
        end: u8,
    },
}

impl Bytes {
    fn into_vec(self) -> Vec<u8> {
        match self {
            Bytes::Singleton(b) => vec![b],
            Bytes::List(v) => v,
            Bytes::Range { start, end } => (start..=end).collect(),
        }
    }
}

impl Parse for Bytes {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitByte) {
            let byte = input.parse::<LitByte>()?.value();

            let lookahead = input.lookahead1();

            if lookahead.peek(Token![..]) {
                input.parse::<Token![..]>()?;
                let start = byte;
                let end = input.parse::<LitByte>()?.value();

                Ok(Self::Range { start, end })
            } else if lookahead.peek(Token![|]) {
                let mut bytes = vec![byte];
                while input.lookahead1().peek(Token![|]) {
                    input.parse::<Token![|]>()?;
                    bytes.push(input.parse::<LitByte>()?.value());
                }

                Ok(Self::List(bytes))
            } else {
                Ok(Self::Singleton(byte))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Class {
    name: String,
    value: Option<u8>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self { name, value: None }
    }

    pub fn with_value(name: String, value: u8) -> Self {
        Self {
            name,
            value: Some(value),
        }
    }
}

#[derive(Debug)]
struct SimdTableInput {
    table: HashMap<Class, Bytes>,
    wildcard: Class,
    ident: Ident,
}

impl SimdTableInput {
    fn generate(self) -> Option<proc_macro2::TokenStream> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        struct Nibbles {
            lo: u8,
            hi: u8,
        }

        impl Nibbles {
            fn from_byte(byte: u8) -> Self {
                Self {
                    lo: byte & 0b1111,
                    hi: byte >> 4,
                }
            }

            fn into_byte(self) -> u8 {
                self.lo | (self.hi << 4)
            }
        }

        assert!(self.table.len() /* wildcard */ <= 8);

        fn is_product(bytes: &[u8]) -> bool {
            let bytes = bytes
                .iter()
                .map(|b| Nibbles::from_byte(*b))
                .collect::<HashSet<_>>();
            let (mut los, mut his) = (HashSet::new(), HashSet::new());
            let mut missing = HashSet::new();

            for Nibbles { lo, hi } in bytes.iter().copied() {
                if !los.contains(&lo) {
                    for lo in los.iter().copied() {
                        let n = Nibbles { lo, hi };
                        if !bytes.contains(&n) {
                            missing.insert(n.into_byte());
                        }
                    }

                    let _ = los.insert(lo);
                }

                if !his.contains(&hi) {
                    for hi in his.iter().copied() {
                        let n = Nibbles { lo, hi };
                        if !bytes.contains(&n) {
                            missing.insert(n.into_byte());
                        }
                    }

                    let _ = his.insert(hi);
                }
            }

            missing.is_empty()
        }

        let ident = self.ident;

        let mut table_lo = [0_u8; LANES];
        let mut table_hi = [0_u8; LANES];

        for (i, (_, bytes)) in self.table.iter().enumerate() {
            let bytes = bytes.clone().into_vec();

            if !is_product(&bytes) {
                return None;
            }

            for byte in bytes {
                let Nibbles { lo, hi } = Nibbles::from_byte(byte);

                table_lo[lo as usize] |= 1 << i;
                table_hi[hi as usize] |= 1 << i;
            }
        }

        let variants = self
            .table
            .keys()
            .enumerate()
            .map(|(i, class)| (class.name.as_str(), 1_u8 << i))
            .chain(iter::once((self.wildcard.name.as_str(), 0_u8)))
            .map(|(name, value)| {
                let name = Ident::new(name, Span::call_site());
                quote!(#name = #value)
            });

        let syntax = quote! {
            enum #ident {
                #(#variants,
                )*
            }

            impl ::absolut::SimdTable<#LANES> for #ident {
                const LO: [u8; #LANES] = [#(#table_lo, )*];
                const HI: [u8; #LANES] = [#(#table_hi, )*];
            }
        };

        Some(syntax)
    }
}
