extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Error, Expr, Lit, LitByte, Token, Variant, ItemEnum, __private::quote::quote,
};

use darling::{FromMeta, export::NestedMeta};

use absolut_core::{Class, SimdTableInputBuilder};

const VARIANT_ATTR_MATCHES: &str = "matches";
const VARIANT_ATTR_WILDCARD: &str = "wildcard";

#[derive(Debug, Default, FromMeta)]
struct Arguments {
    #[darling(rename = "LANES")]
    lanes: usize,
    powers_of_two: Option<()>,
}

#[proc_macro_attribute]
pub fn simd_table(args: proc_macro::TokenStream, tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // FIXME(fuzzypixelz): we don't add back visibility and attributes to `item`

    let args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(args) => args,
        Err(err) => return err.into_compile_error().into()
    };

    let args = match Arguments::from_list(&args) {
        Ok(args) => args,
        Err(err) => return TokenStream::from(err.write_errors())
    };

    let item = parse_macro_input!(tokens as ItemEnum);

    let mut builder = SimdTableInputBuilder::with_capacity(item.variants.len());
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
                    .into()
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

        for attr in attrs {
            if attr.path().is_ident(VARIANT_ATTR_MATCHES) {
                let bytes = match attr.parse_args_with(Bytes::parse) {
                    Ok(bytes) => bytes,
                    Err(err) => return err.into_compile_error().into(),
                };

                match bytes {
                    Bytes::Singleton(byte) => builder.insert(byte, class.clone()),
                    Bytes::List(bytes) => {
                        for byte in bytes {
                            builder.insert(byte, class.clone())
                        }
                    }
                    Bytes::Range { start, end } => {
                        for byte in start..end {
                            builder.insert(byte, class.clone())
                        }
                    }
                }
            } else if attr.path().is_ident(VARIANT_ATTR_WILDCARD) {
                // FIXME(fuzzypixelz): we don't check that `wildcard` has no arguments

                if wildcard.is_some() {
                    return Error::new_spanned(attr, "attribute wildcard can only be set once")
                        .into_compile_error()
                        .into();
                }

                wildcard = Some(class.clone())
            } else {
                return Error::new_spanned(attr, "invalid attribute")
                    .into_compile_error()
                    .into();
            }
        }
    }

    let Some(wildcard) = wildcard else {
        return Error::new_spanned(item, "variant values can only be literals")
                .into_compile_error()
                .into() 
    };

    if args.powers_of_two.is_some() {
        builder.set_powers_of_two();
    }

    let input = builder.build(wildcard, item.ident.clone(), args.lanes);

    let Some(syntax) = input.generate() else {
        return Error::new_spanned(item, "table is unsatisfiable")
            .into_compile_error()
            .into();
    };

    let vis = item.vis;
    let attrs = item.attrs;

    quote!(#(#attrs)* #[repr(u8)] #vis #syntax).into()
}

#[derive(Debug)]
enum Bytes {
    Singleton(u8),
    List(Vec<u8>),
    Range { start: u8, end: u8 },
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
