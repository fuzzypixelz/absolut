extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse::Parse, parse_macro_input, spanned::Spanned, Error, Expr, ItemEnum, Lit, Variant};

use quote::quote;

use crate::algorithms::{Algorithm, Bytes, Class};

const VARIANT_ATTR_MATCHES: &str = "matches";
const VARIANT_ATTR_WILDCARD: &str = "wildcard";

pub fn driver<A: Algorithm>(args: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemEnum);

    let mut algo = A::new(args);
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

            Class {
                name: ident.to_string(),
                value: Some(value),
            }
        } else {
            Class {
                name: ident.to_string(),
                value: None,
            }
        };

        for attr in attrs {
            if attr.path().is_ident(VARIANT_ATTR_MATCHES) {
                let bytes = match attr.parse_args_with(Bytes::parse) {
                    Ok(bytes) => bytes,
                    Err(err) => return err.into_compile_error().into(),
                };

                algo.map(bytes, class.clone());
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
            .into();
    };

    let span = item.span();
    let vis = item.vis;
    let attrs = item.attrs;
    let ident = item.ident;

    match algo.generate(wildcard, ident, span) {
        Ok(syntax) => quote!(#(#attrs)* #[repr(u8)] #vis #syntax).into(),
        Err(error) => error.into_compile_error().into(),
    }
}
