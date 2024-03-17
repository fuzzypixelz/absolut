extern crate proc_macro;

use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Error, Expr, ItemEnum, Lit, LitByte, Token, Variant,
};

use darling::{export::NestedMeta, FromMeta};

use quote::quote;

const VARIANT_ATTR_MATCHES: &str = "matches";
const VARIANT_ATTR_WILDCARD: &str = "wildcard";

pub fn composite(args: TokenStream, tokens: TokenStream) -> TokenStream {
    // FIXME(fuzzypixelz): we don't add back visibility and attributes to `item`

    let args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(args) => args,
        Err(err) => return err.into_compile_error().into(),
    };

    let args = match Arguments::from_list(&args) {
        Ok(args) => args,
        Err(err) => return TokenStream::from(err.write_errors()),
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
                        for byte in start..=end {
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
            .into();
    };

    if args.powers_of_two.is_some() {
        builder.set_powers_of_two();
    }

    let input = builder.build(wildcard, item.ident.clone());

    let Some(syntax) = input.generate() else {
        return Error::new_spanned(item, "table is unsatisfiable")
            .into_compile_error()
            .into();
    };

    let vis = item.vis;
    let attrs = item.attrs;

    quote!(#(#attrs)* #[repr(u8)] #vis #syntax).into()
}

#[derive(Debug, Default, FromMeta)]
struct Arguments {
    // #[darling(rename = "LANES")]
    // lanes: usize,
    powers_of_two: Option<()>,
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

struct ClassGenerator {
    blacklist: HashSet<u8>,
    generated: HashSet<u8>,
    powers_of_two: bool,
    counter: usize,
}

impl ClassGenerator {
    fn new(blacklist: impl Iterator<Item = u8>, powers_of_two: bool) -> Self {
        Self {
            blacklist: HashSet::from_iter(blacklist),
            generated: HashSet::new(),
            powers_of_two,
            counter: 0,
        }
    }

    fn next(&mut self) -> Option<u8> {
        if self.counter + self.blacklist.len() >= u8::MAX as usize + 1 {
            return None;
        }

        let value = if self.powers_of_two {
            1 << self.counter
        } else {
            self.counter as u8
        };

        self.counter += 1;
        self.generated.insert(value);

        Some(value)
    }

    fn gen(&mut self) -> Option<u8> {
        let mut value = self.next()?;

        while self.blacklist.contains(&value) {
            value = self.next()?;
        }

        Some(value)
    }

    fn check(&self, value: u8) -> bool {
        if self.powers_of_two {
            value.count_ones() <= 1 && !self.generated.contains(&value)
        } else {
            !self.generated.contains(&value)
        }
    }
}

struct SimdTableInput {
    table: HashMap<u8, Class>,
    wildcard: Class,
    ident: Ident,
    powers_of_two: bool,
}

impl SimdTableInput {
    fn generate(self) -> Option<proc_macro2::TokenStream> {
        let mut class_vars = HashMap::new();

        let mut class_generator = ClassGenerator::new(
            self.table.values().filter_map(|c| c.value),
            self.powers_of_two,
        );

        let wildcard_value = self.wildcard.value.unwrap_or(class_generator.gen()?);

        let mut q1 = [wildcard_value; 64];
        let mut q2 = [wildcard_value; 64];
        let mut q3 = [wildcard_value; 64];
        let mut q4 = [wildcard_value; 64];

        class_vars.insert(self.wildcard.name.as_str(), wildcard_value);

        for (byte, class) in &self.table {
            let value = match class_vars.get(class.name.as_str()).copied() {
                Some(value) => value,
                None => {
                    let value = match class.value {
                        Some(value) => class_generator.check(value).then(|| value)?,
                        None => class_generator.gen()?,
                    };

                    class_vars.insert(class.name.as_str(), value);

                    value
                }
            };

            match byte {
                0..=63 => q1[*byte as usize] = value,
                64..=127 => q2[*byte as usize - 64] = value,
                128..=191 => q3[*byte as usize - 128] = value,
                192..=255 => q4[*byte as usize - 192] = value,
            };
        }

        let ident = self.ident;

        let variants = class_vars.into_iter().map(|(name, byte)| {
            let name = Ident::new(name, Span::call_site());

            quote!(#name = #byte)
        });

        let syntax = quote! {
            enum #ident {
                #(#variants,
                )*
            }

            impl ::absolut::Composite for #ident {
                const Q0: [u8; 64] = [#(#q1, )*];
                const Q1: [u8; 64] = [#(#q2, )*];
                const Q2: [u8; 64] = [#(#q3, )*];
                const Q3: [u8; 64] = [#(#q4, )*];
            }
        };

        Some(syntax)
    }
}

struct SimdTableInputBuilder {
    table: HashMap<u8, Class>,
    powers_of_two: bool,
}

impl SimdTableInputBuilder {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            table: HashMap::with_capacity(capacity),
            powers_of_two: false,
        }
    }

    pub fn insert(&mut self, byte: u8, class: Class) {
        let _ = self.table.insert(byte, class);
    }

    pub fn set_powers_of_two(&mut self) {
        self.powers_of_two = true;
    }

    pub fn build(self, wildcard: Class, ident: Ident) -> SimdTableInput {
        let SimdTableInputBuilder {
            table,
            powers_of_two,
        } = self;

        SimdTableInput {
            table,
            powers_of_two,
            wildcard,
            ident,
        }
    }
}

#[derive(Clone)]
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
