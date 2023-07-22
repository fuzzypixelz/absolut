extern crate proc_macro;

use std::{collections::HashMap, iter};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Error, Expr, Lit, LitByte, Token, Variant, ItemEnum,
};

use darling::{FromMeta, export::NestedMeta};

use quote::quote;

use z3::{
    ast::{Array, Ast, Bool, BV},
    Config, Context, Model, SatResult, Solver, Sort,
};

const VARIANT_ATTR_MATCHES: &str = "matches";
const VARIANT_ATTR_WILDCARD: &str = "wildcard";

#[proc_macro_attribute]
pub fn simd_table(args: TokenStream, tokens: TokenStream) -> TokenStream {
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

#[derive(Debug, Default, FromMeta)]
struct Arguments {
    #[darling(rename = "LANES")]
    lanes: usize,
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

struct SimdTableInput {
    table: HashMap<u8, Class>,
    wildcard: Class,
    ident: Ident,
    lanes: usize,
    powers_of_two: bool,
}

impl SimdTableInput {
    pub fn generate(self) -> Option<proc_macro2::TokenStream> {
        fn nibble_hi<'ctx>(ctx: &'ctx Context, bv: &BV<'ctx>) -> BV<'ctx> {
            bv.bvlshr(&BV::from_u64(ctx, 4, 8)).extract(3, 0)
        }

        fn nibble_lo<'ctx>(ctx: &'ctx Context, bv: &BV<'ctx>) -> BV<'ctx> {
            bv.bvand(&BV::from_u64(ctx, 0b1111, 8)).extract(3, 0)
        }

        fn lookup<'ctx>(
            ctx: &'ctx Context,
            table_lo: &Array<'ctx>,
            table_hi: &Array<'ctx>,
            bv: &BV<'ctx>,
        ) -> BV<'ctx> {
            table_lo.select(&nibble_lo(ctx, bv)).as_bv().unwrap()
                & table_hi.select(&nibble_hi(ctx, bv)).as_bv().unwrap()
        }

        fn is_power_of_two<'ctx>(ctx: &'ctx Context, bv: &BV<'ctx>) -> Bool<'ctx> {
            let zero = BV::from_u64(ctx, 0, 8);
            let one = BV::from_u64(ctx, 1, 8);

            bv.bvand(&bv.bvsub(&one))._eq(&zero)
        }

        fn bytes<'ctx>(
            ctx: &'ctx Context,
            model: &'ctx Model<'ctx>,
            table: Array<'ctx>,
            lanes: usize,
        ) -> impl Iterator<Item = u8> + 'ctx {
            let table = model.eval(&table, true).unwrap();

            (0..lanes).map(move |i| {
                let tmp = table
                    .select(&BV::from_u64(ctx, i as u64, 4))
                    .as_bv()
                    .unwrap();

                model.eval(&tmp, true).unwrap().as_u64().unwrap() as u8
            })
        }

        // NOTE(fuzzypixelz): the "trace" param is false by default but a ".z3-trace"
        // is still generated. See: https://microsoft.github.io/z3guide/programming/Parameters/
        let ctx = &Context::new(&Config::default());
        let solver = Solver::new(ctx);

        let variables = self
            .table
            .values()
            .map(|class| {
                let bv = BV::new_const(ctx, class.name.as_str(), 8);
                (class.name.as_str(), bv)
            })
            .collect::<HashMap<_, _>>();

        let wildcard_bv = BV::new_const(ctx, self.wildcard.name.as_str(), 8);

        for (lhs, rhs) in variables.iter().flat_map(|lhs| {
            iter::repeat(lhs).zip(variables.iter().filter(move |rhs| rhs.0 != lhs.0))
        }) {
            solver.assert(&lhs.1._eq(rhs.1).not())
        }

        for (_, bv) in variables.iter() {
            solver.assert(&bv._eq(&wildcard_bv).not())
        }

        let table_lo = Array::new_const(
            ctx,
            "table_lo",
            &Sort::bitvector(ctx, 4),
            &Sort::bitvector(ctx, 8),
        );

        let table_hi = Array::new_const(
            ctx,
            "table_hi",
            &Sort::bitvector(ctx, 4),
            &Sort::bitvector(ctx, 8),
        );

        for (byte, class) in self.table.iter() {
            let bv_byte = BV::from_u64(ctx, *byte as u64, 8);
            let bv_target = variables.get(class.name.as_str()).unwrap();

            solver.assert(&lookup(ctx, &table_lo, &table_hi, &bv_byte)._eq(bv_target));

            if self.powers_of_two {
                solver.assert(&is_power_of_two(ctx, bv_target))
            }

            if let Some(value) = class.value {
                let bv_value = BV::from_u64(ctx, value as u64, 8);
                solver.assert(&bv_target._eq(&bv_value));
            }
        }

        for byte in (0..128).filter(|b| !self.table.contains_key(b)) {
            let byte_bv = BV::from_u64(ctx, byte as u64, 8);

            solver.assert(&lookup(ctx, &table_lo, &table_hi, &byte_bv)._eq(&wildcard_bv));

            if self.powers_of_two {
                solver.assert(&is_power_of_two(ctx, &wildcard_bv))
            }

            if let Some(value) = self.wildcard.value {
                let value_bv = BV::from_u64(ctx, value as u64, 8);
                solver.assert(&wildcard_bv._eq(&value_bv));
            }
        }

        match solver.check() {
            SatResult::Unsat | SatResult::Unknown => None,
            SatResult::Sat => {
                let model = solver.get_model().unwrap();

                let ident = self.ident;
                let lanes = self.lanes;

                let variants = variables
                    .into_iter()
                    .chain(iter::once((self.wildcard.name.as_str(), wildcard_bv)))
                    .map(|(name, bv)| {
                        let name = Ident::new(name, Span::call_site());
                        let byte = model.eval(&bv, true).unwrap().as_u64().unwrap() as u8;

                        quote!(#name = #byte)
                    });

                let lo = bytes(ctx, &model, table_lo, self.lanes);
                let hi = bytes(ctx, &model, table_hi, self.lanes);

                let syntax = quote! {
                    enum #ident {
                        #(#variants,
                        )*
                    }

                    impl ::absolut::SimdTable<#lanes> for #ident {
                        const LO: [u8; #lanes] = [#(#lo, )*];
                        const HI: [u8; #lanes] = [#(#hi, )*];
                    }
                };

                Some(syntax)
            }
        }
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

    pub fn build(self, wildcard: Class, ident: Ident, lanes: usize) -> SimdTableInput {
        let SimdTableInputBuilder {
            table,
            powers_of_two,
        } = self;

        SimdTableInput {
            table,
            powers_of_two,
            wildcard,
            ident,
            lanes,
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