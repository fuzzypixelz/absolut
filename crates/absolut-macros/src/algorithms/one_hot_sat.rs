extern crate proc_macro;

use std::{collections::HashMap, iter};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use quote::quote;

use crate::algorithms::{solver, Algorithm, Bytes, Class, Nibbles};

pub struct OneHotSATAlgorithm {
    table: HashMap<Bytes, Class>,
}

impl Algorithm for OneHotSATAlgorithm {
    fn new(_args: TokenStream) -> Self {
        Self {
            table: HashMap::with_capacity(256),
        }
    }

    fn map(&mut self, bytes: Bytes, class: Class) {
        let _ = self.table.insert(bytes, class);
    }

    fn generate(
        self,
        wildcard: Class,
        ident: syn::Ident,
        span: proc_macro2::Span,
    ) -> syn::Result<proc_macro2::TokenStream> {
        type Bits = solver::Bits<8>;
        type BitsArray = solver::BitsArray<8, 16>;

        fn assert_lookup(
            ctx: &mut solver::Context,
            byte: u8,
            table_lo: &BitsArray,
            table_hi: &BitsArray,
            target: &Bits,
        ) {
            let Nibbles { lo, hi } = Nibbles::from_byte(byte);
            ctx.assert_andeq(
                table_lo.get(lo as usize).unwrap(),
                table_hi.get(hi as usize).unwrap(),
                target,
            );
        }

        let mut ctx = solver::Context::new();

        let class_vars = self
            .table
            .values()
            .map(|class| (class.name.as_str(), Bits::new(&mut ctx)))
            .collect::<HashMap<_, _>>();

        let wildcard_var = Bits::new(&mut ctx);

        for (lhs, rhs) in class_vars.iter().flat_map(|lhs| {
            iter::repeat(lhs).zip(class_vars.iter().filter(move |rhs| rhs.0 != lhs.0))
        }) {
            ctx.assert_noteq(lhs.1, rhs.1);
        }

        for (_, var) in class_vars.iter() {
            ctx.assert_noteq(var, &wildcard_var);
        }

        let table_lo = BitsArray::new(&mut ctx);
        let table_hi = BitsArray::new(&mut ctx);

        for (bytes, class) in self.table.iter() {
            let bits = class_vars.get(class.name.as_str()).unwrap();

            for byte in bytes.to_vec() {
                assert_lookup(&mut ctx, byte, &table_lo, &table_hi, bits);
            }

            // TODO(fuzzypixelz): Take a `powers_of_two` argument and `ctx.assert_power_of_two(bits)`

            if let Some(value) = class.value {
                ctx.assert_const(bits, value);
            }
        }

        for byte in (0..u8::MAX).filter(|b| self.table.keys().all(|bs| !bs.contains(b))) {
            assert_lookup(&mut ctx, byte, &table_lo, &table_hi, &wildcard_var);

            if let Some(value) = wildcard.value {
                ctx.assert_const(&wildcard_var, value);
            }
        }

        match ctx.model() {
            None => Err(syn::Error::new(span, "table is unsatisfiable")),
            Some(model) => {
                let variants = class_vars
                    .into_iter()
                    .chain(iter::once((wildcard.name.as_str(), wildcard_var)))
                    .map(|(name, bits)| {
                        let name = Ident::new(name, Span::call_site());
                        let byte = model.eval_bits(&bits);

                        quote!(#name = #byte)
                    });

                let lo = model.eval_bits_array(table_lo);
                let hi = model.eval_bits_array(table_hi);

                let syntax = quote! {
                    enum #ident {
                        #(#variants,
                        )*
                    }

                    impl ::absolut::OneOf8 for #ident {
                        const TABLE_LOW_NIBBLES: [u8; 16] = [#(#lo, )*];
                        const TABLE_HIGH_NIBBLES: [u8; 16] = [#(#hi, )*];
                    }

                    impl ::absolut::OneHot for #ident {}
                };

                Ok(syntax)
            }
        }
    }
}
