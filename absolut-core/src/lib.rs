use std::{collections::HashMap, iter};

use proc_macro2::{Ident, Span, TokenStream};

use quote::quote;

use z3::{
    ast::{Array, Ast, Bool, BV},
    Config, Context, Model, SatResult, Solver, Sort,
};

pub trait SimdTable<const LANES: usize> {
    const LO: [u8; LANES];
    const HI: [u8; LANES];
}

pub struct SimdTableInput {
    table: HashMap<u8, Class>,
    wildcard: Class,
    ident: Ident,
    lanes: usize,
    powers_of_two: bool,
}

impl SimdTableInput {
    pub fn generate(self) -> Option<TokenStream> {
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

                    impl ::absolut_core::SimdTable<#lanes> for #ident {
                        const LO: [u8; #lanes] = [#(#lo, )*];
                        const HI: [u8; #lanes] = [#(#hi, )*];
                    }
                };

                Some(syntax)
            }
        }
    }
}

pub struct SimdTableInputBuilder {
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
pub struct Class {
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
