extern crate proc_macro;

use std::{collections::HashMap, iter};

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

pub fn general(args: TokenStream, tokens: TokenStream) -> TokenStream {
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

struct SimdTableInput {
    table: HashMap<u8, Class>,
    wildcard: Class,
    ident: Ident,
    powers_of_two: bool,
}

impl SimdTableInput {
    pub fn generate(self) -> Option<proc_macro2::TokenStream> {
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
        }

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

        let wildcard = Bits::new(&mut ctx);

        for (lhs, rhs) in class_vars.iter().flat_map(|lhs| {
            iter::repeat(lhs).zip(class_vars.iter().filter(move |rhs| rhs.0 != lhs.0))
        }) {
            ctx.assert_noteq(lhs.1, rhs.1);
        }

        for (_, var) in class_vars.iter() {
            ctx.assert_noteq(var, &wildcard);
        }

        let table_lo = BitsArray::new(&mut ctx);
        let table_hi = BitsArray::new(&mut ctx);

        for (byte, class) in self.table.iter() {
            let bits = class_vars.get(class.name.as_str()).unwrap();

            assert_lookup(&mut ctx, *byte, &table_lo, &table_hi, bits);

            if self.powers_of_two {
                ctx.assert_power_of_two(bits);
            }

            if let Some(value) = class.value {
                ctx.assert_const(bits, value);
            }
        }

        for byte in (0..u8::MAX).filter(|b| !self.table.contains_key(b)) {
            assert_lookup(&mut ctx, byte, &table_lo, &table_hi, &wildcard);

            if let Some(value) = self.wildcard.value {
                ctx.assert_const(&wildcard, value);
            }
        }

        match ctx.model() {
            None => None,
            Some(model) => {
                let ident = self.ident;

                let variants = class_vars
                    .into_iter()
                    .chain(iter::once((self.wildcard.name.as_str(), wildcard)))
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

                    impl ::absolut::SimdTable<16> for #ident {
                        const LO: [u8; 16] = [#(#lo, )*];
                        const HI: [u8; 16] = [#(#hi, )*];
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

mod solver {
    use std::{array, collections::HashSet};

    use varisat::{ExtendFormula, Lit, Solver, Var};

    pub struct Context<'ctx>(Solver<'ctx>);

    impl Context<'_> {
        pub fn new() -> Self {
            Self(Solver::new())
        }

        pub fn assert_andeq<const N: usize>(
            &mut self,
            lhs: &Bits<N>,
            rhs: &Bits<N>,
            res: &Bits<N>,
        ) {
            // In general, any equation of the form X & Y = Z where X, Y, Z are bits (or booleans)
            // is equivalent the following boolean clauses in propositional logic:
            // ~X | ~Y | Z
            // X | ~Z
            // Y | ~Z
            for ((lhs, rhs), res) in lhs.0.iter().zip(rhs.0.iter()).zip(res.0.iter()) {
                self.0
                    .add_clause(&[lhs.negative(), rhs.negative(), res.positive()]);
                self.0.add_clause(&[lhs.positive(), res.negative()]);
                self.0.add_clause(&[rhs.positive(), res.negative()]);
            }
        }

        pub fn assert_noteq<const N: usize>(&mut self, lhs: &Bits<N>, rhs: &Bits<N>) {
            // Go through all possible 8-bit integers and for each such interger,
            // add a clause that prohibits both lhs and rhs to be be equal to it
            let mut clause = Vec::with_capacity(N + N);
            for b in 0..(1 << N) {
                for i in 0..N {
                    if (1 << i) & b == 0 {
                        // The i-th bit of this integer is unset,
                        // thus we mean to guard against both lhs and rhs having their i-th bits unset
                        // thus we want the negation of (NOT lhs[i] AND NOT rhs[i])
                        // i.e. (lhs[i] OR rhs[i])
                        clause.push(lhs.0[i].positive());
                        clause.push(rhs.0[i].positive());
                    } else {
                        // The i-th bit of this integer is set,
                        // thus we mean to guard against both lhs and rhs having their i-th bits set
                        // thus we want the negation of (lhs[i] AND rhs[i])
                        // i.e. (NOT lhs[i] OR NOT rhs[i])
                        clause.push(lhs.0[i].negative());
                        clause.push(rhs.0[i].negative());
                    }
                }
                self.0.add_clause(&clause);
                clause.clear();
            }
        }

        pub fn assert_const<const N: usize>(&mut self, lhs: &Bits<N>, rhs: u8) {
            for i in 0..N {
                self.0.add_clause(&[lhs.0[i].lit((1 << i) & rhs != 0)]);
            }
        }

        pub fn assert_power_of_two<const N: usize>(&mut self, bits: &Bits<N>) {
            // Assert that no two bits are set at the same time
            for i in 0..N {
                for j in 0..N {
                    if i != j {
                        self.0
                            .add_clause(&[bits.0[i].negative(), bits.0[j].negative()]);
                    }
                }
            }

            // Assert that not all bit are unset
            self.0.add_clause(&bits.0.map(|var| var.positive()))
        }

        pub fn model(&mut self) -> Option<Model> {
            match self.0.solve() {
                Ok(true) => Some(Model(HashSet::from_iter(self.0.model().unwrap()))),
                Ok(false) | Err(_) => None,
            }
        }
    }

    #[derive(Debug)]
    pub struct Model(HashSet<Lit>);

    impl Model {
        pub fn eval_bits<const N: usize>(&self, bits: &Bits<N>) -> u8 {
            bits.0.iter().enumerate().fold(0, |acc, (i, var)| {
                if self.0.contains(&var.positive()) {
                    acc | (1 << i)
                } else {
                    acc
                }
            })
        }

        pub fn eval_bits_array<const N: usize, const M: usize>(
            &self,
            bits_array: BitsArray<N, M>,
        ) -> [u8; M] {
            bits_array.0.map(|bits| self.eval_bits(&bits))
        }
    }

    // const BITS_SIZE: usize = 8;
    #[derive(Debug)]
    pub struct Bits<const N: usize>([Var; N]);

    impl<const N: usize> Bits<N> {
        pub fn new(ctx: &mut Context) -> Self {
            let mut vars = ctx.0.new_var_iter(N);
            Self(array::from_fn(|_| vars.next().unwrap()))
        }
    }

    // const BITS_ARRAY_SIZE: usize = 16;
    #[derive(Debug)]
    pub struct BitsArray<const N: usize, const M: usize>([Bits<N>; M]);

    impl<const N: usize, const M: usize> BitsArray<N, M> {
        pub fn new(ctx: &mut Context) -> Self {
            Self(array::from_fn(|_| Bits::new(ctx)))
        }

        pub fn get(&self, index: usize) -> Option<&Bits<N>> {
            self.0.get(index)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::{Bits, Context};

        #[test]
        fn test_noteq() {
            let mut ctx = Context::new();

            let b0 = Bits::<8>::new(&mut ctx);
            let b1 = Bits::<8>::new(&mut ctx);

            ctx.assert_noteq(&b0, &b1);

            let model = ctx.model().unwrap();

            assert_ne!(model.eval_bits(&b0), model.eval_bits(&b1))
        }
    }
}
