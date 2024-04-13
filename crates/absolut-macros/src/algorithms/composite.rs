extern crate proc_macro;

use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};

use quote::quote;

use crate::algorithms::{Algorithm, Bytes, Class};

pub struct CompositeAlgorithm {
    table: HashMap<Bytes, Class>,
}

impl Algorithm for CompositeAlgorithm {
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
        let mut class_vars = HashMap::new();

        let mut class_generator = ClassGenerator::new(
            self.table.values().filter_map(|c| c.value),
            // TODO(fuzzypixelz): Customize this using an argument
            false,
        );

        let wildcard_value = match wildcard.value {
            None => class_generator
                .gen()
                .expect("should be able to generate at least one class"),
            Some(value) => value,
        };

        let mut quarters = [[wildcard_value; 64]; 4];

        class_vars.insert(wildcard.name.as_str(), wildcard_value);

        for (bytes, class) in &self.table {
            for byte in bytes.to_vec() {
                let value = match class_vars.get(class.name.as_str()).copied() {
                    Some(value) => value,
                    None => {
                        let value = match class.value {
                            Some(value) => {
                                if class_generator.check(value) {
                                    value
                                } else {
                                    return Err(syn::Error::new(
                                        span,
                                        format!("invalid variant value {value}"),
                                    ));
                                }
                            }
                            None => class_generator
                                .gen()
                                .ok_or(syn::Error::new(span, "too many variants"))?,
                        };

                        class_vars.insert(class.name.as_str(), value);

                        value
                    }
                };

                match byte {
                    0..=63 => quarters[0][byte as usize] = value,
                    64..=127 => quarters[1][byte as usize - 64] = value,
                    128..=191 => quarters[2][byte as usize - 128] = value,
                    192..=255 => quarters[3][byte as usize - 192] = value,
                };
            }
        }

        let variants = class_vars.into_iter().map(|(name, byte)| {
            let name = Ident::new(name, Span::call_site());

            quote!(#name = #byte)
        });

        let [q0, q1, q2, q3] = quarters;

        let syntax = quote! {
            enum #ident {
                #(#variants,
                )*
            }

            impl ::absolut::Composite for #ident {
                const TABLE_QUARTERS: [[u8; 64]; 4] = [
                    [#(#q0, )*],
                    [#(#q1, )*],
                    [#(#q2, )*],
                    [#(#q3, )*]
                ];
            }
        };

        Ok(syntax)
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
