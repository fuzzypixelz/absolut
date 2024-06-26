extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    LitByte, Token,
};

pub mod composite;
#[cfg(feature = "sat")]
pub mod general;
pub mod one_of_8;
#[cfg(feature = "sat")]
mod solver;

pub trait Algorithm {
    fn new(args: TokenStream) -> Self;

    fn map(&mut self, bytes: Bytes, class: Class);

    fn generate(
        self,
        wildcard: Class,
        ident: syn::Ident,
        span: proc_macro2::Span,
    ) -> syn::Result<proc_macro2::TokenStream>;
}

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

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub value: Option<u8>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Bytes {
    Singleton(u8),
    List(Vec<u8>),
    ExclusiveRange { start: u8, end: u8 },
    InclusiveRange { start: u8, end: u8 },
}

impl Bytes {
    pub fn to_vec(&self) -> Vec<u8> {
        match self {
            Bytes::Singleton(b) => vec![*b],
            Bytes::List(v) => v.clone(),
            Bytes::ExclusiveRange { start, end } => (*start..*end).collect(),
            Bytes::InclusiveRange { start, end } => (*start..=*end).collect(),
        }
    }

    #[cfg(feature = "sat")]
    pub fn contains(&self, byte: &u8) -> bool {
        match self {
            Bytes::Singleton(b) => byte == b,
            Bytes::List(v) => v.contains(byte),
            Bytes::ExclusiveRange { start, end } => start <= byte && byte < end,
            Bytes::InclusiveRange { start, end } => start <= byte && byte <= end,
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

                if input.lookahead1().peek(Token![=]) {
                    input.parse::<Token![=]>()?;

                    let start = byte;
                    let end = input.parse::<LitByte>()?.value();

                    Ok(Self::InclusiveRange { start, end })
                } else {
                    let start = byte;
                    let end = input.parse::<LitByte>()?.value();

                    Ok(Self::ExclusiveRange { start, end })
                }
            } else if lookahead.peek(Token![,]) {
                let mut bytes = vec![byte];
                while input.lookahead1().peek(Token![,]) {
                    input.parse::<Token![,]>()?;
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
