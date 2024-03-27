extern crate proc_macro;

use syn::{
    parse::{Parse, ParseStream},
    LitByte, Token,
};

pub mod composite;
#[cfg(feature = "general")]
pub mod general;
pub mod one_cold;
pub mod one_hot;
pub mod one_of_8;

mod solver;

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
    /// Inclusive range of bytes, i.e. `start..=end`.
    Range {
        start: u8,
        end: u8,
    },
}

impl Bytes {
    pub fn to_vec(&self) -> Vec<u8> {
        match self {
            Bytes::Singleton(b) => vec![*b],
            Bytes::List(v) => v.clone(),
            Bytes::Range { start, end } => (*start..=*end).collect(),
        }
    }

    pub fn contains(&self, byte: &u8) -> bool {
        match self {
            Bytes::Singleton(b) => byte == b,
            Bytes::List(v) => v.contains(byte),
            Bytes::Range { start, end } => start <= byte && byte <= end,
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
