mod algorithms;

extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn one_hot(args: TokenStream, tokens: TokenStream) -> TokenStream {
    crate::algorithms::one_hot::one_hot(args, tokens)
}

#[proc_macro_attribute]
pub fn one_cold(args: TokenStream, tokens: TokenStream) -> TokenStream {
    crate::algorithms::one_cold::one_cold(args, tokens)
}

#[cfg(feature = "general")]
#[proc_macro_attribute]
pub fn general(args: TokenStream, tokens: TokenStream) -> TokenStream {
    crate::algorithms::general::general(args, tokens)
}
