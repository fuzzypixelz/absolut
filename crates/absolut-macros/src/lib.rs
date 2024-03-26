mod algorithms;
mod driver;

extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn one_hot(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::one_hot::OneHotBuilder>(args, tokens)
}

#[proc_macro_attribute]
pub fn one_cold(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::one_cold::OneColdBuilder>(args, tokens)
}

#[proc_macro_attribute]
pub fn composite(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::composite::CompositeBuilder>(args, tokens)
}

#[cfg(feature = "general")]
#[proc_macro_attribute]
pub fn general(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::general::GeneralBuilder>(args, tokens)
}
