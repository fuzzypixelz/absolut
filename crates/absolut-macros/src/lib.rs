mod algorithms;
mod driver;

extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn one_hot(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::one_of_8::OneOf8Algorithm</* HOT? */ true>>(args, tokens)
}

#[proc_macro_attribute]
pub fn one_cold(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::one_of_8::OneOf8Algorithm</* HOT? */ false>>(args, tokens)
}

#[proc_macro_attribute]
pub fn composite(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::composite::CompositeAlgorithm>(args, tokens)
}

#[cfg(feature = "sat")]
#[proc_macro_attribute]
pub fn general(args: TokenStream, tokens: TokenStream) -> TokenStream {
    driver::driver::<crate::algorithms::general::General>(args, tokens)
}
