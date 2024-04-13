extern crate self as absolut;

pub use absolut_macros::composite;
pub use absolut_macros::one_cold;
pub use absolut_macros::one_hot;
#[cfg(feature = "sat")]
pub use absolut_macros::one_hot_sat;

pub trait OneOf8 {
    const TABLE_LOW_NIBBLES: [u8; 16];
    const TABLE_HIGH_NIBBLES: [u8; 16];
}

pub trait OneHot: OneOf8 {}

pub trait OneCold: OneOf8 {}

pub trait Composite {
    const TABLE_QUARTERS: [[u8; 64]; 4];
}
