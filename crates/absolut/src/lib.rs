extern crate self as absolut;

pub use absolut_macros::general;
pub use absolut_macros::one_cold;
pub use absolut_macros::one_hot;

pub trait SimdTable<const LANES: usize> {
    const LO: [u8; LANES];
    const HI: [u8; LANES];
}

#[cfg(test)]
mod tests {
    // FIXME(fuzzypixelz): add x86_64 tests

    use super::{absolut, SimdTable};

    #[cfg(all(target_arch = "aarch64", target_feature = "neon"))]
    fn lookup_one_hot<const LANES: usize, T: SimdTable<LANES>>(input: &[u8; LANES]) -> [u8; LANES] {
        use std::arch::aarch64::{
            vandq_u8, vdupq_n_u8, vld1q_u8, vqtbl1q_u8, vshrq_n_u8, vst1q_u8,
        };

        unsafe {
            let v_input = vld1q_u8(input.as_ptr());

            let v_table_lo = vld1q_u8(T::LO.as_ptr());
            let v_table_hi = vld1q_u8(T::HI.as_ptr());

            let v_input_lo = vandq_u8(v_input, vdupq_n_u8(0b1111));
            let v_input_hi = vshrq_n_u8::<4>(v_input);

            let v_lookup_lo = vqtbl1q_u8(v_table_lo, v_input_lo);
            let v_lookup_hi = vqtbl1q_u8(v_table_hi, v_input_hi);

            let v_lookup = vandq_u8(v_lookup_lo, v_lookup_hi);

            let mut lookup = [0; LANES];
            vst1q_u8(lookup.as_mut_ptr(), v_lookup);
            lookup
        }
    }

    #[cfg(all(target_arch = "aarch64", target_feature = "neon"))]
    fn lookup_one_cold<const LANES: usize, T: SimdTable<LANES>>(
        input: &[u8; LANES],
    ) -> [u8; LANES] {
        use std::arch::aarch64::{
            vandq_u8, vdupq_n_u8, vld1q_u8, vorrq_u8, vqtbl1q_u8, vshrq_n_u8, vst1q_u8,
        };

        unsafe {
            let v_input = vld1q_u8(input.as_ptr());

            let v_table_lo = vld1q_u8(T::LO.as_ptr());
            let v_table_hi = vld1q_u8(T::HI.as_ptr());

            let v_input_lo = vandq_u8(v_input, vdupq_n_u8(0b1111));
            let v_input_hi = vshrq_n_u8::<4>(v_input);

            let v_lookup_lo = vqtbl1q_u8(v_table_lo, v_input_lo);
            let v_lookup_hi = vqtbl1q_u8(v_table_hi, v_input_hi);

            let v_lookup = vorrq_u8(v_lookup_lo, v_lookup_hi);

            let mut lookup = [0; LANES];
            vst1q_u8(lookup.as_mut_ptr(), v_lookup);
            lookup
        }
    }

    #[cfg(all(target_arch = "aarch64", target_feature = "neon"))]
    fn lookup_general<const LANES: usize, T: SimdTable<LANES>>(input: &[u8; LANES]) -> [u8; LANES] {
        use std::arch::aarch64::{
            vandq_u8, vdupq_n_u8, vld1q_u8, vqtbl1q_u8, vshrq_n_u8, vst1q_u8,
        };

        unsafe {
            let v_input = vld1q_u8(input.as_ptr());

            let v_table_lo = vld1q_u8(T::LO.as_ptr());
            let v_table_hi = vld1q_u8(T::HI.as_ptr());

            let v_input_lo = vandq_u8(v_input, vdupq_n_u8(0b1111));
            let v_input_hi = vshrq_n_u8::<4>(v_input);

            let v_lookup_lo = vqtbl1q_u8(v_table_lo, v_input_lo);
            let v_lookup_hi = vqtbl1q_u8(v_table_hi, v_input_hi);

            let v_lookup = vandq_u8(v_lookup_lo, v_lookup_hi);

            let mut lookup = [0; LANES];
            vst1q_u8(lookup.as_mut_ptr(), v_lookup);
            lookup
        }
    }

    #[test]
    #[cfg(all(target_arch = "aarch64", target_feature = "neon"))]
    fn test_ident() {
        macro_rules! test_ident {
            ($algorithm:meta, $lookup:ident) => {{
                #[$algorithm]
                #[derive(Debug)]
                pub enum Table {
                    #[matches(b'a'..b'o')]
                    Lowercase1,
                    #[matches(b'p'..b'z')]
                    Lowercase2,
                    #[matches(b'A'..b'O')]
                    Uppercase1,
                    #[matches(b'P'..b'Z')]
                    Uppercase2,
                    #[matches(b'_')]
                    Underscore,
                    #[matches(b'0'..b'9')]
                    Digits,
                    #[wildcard]
                    Other = 0,
                }

                let input = b"_some_1dent1f13r";

                use Table::*;
                assert_eq!(
                    $lookup::<16, Table>(&input),
                    [
                        Underscore as u8,
                        Lowercase2 as u8,
                        Lowercase1 as u8,
                        Lowercase1 as u8,
                        Lowercase1 as u8,
                        Underscore as u8,
                        Digits as u8,
                        Lowercase1 as u8,
                        Lowercase1 as u8,
                        Lowercase1 as u8,
                        Lowercase2 as u8,
                        Digits as u8,
                        Lowercase1 as u8,
                        Digits as u8,
                        Digits as u8,
                        Lowercase2 as u8
                    ]
                );
            }};
        }

        test_ident!(absolut::one_cold, lookup_one_cold);
        test_ident!(absolut::one_hot, lookup_one_hot);
        test_ident!(absolut::general, lookup_general);
        test_ident!(absolut::general(powers_of_two), lookup_general);
    }

    #[test]
    #[cfg(all(target_arch = "aarch64", target_feature = "neon"))]
    fn test_json() {
        macro_rules! test_json {
            ($algorithm:meta, $lookup:ident) => {{
                #[$algorithm]
                #[derive(Debug)]
                pub enum Table {
                    #[matches(b',')]
                    Comma,
                    #[matches(b':')]
                    Colon,
                    #[matches(b'[' | b']' | b'{' | b'}')]
                    Brackets,
                    #[matches(b'\r' | b'\n' | b'\t')]
                    Control,
                    #[matches(b' ')]
                    Space,
                    #[wildcard]
                    Other,
                }

                let input = b"'o':{'k':[1,2]}\n";

                use Table::*;
                assert_eq!(
                    $lookup::<16, Table>(&input),
                    [
                        Other as u8,
                        Other as u8,
                        Other as u8,
                        Colon as u8,
                        Brackets as u8,
                        Other as u8,
                        Other as u8,
                        Other as u8,
                        Colon as u8,
                        Brackets as u8,
                        Other as u8,
                        Comma as u8,
                        Other as u8,
                        Brackets as u8,
                        Brackets as u8,
                        Control as u8
                    ]
                );
            }};
        }

        test_json!(absolut::one_cold, lookup_one_cold);
        test_json!(absolut::one_hot, lookup_one_hot);
        test_json!(absolut::general, lookup_general);
        test_json!(absolut::general(powers_of_two), lookup_general);
    }
}
