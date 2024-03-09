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
    #[cfg(target_arch = "aarch64")]
    #[target_feature(enable = "neon")]
    unsafe fn lookup_neon<const ONE_HOT: bool, Table: absolut::SimdTable<16>>(
        input: &[u8; 16],
    ) -> [u8; 16] {
        use std::arch::aarch64::{
            vandq_u8, vdupq_n_u8, vld1q_u8, vorrq_u8, vqtbl1q_u8, vshrq_n_u8, vst1q_u8,
        };

        let v_input = vld1q_u8(input.as_ptr());

        let v_table_lo = vld1q_u8(Table::LO.as_ptr());
        let v_table_hi = vld1q_u8(Table::HI.as_ptr());

        let v_input_lo = vandq_u8(v_input, vdupq_n_u8(0b1111));
        let v_input_hi = vshrq_n_u8::<4>(v_input);

        let v_lookup_lo = vqtbl1q_u8(v_table_lo, v_input_lo);
        let v_lookup_hi = vqtbl1q_u8(v_table_hi, v_input_hi);

        let v_lookup = if ONE_HOT {
            vandq_u8(v_lookup_lo, v_lookup_hi)
        } else {
            vorrq_u8(v_lookup_lo, v_lookup_hi)
        };

        let mut lookup = [0; 16];
        vst1q_u8(lookup.as_mut_ptr(), v_lookup);
        lookup
    }

    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    #[target_feature(enable = "sse2")]
    unsafe fn lookup_ssse3<const ONE_HOT: bool, Table: absolut::SimdTable<16>>(
        input: &[u8; 16],
    ) -> [u8; 16] {
        #[cfg(target_arch = "x86")]
        use std::arch::x86 as arch;

        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64 as arch;

        use arch::{
            _mm_and_si128, _mm_loadu_si128, _mm_or_si128, _mm_set1_epi8, _mm_shuffle_epi8,
            _mm_srli_epi32, _mm_storeu_si128,
        };

        let v_input = _mm_loadu_si128(input.as_ptr().cast());

        let v_table_lo = _mm_loadu_si128(Table::LO.as_ptr().cast());
        let v_table_hi = _mm_loadu_si128(Table::HI.as_ptr().cast());

        let v_input_lo = v_input;
        let v_input_hi = _mm_and_si128(_mm_srli_epi32::<4>(v_input), _mm_set1_epi8(0b1111111));

        let v_lookup_lo = _mm_shuffle_epi8(v_table_lo, v_input_lo);
        let v_lookup_hi = _mm_shuffle_epi8(v_table_hi, v_input_hi);

        let v_lookup = if ONE_HOT {
            _mm_and_si128(v_lookup_lo, v_lookup_hi)
        } else {
            // ONE_COLD
            _mm_or_si128(v_lookup_lo, v_lookup_hi)
        };

        let mut lookup = [0; 16];
        _mm_storeu_si128(lookup.as_mut_ptr().cast(), v_lookup);
        lookup
    }

    fn lookup_fallback<const ONE_HOT: bool, Table: absolut::SimdTable<16>>(
        input: &[u8; 16],
    ) -> [u8; 16] {
        let mut lookup = [0; 16];

        for (index, byte) in input.iter().copied().enumerate() {
            let lo = byte & 0b1111;
            let hi = byte >> 4;

            lookup[index] = if ONE_HOT {
                Table::LO[lo as usize] & Table::HI[hi as usize]
            } else {
                // ONE_COLD
                Table::LO[lo as usize] | Table::HI[hi as usize]
            };
        }

        lookup
    }

    fn lookup<const ONE_HOT: bool, Table: absolut::SimdTable<16>>(input: &[u8; 16]) -> [u8; 16] {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            if std::arch::is_x86_feature_detected!("ssse3") {
                unsafe { lookup_ssse3::<ONE_HOT, Table>(input) }
            } else {
                lookup_fallback::<ONE_HOT, Table>(input)
            }
        }
        #[cfg(target_arch = "aarch64")]
        {
            if std::arch::is_aarch64_feature_detected!("neon") {
                unsafe { lookup_neon::<ONE_HOT, Table>(input) }
            } else {
                lookup_fallback::<ONE_HOT, Table>(input)
            }
        }
        #[cfg(not(any(target_arch = "x86", target_arch = "x86_64", target_arch = "aarch64")))]
        {
            lookup_fallback::<ONE_HOT, Table>(input)
        }
    }

    fn lookup_one_hot<Table: absolut::SimdTable<16>>(input: &[u8; 16]) -> [u8; 16] {
        lookup::<true, Table>(input)
    }
    fn lookup_one_cold<Table: absolut::SimdTable<16>>(input: &[u8; 16]) -> [u8; 16] {
        lookup::<false, Table>(input)
    }

    #[test]
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
                    $lookup::<Table>(&input),
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
        test_ident!(absolut::general, lookup_one_hot);
        test_ident!(absolut::general(powers_of_two), lookup_one_hot);
    }

    #[test]
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
                    $lookup::<Table>(&input),
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
        test_json!(absolut::general, lookup_one_hot);
        test_json!(absolut::general(powers_of_two), lookup_one_hot);
    }
}
