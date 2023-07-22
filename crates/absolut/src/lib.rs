extern crate self as absolut;

pub use absolut_macros::simd_table;

pub trait SimdTable<const LANES: usize> {
    const LO: [u8; LANES];
    const HI: [u8; LANES];
}

#[cfg(test)]
mod tests {
    // FIXME(fuzzypixelz): add x86_64 tests

    use super::{simd_table, SimdTable};

    #[simd_table(LANES = 16, powers_of_two)]
    #[derive(Debug)]
    pub enum TestSimdTable16 {
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
        Other = 0,
    }

    #[test]
    #[cfg(all(target_arch = "aarch64", target_feature = "neon"))]
    fn test_lookup16_neon() {
        use std::arch::aarch64::{
            vandq_u8, vdupq_n_u8, vld1q_u8, vqtbl1q_u8, vshrq_n_u8, vst1q_u8,
        };

        const LANES: usize = 16;

        let input = b"'o':{'k':[1,2]}\n";

        let lookup = unsafe {
            let v_input = vld1q_u8(input.as_ptr());

            let v_table_lo = vld1q_u8(TestSimdTable16::LO.as_ptr());
            let v_table_hi = vld1q_u8(TestSimdTable16::HI.as_ptr());

            let v_input_lo = vandq_u8(v_input, vdupq_n_u8(0b1111));
            let v_input_hi = vshrq_n_u8::<4>(v_input);

            let v_lookup_lo = vqtbl1q_u8(v_table_lo, v_input_lo);
            let v_lookup_hi = vqtbl1q_u8(v_table_hi, v_input_hi);

            let v_lookup = vandq_u8(v_lookup_lo, v_lookup_hi);
            let mut lookup = [0; LANES];
            vst1q_u8(lookup.as_mut_ptr(), v_lookup);

            lookup
        };

        use TestSimdTable16::*;

        assert_eq!(
            lookup,
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
    }
}
