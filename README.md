# Absolut

Absolut stands for "**A**utogenerated **B**ytewise **S**IMD-**O**ptimized **L**ook-**U**p **T**ables".
The following is a breakdown of this jargon:

- **Bytewise Lookup Table**: One-to-one mappings between sets of bytes.

- **SIMD-Optimized**: Said lookup tables are implemented using SIMD (Single Instruction Multipe Data) 
  instructions, such as `PSHUFB` on x86_64 and `TBL` on AArch64.

- **Autogenerated**: This crate utilizes [Procedural Macros](https://doc.rust-lang.org/reference/procedural-macros.html) 
  to generate (if possible) SIMD lookup tables given a human-readable byte-to-byte mapping.

## Why?

SIMD instructions allow for greater data parallelism when performing table lookups on bytes. This is
has proved incredibly useful for high-performance data processing[^1].

Unfortunately, SIMD table lookup instructions (or byte suffling instructions) operate on tables too small
to cover the entire 8-bit integer space. These tables typically have a size of 16 on x86_64, while
on AArch64 tables of up to 64 elements are supported.

This library facilitates generation of SIMD lookup tables from high-level descriptions of byte-to-byte mappings.
The goal being to avoid the need to 
[hardcode manually-computed](https://github.com/simd-lite/simd-json/blob/main/src/impls/sse42/stage1.rs#L22) 
SIMD lookup tables, thus enabling a wider audience to utilize these techniques more easily.

## How?

Absolut is essentially a set of Procedural Macros that accept byte-to-byte mapping descriptions in the form
of Rust enums:

```rust
#[absolut::one_hot]
pub enum JsonTable {
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
```

The above `JsonTable` enum encodes the following one-to-one mapping:

| Input                    | Output   |
|------------------------- |----------|
| `0x2C`                   | Comma    |
| `0x3A`                   | Colon    |
| `0x5B, 0x5D, 0x7B, 0x7D` | Brackets |
| `0xD, 0xA, 0x9`          | Control  |
| `0x20`                   | Space    |
| `*`                      | Other    |

Where `*` denotes all other bytes not explicitly mapped.

Mapping results needn't be explictly defined as Absolut will solve for them automatically.
In the previous code snippet, the expression `JsonTable::Space as u8` evaluates to the
output byte when performing a table lookup on `0x20`.

[^1]: [Parsing Gigabytes of JSON per Second](https://arxiv.org/abs/1902.08318)