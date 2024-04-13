#![cfg_attr(doc_auto_cfg, feature(doc_auto_cfg))]
#![doc = include_str!("../../../README.md")]

extern crate self as absolut;

/// Composite algorithm.
///
/// This procedural macro expects an `enum` item where all variants are annotated with either the
/// `matches` attribute, or the `wilcard` attribute. It accepts no arguments.
///
/// The `matches` attribute takes a description of the set of bytes mapped to the corresponding enum
/// variant. This description takes one of the the following forms:
/// - A single byte literal, e.g. `#[matches(b'\xA')]`)
/// - A set of byte literals separated by a vartical bat, e.g. `#[matches(b'\xA' | b'\xD')]`
/// - An range of byte literals where the start (inclusive) and the end (exclusive) are join by two
///   dots, e.g. `#[matches(b'\xA'..b'\xD')]`
///
/// The `wilcard` attribute must be present on exactly one of the enum variants. It signifies the
/// value to which all non-matched bytes are mapped.
///
/// See [`Composite`] for more information.
pub use absolut_macros::composite;

/// One-cold algorithm.
///
/// This procedural macro expects an `enum` item where all variants are annotated with either the
/// `matches` attribute, or the `wilcard` attribute. It accepts no arguments.
///
/// The `matches` attribute takes a description of the set of bytes mapped to the corresponding enum
/// variant. This description takes one of the the following forms:
/// - A single byte literal, e.g. `#[matches(b'\xA')]`)
/// - A set of byte literals separated by a vartical bat, e.g. `#[matches(b'\xA' | b'\xD')]`
/// - An range of byte literals where the start (inclusive) and the end (exclusive) are join by two
///   dots, e.g. `#[matches(b'\xA'..b'\xD')]`
///
/// The `wilcard` attribute must be present on exactly one of the enum variants. It signifies the
/// value to which all non-matched bytes are mapped.
///
/// See [`OneCold`] for more information.
pub use absolut_macros::one_cold;

/// The One-hot algorithm.
///
/// This procedural macro expects an `enum` item where all variants are annotated with either the
/// `matches` attribute, or the `wilcard` attribute. It accepts no arguments.
///
/// The `matches` attribute takes a description of the set of bytes mapped to the corresponding enum
/// variant. This description takes one of the the following forms:
/// - A single byte literal, e.g. `#[matches(b'\xA')]`)
/// - A set of byte literals separated by a vartical bat, e.g. `#[matches(b'\xA' | b'\xD')]`
/// - An range of byte literals where the start (inclusive) and the end (exclusive) are join by two
///   dots, e.g. `#[matches(b'\xA'..b'\xD')]`
///
/// The `wilcard` attribute must be present on exactly one of the enum variants. It signifies the
/// value to which all non-matched bytes are mapped.
///
/// See [`OneHot`] for more information.
pub use absolut_macros::one_hot;

/// General algorithm.
///
/// This procedural macro expects an `enum` item where all variants are annotated with either the
/// `matches` attribute, or the `wilcard` attribute. It accepts no arguments.
///
/// The `matches` attribute takes a description of the set of bytes mapped to the corresponding enum
/// variant. This description takes one of the the following forms:
/// - A single byte literal, e.g. `#[matches(b'\xA')]`)
/// - A set of byte literals separated by a vartical bat, e.g. `#[matches(b'\xA' | b'\xD')]`
/// - An range of byte literals where the start (inclusive) and the end (exclusive) are join by two
///   dots, e.g. `#[matches(b'\xA'..b'\xD')]`
///
/// The `wilcard` attribute must be present on exactly one of the enum variants. It signifies the
/// value to which all non-matched bytes are mapped.
///
/// See [`General`] for more information.
#[cfg(feature = "sat")]
pub use absolut_macros::general;

/// Interface for tables generated by [`one_hot`].
///
/// The name "One-hot" refers to the [One-hot](https://en.wikipedia.org/wiki/One-hot) binary
/// encoding, where at most one bit is 1 and all the others are 0 in any given set of bits. For the
/// [`one_hot`] algorithm, all table outputs follow the One-hot encoding (thus being powers of two).
///
/// Consequently, the [`one_hot`] algorithm only supports tables whose range (i.e. the set of all
/// possible outputs) contains no more than 9 elements (including the wildcard).
///
/// Lookup table [`OneHot::TABLE_LOW_NIBBLES`] is used to perform a lookup on the low nibbles of
/// input bytes while [`OneHot::TABLE_HIGH_NIBBLES`] is used to perform a lookup on the high nibbles
/// of input bytes. The results of the two lookups are combined using a __bitwise AND__ operation to
/// yield the final lookup.
///
/// The following `lookup` function implements the lookup algorithm supported by all [`OneHot`]
/// tables (using non-vectorized code for demonstration purposes).
///
/// ```rust
/// use absolut::OneHot;
///
/// fn lookup<T: OneHot>(input: &[u8; 16], output: &mut [u8; 16]) {
///     for (index, byte) in input.iter().copied().enumerate() {
///         let (lo, hi) = (byte & 0b1111, byte >> 4);
///         output[index] = T::TABLE_LOW_NIBBLES[lo as usize]
///                       & T::TABLE_HIGH_NIBBLES[hi as usize];
///     }
/// }
///
/// #[absolut::one_hot]
/// #[derive(Debug)]
/// enum Table {
///     #[matches(b'A')] A,
///     #[matches(b'B')] B,
///     #[matches(b'C')] C,
///     #[wildcard] Other,
/// }
///
/// use Table::*;
/// let input = b"A___B___C___D___";
/// let expected_output = [
///     A as u8, Other as u8, Other as u8, Other as u8,
///     B as u8, Other as u8, Other as u8, Other as u8,
///     C as u8, Other as u8, Other as u8, Other as u8,
///     Other as u8, Other as u8, Other as u8, Other as u8,
/// ];
///
/// let mut output = [0; 16];
/// lookup::<Table>(&input, &mut output);
/// assert_eq!(&output, &expected_output);
/// ```
pub trait OneHot {
    /// Lookup table for the low nibbles of input bytes.
    const TABLE_LOW_NIBBLES: [u8; 16];
    /// Lookup table for the high nibbles of input bytes.
    const TABLE_HIGH_NIBBLES: [u8; 16];
}

/// Interface for tables generated by [`one_cold`].
///
/// The name `one_cold` refers to the [One-cold](https://en.wikipedia.org/wiki/One-hot) binary
/// encoding, where at most one bit is 0 and all the others are 1 in any given set of bits. For the
/// [`one_cold`] algorithm, all table outputs follow the One-cold encoding.
///
/// Consequently, the [`one_cold`] algorithm only supports tables whose range (i.e. the set of all
/// possible outputs) contains no more than 9 elements (including the wildcard).
///
/// Lookup table [`OneCold::TABLE_LOW_NIBBLES`] is used to perform a lookup on the low nibbles of
/// input bytes while [`OneCold::TABLE_HIGH_NIBBLES`] is used to perform a lookup on the high nibbles
/// of input bytes. The results of the two lookups are combined using a __bitwise OR__ operation to
/// yield the final lookup.
///
/// The following `lookup` function implements the lookup algorithm supported by all [`OneCold`]
/// tables (using non-vectorized code for demonstration purposes).
///
/// ```rust
/// use absolut::OneCold;
///
/// fn lookup<T: OneCold>(input: &[u8; 16], output: &mut [u8; 16]) {
///     for (index, byte) in input.iter().copied().enumerate() {
///         let (lo, hi) = (byte & 0b1111, byte >> 4);
///         output[index] = T::TABLE_LOW_NIBBLES[lo as usize]
///                       | T::TABLE_HIGH_NIBBLES[hi as usize];
///     }
/// }
///
/// #[absolut::one_cold]
/// #[derive(Debug)]
/// enum Table {
///     #[matches(b'E')] E,
///     #[matches(b'F')] F,
///     #[matches(b'G')] G,
///     #[wildcard] Other,
/// }
///
/// use Table::*;
/// let input = b"E___F___G___H___";
/// let expected_output = [
///     E as u8, Other as u8, Other as u8, Other as u8,
///     F as u8, Other as u8, Other as u8, Other as u8,
///     G as u8, Other as u8, Other as u8, Other as u8,
///     Other as u8, Other as u8, Other as u8, Other as u8,
/// ];
///
/// let mut output = [0; 16];
/// lookup::<Table>(&input, &mut output);
/// assert_eq!(&output, &expected_output);
/// ```
pub trait OneCold {
    /// Lookup table for the low nibbles of input bytes.
    const TABLE_LOW_NIBBLES: [u8; 16];
    /// Lookup table for the high nibbles of input bytes.
    const TABLE_HIGH_NIBBLES: [u8; 16];
}

/// Interface for tables generated by [`composite`].
///
/// This algorithm supports [arbitrary byte-to-byte
/// map](https://lemire.me/blog/2019/07/23/arbitrary-byte-to-byte-maps-using-arm-neon/) (i.e. every
/// possible map description has a solution). The downside of this approach is that it only works on
/// AArch64 as it requires four distinct 64-element lookup tables. Moreover, it requires four
/// distinct lookup tables which may lead to disappointing performance.
///
/// The following `lookup` function implements the lookup algorithm supported by all [`Composite`]
/// tables (using non-vectorized code for demonstration purposes).
///
/// ```rust
/// use absolut::Composite;
///
/// fn lookup<T: Composite>(input: &[u8; 16], output: &mut [u8; 16]) {
///     for (index, byte) in input.iter().copied().enumerate() {
///         output[index] = match byte {
///             0..=63 => T::TABLE_QUARTERS[0][byte as usize],
///             64..=127 => T::TABLE_QUARTERS[1][byte as usize - 64],
///             128..=191 => T::TABLE_QUARTERS[2][byte as usize - 128],
///             192..=255 => T::TABLE_QUARTERS[3][byte as usize - 192],
///         };
///     }
/// }
///
/// #[absolut::composite]
/// #[derive(Debug)]
/// enum Table {
///     #[matches(b'I')] I,
///     #[matches(b'J')] J,
///     #[matches(b'K')] K,
///     #[wildcard] Other,
/// }
///
/// use Table::*;
/// let input = b"I___J___K___L___";
/// let expected_output = [
///     I as u8, Other as u8, Other as u8, Other as u8,
///     J as u8, Other as u8, Other as u8, Other as u8,
///     K as u8, Other as u8, Other as u8, Other as u8,
///     Other as u8, Other as u8, Other as u8, Other as u8,
/// ];
///
/// let mut output = [0; 16];
/// lookup::<Table>(&input, &mut output);
/// assert_eq!(&output, &expected_output);
/// ```
pub trait Composite {
    /// Four lookup tables of size `64`. For each index `i` in the range `0..4`, lookup table
    /// `TABLE_QUARTERS[i]` is responsible for mapping the byte range `(64 * i)..(64 * (i + 1))`.
    const TABLE_QUARTERS: [[u8; 64]; 4];
}

/// Interface for tables generated by [`general`].
///
/// Similarly to [`OneHot`], lookup table [`General::TABLE_LOW_NIBBLES`] is used to perform a lookup
/// on the low nibbles of input bytes while [`General::TABLE_HIGH_NIBBLES`] is used to perform a
/// lookup on the high nibbles of input bytes. The results of the two lookups are combined using a
/// __bitwise AND__ operation to yield the final lookup.
///
/// However, lookup tables generated by [`general`] do not follow the One-hot encoding. Instead, a
/// SAT solver is used to compute the lookup table. Thus supporting more than 9 possible output
/// values that needn't necessarily be powers of two.
///
/// Consequently, [`general`] may find the a lookup table satisfiable when [`one_hot`] doesn't.
///
/// The following `lookup` function implements the lookup algorithm supported by all [`General`] (and [`OneHot`])
/// tables (using non-vectorized code for demonstration purposes).
///
/// ```rust
/// use absolut::General;
///
/// fn lookup<T: General>(input: &[u8; 16], output: &mut [u8; 16]) {
///     for (index, byte) in input.iter().copied().enumerate() {
///         let (lo, hi) = (byte & 0b1111, byte >> 4);
///         output[index] = T::TABLE_LOW_NIBBLES[lo as usize]
///                       & T::TABLE_HIGH_NIBBLES[hi as usize];
///     }
/// }
///
/// #[absolut::general]
/// #[derive(Debug)]
/// enum Table {
///     #[matches(b'M')] M,
///     #[matches(b'N')] N,
///     #[matches(b'O')] O,
///     #[wildcard] Other,
/// }
///
/// use Table::*;
/// let input = b"M___N___O___P___";
/// let expected_output = [
///     M as u8, Other as u8, Other as u8, Other as u8,
///     N as u8, Other as u8, Other as u8, Other as u8,
///     O as u8, Other as u8, Other as u8, Other as u8,
///     Other as u8, Other as u8, Other as u8, Other as u8,
/// ];
///
/// let mut output = [0; 16];
/// lookup::<Table>(&input, &mut output);
/// assert_eq!(&output, &expected_output);
/// ```
#[cfg(feature = "sat")]
pub trait General {
    /// Lookup table for the low nibbles of input bytes.
    const TABLE_LOW_NIBBLES: [u8; 16];
    /// Lookup table for the high nibbles of input bytes.
    const TABLE_HIGH_NIBBLES: [u8; 16];
}
