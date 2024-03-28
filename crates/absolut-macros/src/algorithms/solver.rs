use std::{array, collections::HashSet};

use varisat::{ExtendFormula, Lit, Solver, Var};

pub struct Context<'ctx>(Solver<'ctx>);

impl Context<'_> {
    pub fn new() -> Self {
        Self(Solver::new())
    }

    pub fn assert_andeq<const N: usize>(&mut self, lhs: &Bits<N>, rhs: &Bits<N>, res: &Bits<N>) {
        // In general, any equation of the form X & Y = Z where X, Y, Z are bits (or booleans)
        // is equivalent the following boolean clauses in propositional logic:
        // ~X | ~Y | Z
        // X | ~Z
        // Y | ~Z
        for ((lhs, rhs), res) in lhs.0.iter().zip(rhs.0.iter()).zip(res.0.iter()) {
            self.0
                .add_clause(&[lhs.negative(), rhs.negative(), res.positive()]);
            self.0.add_clause(&[lhs.positive(), res.negative()]);
            self.0.add_clause(&[rhs.positive(), res.negative()]);
        }
    }

    pub fn assert_noteq<const N: usize>(&mut self, lhs: &Bits<N>, rhs: &Bits<N>) {
        // Go through all possible 8-bit integers and for each such interger,
        // add a clause that prohibits both lhs and rhs to be be equal to it
        let mut clause = Vec::with_capacity(N + N);
        for b in 0..(1 << N) {
            for i in 0..N {
                if (1 << i) & b == 0 {
                    // The i-th bit of this integer is unset,
                    // thus we mean to guard against both lhs and rhs having their i-th bits unset
                    // thus we want the negation of (NOT lhs[i] AND NOT rhs[i])
                    // i.e. (lhs[i] OR rhs[i])
                    clause.push(lhs.0[i].positive());
                    clause.push(rhs.0[i].positive());
                } else {
                    // The i-th bit of this integer is set,
                    // thus we mean to guard against both lhs and rhs having their i-th bits set
                    // thus we want the negation of (lhs[i] AND rhs[i])
                    // i.e. (NOT lhs[i] OR NOT rhs[i])
                    clause.push(lhs.0[i].negative());
                    clause.push(rhs.0[i].negative());
                }
            }
            self.0.add_clause(&clause);
            clause.clear();
        }
    }

    pub fn assert_const<const N: usize>(&mut self, lhs: &Bits<N>, rhs: u8) {
        for i in 0..N {
            self.0.add_clause(&[lhs.0[i].lit((1 << i) & rhs != 0)]);
        }
    }

    #[allow(dead_code)]
    pub fn assert_power_of_two<const N: usize>(&mut self, bits: &Bits<N>) {
        // Assert that no two bits are set at the same time
        for i in 0..N {
            for j in 0..N {
                if i != j {
                    self.0
                        .add_clause(&[bits.0[i].negative(), bits.0[j].negative()]);
                }
            }
        }

        // Assert that not all bit are unset
        self.0.add_clause(&bits.0.map(|var| var.positive()))
    }

    pub fn model(&mut self) -> Option<Model> {
        match self.0.solve() {
            Ok(true) => Some(Model(HashSet::from_iter(self.0.model().unwrap()))),
            Ok(false) | Err(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct Model(HashSet<Lit>);

impl Model {
    pub fn eval_bits<const N: usize>(&self, bits: &Bits<N>) -> u8 {
        bits.0.iter().enumerate().fold(0, |acc, (i, var)| {
            if self.0.contains(&var.positive()) {
                acc | (1 << i)
            } else {
                acc
            }
        })
    }

    pub fn eval_bits_array<const N: usize, const M: usize>(
        &self,
        bits_array: BitsArray<N, M>,
    ) -> [u8; M] {
        bits_array.0.map(|bits| self.eval_bits(&bits))
    }
}

// const BITS_SIZE: usize = 8;
#[derive(Debug)]
pub struct Bits<const N: usize>([Var; N]);

impl<const N: usize> Bits<N> {
    pub fn new(ctx: &mut Context) -> Self {
        let mut vars = ctx.0.new_var_iter(N);
        Self(array::from_fn(|_| vars.next().unwrap()))
    }
}

// const BITS_ARRAY_SIZE: usize = 16;
#[derive(Debug)]
pub struct BitsArray<const N: usize, const M: usize>([Bits<N>; M]);

impl<const N: usize, const M: usize> BitsArray<N, M> {
    pub fn new(ctx: &mut Context) -> Self {
        Self(array::from_fn(|_| Bits::new(ctx)))
    }

    pub fn get(&self, index: usize) -> Option<&Bits<N>> {
        self.0.get(index)
    }
}

#[cfg(test)]
mod tests {
    use super::{Bits, Context};

    #[test]
    fn test_noteq() {
        let mut ctx = Context::new();

        let b0 = Bits::<8>::new(&mut ctx);
        let b1 = Bits::<8>::new(&mut ctx);

        ctx.assert_noteq(&b0, &b1);

        let model = ctx.model().unwrap();

        assert_ne!(model.eval_bits(&b0), model.eval_bits(&b1))
    }
}
