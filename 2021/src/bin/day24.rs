#![feature(bool_to_option)]
#![feature(test)]

use thiserror::Error;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 24;

type Parsed = Vec<Instr>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instr {
    Inp(Param),
    Add(Param, Param),
    Mul(Param, Param),
    Div(Param, Param),
    Mod(Param, Param),
    Eql(Param, Param),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Param {
    Register(usize),
    Literal(i64),
}

use Instr::*;
use Param::*;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use std::str::FromStr;
    use std::num::ParseIntError;

    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .lines()
            .map(|line| line.parse().unwrap())
            .collect()
    }

    #[derive(Error, Debug, PartialEq, Eq)]
    pub enum ParseError {
        #[error("Unable to parse line {0:?}")]
        BadLine(String),
        #[error("Unable to parse number: {0:?}")]
        BadNum(#[from] ParseIntError),
        #[error("Invalid Argument; expected register: {0:?}")]
        ExpectedRegister(Param),
    }

    use ParseError::*;

    impl FromStr for Instr {
        type Err = ParseError;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (cmd, rest) = s.split_once(' ').ok_or_else(|| BadLine(s.to_string()))?;
            let params: Vec<Param> = rest.split_ascii_whitespace().map(|w| w.parse()).collect::<Result<_, _>>()?;
            Ok(match (cmd, &params[..]) {
                (_, &[a @ Param::Literal(_), ..]) => return Err(ExpectedRegister(a)),
                ("inp", &[a])    => Self::Inp(a),
                ("add", &[a, b]) => Self::Add(a, b),
                ("mul", &[a, b]) => Self::Mul(a, b),
                ("div", &[a, b]) => Self::Div(a, b),
                ("mod", &[a, b]) => Self::Mod(a, b),
                ("eql", &[a, b]) => Self::Eql(a, b),
                _ => return Err(BadLine(s.to_string())),
            })
        }
    }

    impl FromStr for Param {
        type Err = ParseError;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s.as_bytes() {
                &[r] if r >= b'w' => Self::Register((r - b'w') as usize),
                _ => Self::Literal(s.parse()?),
            })
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn literal() {
            assert_eq!("1".parse(), Ok(Literal(1)));
            assert_eq!("-1".parse(), Ok(Literal(-1)));
            assert_eq!("1111".parse(), Ok(Literal(1111)));
            let invalid_digit = "a".parse::<i64>().unwrap_err();
            assert_eq!("a".parse::<Param>(), Err(BadNum(invalid_digit)));
        }

        #[test]
        fn instr() {
            assert_eq!("inp w".parse(), Ok(Inp(Register(0))));
            assert_eq!("add z 20".parse(), Ok(Add(Register(3), Literal(20))));
            assert_eq!("eql w w".parse(), Ok(Eql(Register(0), Register(0))));
            assert_eq!("eql 10 w".parse::<Instr>(), Err(ExpectedRegister(Literal(10))));
        }
    }
}

fn part_1(parsed: &Parsed) -> i64 {
    let params = extract_params(parsed);
    find_input(&params, 0, 0).unwrap().into_iter()
        .rev()
        .fold(0, |acc, digit| acc * 10 + digit)
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

// All iterations follow the same pattern and differ in 3 numbers.
// Extract these here so we can run a simplified version of the code
// with early exits for invalid starting combinations
// that will never result in a valid number.
#[derive(Debug)]
struct StepParams {
    div_z: i64,
    add_x: i64,
    add_y: i64,
}

const BLOCK_SIZE: usize = 18;
const BLOCK_COUNT: usize = 14;

fn extract_params(parsed: &Parsed) -> Vec<StepParams> {
    assert_eq!(parsed.len(), BLOCK_COUNT * BLOCK_SIZE, "Input didn't match expected format");
    (0..BLOCK_COUNT)
        .map(|i| {
            let div_z = match parsed[i * BLOCK_SIZE + 4] {
                Div(Register(3), Literal(n)) => n,
                instr => panic!("Expected `div z …` at pos {}, found: {:?}", i * BLOCK_SIZE + 4, instr),
            };
            let add_x = match parsed[i * BLOCK_SIZE + 5] {
                Add(Register(1), Literal(n)) => n,
                instr => panic!("Expected `add x …` at pos {}, found: {:?}", i * BLOCK_SIZE + 5, instr),
            };
            let add_y = match parsed[i * BLOCK_SIZE + 15] {
                Add(Register(2), Literal(n)) => n,
                instr => panic!("Expected `add y …` at pos {}, found: {:?}", i * BLOCK_SIZE + 15, instr),
            };
            StepParams { div_z, add_x, add_y }
        }).collect()
}

fn find_input(params: &[StepParams], index: usize, z: i64) -> Option<Vec<i64>> {
    if index == BLOCK_COUNT {
        (z == 0).then(|| vec![])
    } else {
        (1..=9).rev()
            .find_map(|i| {
                calc_block(&params[index], z, i)
                    .and_then(|new_z| find_input(params, index + 1, new_z))
                    .map(|mut res| { res.push(i); res })
            })
    }
}

fn calc_block(params: &StepParams, z: i64, input: i64) -> Option<i64> {
    assert_eq!(params.div_z == 26, params.add_x < 10);
    // In each iteration, there is a check for whether the input matches
    // the carry modulo 26 plus add_x.
    // If it matches, the carry (z) is multiplied with 26,
    // then the input + some constant is added.
    //
    // Throughout the entire code base, z is divided by 26 7 times
    // (and by 1 the other 7 times),
    // in the same blocks where add_x is smaller than 10,
    // i.e. there is a possibility of z *not* being multiplied by 26.
    // Thus it must hold that the inputs for these blocks prevent a multiplication,
    // otherwise we will never reach 0 at the end.
    //
    // FWIW, add_x is never in 0..10, at least in my input.
    if params.add_x < 10 {
        (z % 26 + params.add_x == input).then_some(z / params.div_z)
    } else {
        Some(z * 26 + input + params.add_y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    bench_parse!(Vec::len, 252);
    bench!(part_1() == 99995969919326);
    // bench!(part_2() == 0);

}

// Treating the given code as a blackbox and passing all inputs into it is obviously infeasible,
// but I wrote it anyway (with tests), so I might as well keep it.
// It's just not used in the actual solution.
#[cfg(test)]
mod monad {
    use super::*;

    use std::collections::VecDeque;


    struct Monad {
        registers: [i64; 4],
        instructions: Vec<Instr>,
        input: VecDeque<i64>, // pops front
    }

    impl Monad {
        fn new(instrs: &[Instr], input: &[i64]) -> Self {
            Monad {
                registers: [0; 4],
                instructions: instrs.into(),
                input: input.iter().cloned().collect(),
            }
        }

        fn compute(&mut self) {
            for instr in self.instructions.clone() {
                self.step(instr);
                // println!("registers: {:?}", self.registers);
            }
        }

        fn step(&mut self, instr: Instr) {
            // println!("executing {instr:?}");
            match instr {
                Inp(Register(r)) => self.registers[r] = self.input.pop_front().expect("ran out of input"),
                Add(Register(r), param) => self.registers[r] += self.read(param),
                Mul(Register(r), param) => self.registers[r] *= self.read(param),
                Div(Register(r), param) => self.registers[r] /= self.read(param),
                Mod(Register(r), param) => self.registers[r] %= self.read(param),
                Eql(Register(r), param) => self.registers[r] = (self.registers[r] == self.read(param)) as i64,
                _ => unreachable!(),
            }
        }

        fn read(&self, param: Param) -> i64 {
            match param {
                Literal(n) => n,
                Register(r) => self.registers[r],
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn negate() {
            let instrs = parse_input("inp x\nmul x -1");
            let mut monad = Monad::new(&instrs, &[100]);
            monad.compute();
            assert_eq!(monad.registers[1], -100);
        }

        #[test]
        fn gt_times_3() {
            let instrs = parse_input("inp z\ninp x\nmul z 3\neql z x");
            let mut monad = Monad::new(&instrs, &[10, 10]);
            monad.compute();
            assert_eq!(monad.registers[3], 0);

            let mut monad = Monad::new(&instrs, &[10, 30]);
            monad.compute();
            assert_eq!(monad.registers[3], 1);
        }

        #[test]
        fn split_last_4_bits() {
            let instrs = parse_input("\
                inp w\n\
                add z w\n\
                mod z 2\n\
                div w 2\n\
                add y w\n\
                mod y 2\n\
                div w 2\n\
                add x w\n\
                mod x 2\n\
                div w 2\n\
                mod w 2\n\
                ");
            let mut monad = Monad::new(&instrs, &[15]);
            monad.compute();
            assert_eq!(monad.registers, [1, 1, 1, 1]);

            let mut monad = Monad::new(&instrs, &[6]);
            monad.compute();
            assert_eq!(monad.registers, [0, 1, 1, 0]);
        }
    }
}
