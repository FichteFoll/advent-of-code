#![feature(test)]

use std::collections::VecDeque;

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
    (1e14 as i64..1e15 as i64).rev()
        .map(|n| (n, format!("{n}").bytes().map(|b| (b - b'0') as i64).collect::<Vec<_>>()))
        .filter(|(_, digits)| digits.iter().any(|&d| d == 0))
        .find(|(n, digits)| {
            if n % (1e10 as i64) == 0 {
                println!("{n}");
            }
            let mut monad = Monad::new(parsed, digits);
            monad.compute();
            monad.registers[3] == 0
        })
        .unwrap().0
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

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
    extern crate test;

    // const TEST_INPUT: &str = "\
    //     ";

    // test!(part_1() == 0);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 252);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);

    mod monad {
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
