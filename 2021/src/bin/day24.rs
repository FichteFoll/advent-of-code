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
    Register(u8),
    Literal(i64),
}

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

    impl FromStr for Instr {
        type Err = ParseError;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (cmd, rest) = s.split_once(' ').ok_or_else(|| ParseError::BadLine(s.to_string()))?;
            let params: Vec<Param> = rest.split_ascii_whitespace().map(|w| w.parse()).collect::<Result<_, _>>()?;
            Ok(match (cmd, &params[..]) {
                (_, &[a @ Param::Literal(_), ..]) => return Err(ParseError::ExpectedRegister(a)),
                ("inp", &[a])    => Self::Inp(a),
                ("add", &[a, b]) => Self::Add(a, b),
                ("mul", &[a, b]) => Self::Mul(a, b),
                ("div", &[a, b]) => Self::Div(a, b),
                ("mod", &[a, b]) => Self::Mod(a, b),
                ("eql", &[a, b]) => Self::Eql(a, b),
                _ => return Err(ParseError::BadLine(s.to_string())),
            })
        }
    }

    impl FromStr for Param {
        type Err = ParseError;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(match s.as_bytes() {
                &[r] if r >= b'w' => Self::Register(r - b'w'),
                _ => Self::Literal(s.parse()?),
            })
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn literal() {
            assert_eq!("1".parse(), Ok(Param::Literal(1)));
            assert_eq!("-1".parse(), Ok(Param::Literal(-1)));
            assert_eq!("1111".parse(), Ok(Param::Literal(1111)));
            let invalid_digit = "a".parse::<i64>().unwrap_err();
            assert_eq!("a".parse::<Param>(), Err(ParseError::BadNum(invalid_digit)));
        }

        #[test]
        fn instr() {
            assert_eq!("inp w".parse(), Ok(Instr::Inp(Param::Register(0))));
            assert_eq!("add z 20".parse(), Ok(Instr::Add(Param::Register(3), Param::Literal(20))));
            assert_eq!("eql w w".parse(), Ok(Instr::Eql(Param::Register(0), Param::Register(0))));
            assert_eq!("eql 10 w".parse::<Instr>(), Err(ParseError::ExpectedRegister(Param::Literal(10))));
        }
    }
}

fn part_1(_parsed: &Parsed) -> usize {
    todo!()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        ";

    test!(part_1() == 0);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 252);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
