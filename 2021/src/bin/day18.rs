#![feature(if_let_guard)]
#![feature(test)]

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 18;

type Parsed = Vec<SnailNum>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

#[derive(Debug, PartialEq, Eq)]
pub enum SnailNum {
    Terminal(usize),
    Pair(Box<SnailNum>, Box<SnailNum>),
}


use SnailNum::{Terminal, Pair};

mod parse {
    use core::panic;
    use std::str::FromStr;
    use thiserror::Error;
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
        #[error("Bad number")]
        BadInt(#[from] ParseIntError),
        #[error("Unclosed pair")]
        Unclosed,
        #[error("Bad Pair; expected 2, found {0}")]
        BadCount(usize),
        #[error("Closing bracket without opening")]
        UnexpectedClose,
        #[error("Input was empty")]
        Empty,
        #[error("Unrecognized character {0:?}")]
        BadChar(char),
        #[error("Expected '[', found {0:?}")]
        ExpectedOpen(char),
    }

    impl FromStr for SnailNum {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut stack: Vec<Vec<SnailNum>> = vec![vec![]];
            let mut digits: Vec<char> = vec![];

            fn drain_digits(digits: &mut Vec<char>) -> Result<SnailNum, ParseError> {
                Ok(Terminal((&digits.drain(..).collect::<String>()).parse()?))
            }

            for c in s.chars() {
                match (c, stack.last_mut()) {
                    ('[', _) =>
                        stack.push(vec![]),
                    (_, None) =>
                        return Err(ParseError::ExpectedOpen(c)),
                    (']', Some(curr)) => {
                        if !digits.is_empty() {
                            curr.push(drain_digits(&mut digits)?);
                        }
                        if curr.len() != 2 {
                            return Err(ParseError::BadCount(curr.len()));
                        }
                        let mut iter = stack.pop().unwrap().into_iter();
                        let new = Pair(iter.next().unwrap().into(), iter.next().unwrap().into());
                        if stack.len() == 1 {
                            return Ok(new);
                        } else {
                            stack.last_mut().ok_or(ParseError::UnexpectedClose)?.push(new);
                        }
                    },
                    (',', Some(curr)) if !digits.is_empty() =>
                        curr.push(drain_digits(&mut digits)?),
                    (',', _) => (),
                    _ if c.is_digit(10) =>
                        digits.push(c),
                    _ =>
                        return Err(ParseError::BadChar(c)),
                }
            }
            Err(ParseError::Empty)
        }
    }
}

fn part_1(parsed: &Parsed) -> usize {
    0
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

#[cfg(test)]
mod tests {

    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        ";

    mod parse {
        mod error {
            use crate::*;
            use crate::parse::ParseError;

            #[test]
            fn unclosed() {
                let result = "[".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::Unclosed));
            }

            #[test]
            fn unexpected_close() {
                let result = "[1,2]]".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::UnexpectedClose));
            }

            #[test]
            fn too_few() {
                let result = "[1]".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::BadCount(1)));
            }

            #[test]
            fn too_many() {
                let result = "[1,2,3]".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::BadCount(3)));
            }

            #[test]
            fn empty() {
                let result = "".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::Empty));
            }

            #[test]
            fn expected_open() {
                let result = "12".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::ExpectedOpen('1')));
            }

            #[test]
            fn bad_char() {
                let result = "[a,2]".parse::<SnailNum>();
                assert_eq!(result, Err(ParseError::BadChar('a')));
            }
        }

        mod success {
            use crate::*;

            #[test]
            fn one_level() {
                let result = "[1,2]".parse::<SnailNum>();
                assert_eq!(result, Ok(Pair(Terminal(1).into(), Terminal(2).into())));
            }

            #[test]
            fn two_levels() {
                let result = "[[1,2],3]".parse::<SnailNum>();
                assert_eq!(result, Ok(Pair(
                    Pair(Terminal(1).into(), Terminal(2).into()).into(),
                    Terminal(3).into()
                )));
            }

            #[test]
            fn four_levels() {
                let result = "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]".parse::<SnailNum>();
                assert_eq!(result, Ok(Pair(
                    Pair(
                        Pair(
                            Pair(Terminal(1).into(), Terminal(2).into()).into(),
                            Pair(Terminal(3).into(), Terminal(4).into()).into(),
                        ).into(),
                        Pair(
                            Pair(Terminal(5).into(), Terminal(6).into()).into(),
                            Pair(Terminal(7).into(), Terminal(8).into()).into(),
                        ).into(),
                    ).into(),
                    Terminal(9).into()
                )));
            }
        }
    }

    test!(part_1() == 4140);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 100);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
