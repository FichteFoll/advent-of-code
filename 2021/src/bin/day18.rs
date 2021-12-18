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
    use std::str::FromStr;
    use thiserror::Error;

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
        #[error("Unclosed pair")]
        Unclosed,
        #[error("Bad Pair; expected 2, found {0}")]
        BadCount(usize),
        #[error("Expected end of line")]
        ExpectedEol,
        #[error("Input was empty")]
        Empty,
        #[error("Unrecognized character {0:?}")]
        BadChar(char),
        #[error("Expected '[', found {0:?}")]
        ExpectedOpen(char),
        #[error("Unexpected '['")]
        UnexpectedOpen,
    }

    impl FromStr for SnailNum {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut stack: Vec<Vec<SnailNum>> = vec![];
            let mut digits: Vec<char> = vec![];

            fn drain_digits(digits: &mut Vec<char>) -> SnailNum {
                Terminal((&digits.drain(..).collect::<String>()).parse().unwrap())
            }

            let mut chars = s.chars();
            while let Some(c) = chars.next() {
                match (c, stack.last_mut()) {
                    ('[', _) if !digits.is_empty() =>
                        return Err(ParseError::UnexpectedOpen),
                    ('[', _) =>
                        stack.push(vec![]),
                    (_, None) =>
                        return Err(ParseError::ExpectedOpen(c)),
                    (']', Some(curr)) => {
                        if !digits.is_empty() {
                            curr.push(drain_digits(&mut digits));
                        }
                        if curr.len() != 2 {
                            return Err(ParseError::BadCount(curr.len()));
                        }
                        let mut iter = stack.pop().unwrap().into_iter();
                        let new = Pair(iter.next().unwrap().into(), iter.next().unwrap().into());
                        if let Some(prev) = stack.last_mut() {
                            prev.push(new);
                        } else if chars.next().is_some() {
                            return Err(ParseError::ExpectedEol)
                        } else {
                            return Ok(new);
                        }
                    },
                    (',', Some(curr)) if !digits.is_empty() =>
                        curr.push(drain_digits(&mut digits)),
                    (',', _) => (),
                    _ if c.is_digit(10) =>
                        digits.push(c),
                    _ =>
                        return Err(ParseError::BadChar(c)),
                }
            }
            if stack.pop().is_some() {
                Err(ParseError::Unclosed)
            } else {
                Err(ParseError::Empty)
            }
        }
    }

    #[cfg(test)]
    mod tests {
        mod error {
            use super::super::{*, ParseError::*};

            macro_rules! test_err {
                ($name:ident, $str:expr, $err:expr) => {
                    #[test]
                    fn $name() {
                        let result = $str.parse::<SnailNum>();
                        assert_eq!(result, Err($err));
                    }
                }
            }

            test_err!(unclosed, "[", Unclosed);
            test_err!(expected_open_1, "]", ExpectedOpen(']'));
            test_err!(expected_open_2, "12", ExpectedOpen('1'));
            test_err!(expected_eol, "[1,2]]", ExpectedEol);
            test_err!(too_few, "[1]", BadCount(1));
            test_err!(too_many, "[1,2,3]", BadCount(3));
            test_err!(empty, "", Empty);
            test_err!(bad_char, "[a,2]", BadChar('a'));
            test_err!(unexpected_open, "[12[", UnexpectedOpen);
        }

        mod success {
            use super::super::*;

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

    test!(part_1() == 4140);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 100);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
