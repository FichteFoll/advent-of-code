#![feature(box_patterns)]
#![feature(test)]

use std::ops::Add;
use std::iter::Sum;

use itertools::Itertools;

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

#[derive(Clone, Debug, PartialEq, Eq)]
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
        #[error("Expected {0:?} found {0:?}")]
        Expected(char, char),
        #[error("Unexpected '['")]
        UnexpectedOpen,
        #[error("Unexpected ']'")]
        UnexpectedClose,
    }

    impl FromStr for SnailNum {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut stack: Vec<Vec<SnailNum>> = vec![];
            let mut after_token = false;
            let mut chars = s.chars();
            while let Some(c) = chars.next() {
                after_token = match (c, stack.last_mut(), after_token) {
                    ('[', _, true) =>
                        return Err(ParseError::UnexpectedOpen),
                    ('[', _, false) => {
                        stack.push(vec![]);
                        false
                    }
                    (']', Some(curr), true) => {
                        if curr.len() != 2 {
                            return Err(ParseError::BadCount(curr.len()));
                        }
                        let mut iter = stack.pop().unwrap().into_iter();
                        let new = Pair(iter.next().unwrap().into(), iter.next().unwrap().into());
                        if let Some(prev) = stack.last_mut() {
                            prev.push(new);
                            true
                        } else if chars.next().is_some() {
                            return Err(ParseError::ExpectedEol);
                        } else {
                            return Ok(new);
                        }
                    }
                    (']', _, false) =>
                        return Err(ParseError::UnexpectedClose),
                    (_, None, _) =>
                        return Err(ParseError::Expected('[', c)),
                    (',', _, true) =>
                        false,
                    (_, Some(curr), false) => {
                        curr.push(Terminal(c.to_digit(10).ok_or(ParseError::BadChar(c))? as _));
                        true
                    }
                    (_, Some(_), true) =>
                        return Err(ParseError::Expected(',', c)),
                };
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
            test_err!(expected_open_1, "]", UnexpectedClose);
            test_err!(expected_open_2, "12", Expected('[', '1'));
            test_err!(expected_eol, "[1,2]]", ExpectedEol);
            test_err!(too_few, "[1]", BadCount(1));
            test_err!(too_many, "[1,2,3]", BadCount(3));
            test_err!(empty, "", Empty);
            test_err!(bad_char, "[a,2]", BadChar('a'));
            test_err!(unexpected_open, "[1[", UnexpectedOpen);
            test_err!(two_commas, "[1,,2]", BadChar(','));
            test_err!(leading_comma, "[,1,2]", BadChar(','));
            test_err!(missing_comma, "[,1,2]", BadChar(','));
            test_err!(expected_comma, "[12]", Expected(',', '2'));
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
    let sum: SnailNum = parsed.iter().cloned().sum();
    sum.magnitude()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter()
        .permutations(2)
        .map(|vec| (vec[0].clone() + vec[1].clone()).magnitude())
        .max()
        .unwrap()
}

#[derive(Debug, PartialEq, Eq)]
struct Explosion(Option<usize>, Option<usize>);

impl SnailNum {
    const MAX_DEPTH: usize = 4;
    const MAX_NUM: usize = 10;

    fn reduce(self) -> Self {
        let mut next_self = self;
        loop {
            let exploded;
            (next_self, exploded) = next_self.explode(0);
            if exploded.is_some() {
                continue;
            }
            next_self = match next_self.split() {
                (split_self, true) => split_self,
                (new_self, false) => break new_self,
            };
        }
    }

    fn explode(self, level: usize) -> (Self, Option<Explosion>) {
        match self {
            Terminal(_) => (self, None),
            Pair(box Terminal(a), box Terminal(b)) if level >= SnailNum::MAX_DEPTH =>
                (Terminal(0), Some(Explosion(Some(a), Some(b)))),
            Pair(box left, box right) => {
                match left.explode(level + 1) {
                    (new_left, Some(Explosion(left_exp, Some(carry)))) => {
                        let new_right = match right {
                            Terminal(x) => Terminal(x + carry),
                            _ => right.add_left(carry),
                        };
                        (Pair(new_left.into(), new_right.into()), Some(Explosion(left_exp, None)))
                    },
                    (new_left, None) => match right.explode(level + 1) {
                        (new_right, Some(Explosion(Some(carry), right_exp))) => {
                            let new_new_left = match new_left {
                                Terminal(x) => Terminal(x + carry),
                                _ => new_left.add_right(carry),
                            };
                            (Pair(new_new_left.into(), new_right.into()), Some(Explosion(None, right_exp)))
                        },
                        (new_right, exp) =>
                            (Pair(new_left.into(), new_right.into()), exp),
                    },
                    (new_left, exp) =>
                        (Pair(new_left.into(), right.into()), exp),
                }
            }
        }
    }

    fn add_left(self, carry: usize) -> Self {
        match self {
            Terminal(x) => Terminal(x + carry),
            Pair(box left, right) => Pair(left.add_left(carry).into(), right),
        }
    }

    fn add_right(self, carry: usize) -> Self {
        match self {
            Terminal(x) => Terminal(x + carry),
            Pair(left, box right) => Pair(left, right.add_right(carry).into()),
        }
    }

    fn split(self) -> (Self, bool) {
        match self {
            Terminal(n) if n >= SnailNum::MAX_NUM =>
                (Pair(Terminal(n / 2).into(), Terminal((n + 1) / 2).into()), true),
            Terminal(_) =>
                (self, false),
            Pair(box left, box right) => {
                let (new_left, split_left) = left.split();
                let (new_right, split_right) = match split_left {
                    true => (right, true),
                    false => right.split(),
                };
                (Pair(new_left.into(), new_right.into()), split_right)
            },
        }
    }

    fn magnitude(&self) -> usize {
        match self {
            Terminal(x) => *x,
            Pair(left, right) => left.magnitude() * 3 + right.magnitude() * 2,
        }
    }
}

impl Add for SnailNum {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Pair(self.into(), rhs.into()).reduce()
    }
}

impl Sum for SnailNum {
    fn sum<I: Iterator<Item = Self>>(mut iter: I) -> Self {
        let first = iter.next().expect("must have at least one element");
        iter.fold(first, |acc, n| acc + n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
        [[[5,[2,8]],4],[5,[[9,9],0]]]\n\
        [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
        [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
        [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
        [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
        [[[[5,4],[7,7]],8],[[8,3],8]]\n\
        [[9,3],[[9,9],[6,[4,9]]]]\n\
        [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
        [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]\n\
        ";

    test!(part_1() == 4140);
    test!(part_2() == 3993);
    bench_parse!(Vec::len, 100);
    bench!(part_1() == 4207);
    bench!(part_2() == 4635);


    #[test]
    fn explode_no_explosion() {
        let num: SnailNum = "[[[[0,1],2],3],4]".parse().unwrap();
        let (exploded, explosion) = num.clone().explode(0);
        assert_eq!(exploded, num);
        assert_eq!(explosion, None);
    }

    #[test]
    fn explode_left_carry() {
        let num: SnailNum = "[[[[[9,8],1],2],3],4]".parse().unwrap();
        let expected: SnailNum = "[[[[0,9],2],3],4]".parse().unwrap();
        let (exploded, explosion) = num.explode(0);
        assert_eq!(exploded, expected);
        assert_eq!(explosion, Some(Explosion(Some(9), None)));
    }

    #[test]
    fn explode_right_carry() {
        let num: SnailNum = "[7,[6,[5,[4,[3,2]]]]]".parse().unwrap();
        let expected: SnailNum = "[7,[6,[5,[7,0]]]]".parse().unwrap();
        let (exploded, explosion) = num.explode(0);
        assert_eq!(exploded, expected);
        assert_eq!(explosion, Some(Explosion(None, Some(2))));
    }

    #[test]
    fn explode_right_no_carry() {
        let num: SnailNum = "[[6,[5,[4,[3,2]]]],1]".parse().unwrap();
        let expected: SnailNum = "[[6,[5,[7,0]]],3]".parse().unwrap();
        let (exploded, explosion) = num.explode(0);
        assert_eq!(exploded, expected);
        assert_eq!(explosion, Some(Explosion(None, None)));
    }

    #[test]
    fn split() {
        assert_eq!(Terminal(9).split(), (Terminal(9), false));
        assert_eq!(Terminal(10).split(), (Pair(Terminal(5).into(), Terminal(5).into()), true));
        assert_eq!(Terminal(11).split(), (Pair(Terminal(5).into(), Terminal(6).into()), true));
        assert_eq!(Terminal(12).split(), (Pair(Terminal(6).into(), Terminal(6).into()), true));
    }

    #[test]
    fn sum_no_reduce() {
        let input = "[1,1]\n[2,2]\n[3,3]\n[4,4]";
        let expected: SnailNum = "[[[[1,1],[2,2]],[3,3]],[4,4]]".parse().unwrap();
        let parsed = parse_input(input);
        let sum: SnailNum = parsed.into_iter().sum();
        assert_eq!(sum, expected);
    }

    #[test]
    fn sum_reduce() {
        let input = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]";
        let expected: SnailNum = "[[[[5,0],[7,4]],[5,5]],[6,6]]".parse().unwrap();
        let parsed = parse_input(input);
        let sum: SnailNum = parsed.into_iter().sum();
        assert_eq!(sum, expected);
    }

    #[test]
    fn magnitude() {
        let mag = |s: &str| s.parse::<SnailNum>().unwrap().magnitude();
        assert_eq!(mag("[[1,2],[[3,4],5]]"), 143);
        assert_eq!(mag("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"), 1384);
        assert_eq!(mag("[[[[1,1],[2,2]],[3,3]],[4,4]]"), 445);
        assert_eq!(mag("[[[[3,0],[5,3]],[4,4]],[5,5]]"), 791);
        assert_eq!(mag("[[[[5,0],[7,4]],[5,5]],[6,6]]"), 1137);
        assert_eq!(mag("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"), 3488);
        assert_eq!(mag("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"), 4140);
    }
}
