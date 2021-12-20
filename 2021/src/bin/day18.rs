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
                    (']', Some(_), true) => {
                        let [a, b]: [_; 2] = stack.pop().unwrap().try_into()
                            .map_err(|v: Vec<SnailNum>| ParseError::BadCount(v.len()))?;
                        let new = Pair(a.into(), b.into());
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
        use test_case::test_case;
        use super::{*, ParseError::*};

        #[test_case("[" => Unclosed; "unclosed")]
        #[test_case("]" => UnexpectedClose; "expected open 1")]
        #[test_case("12" => Expected('[', '1'); "expected open 2")]
        #[test_case("[1,2]]" => ExpectedEol; "expected eol")]
        #[test_case("[1]" => BadCount(1); "too few")]
        #[test_case("[1,2,3]" => BadCount(3); "too many")]
        #[test_case("" => Empty; "empty")]
        #[test_case("[a,2]" => BadChar('a'); "bad char")]
        #[test_case("[1[" => UnexpectedOpen; "unexpected open")]
        #[test_case("[1,,2]" => BadChar(','); "two commas")]
        #[test_case("[,1,2]" => BadChar(','); "leading comma")]
        #[test_case("[,1,2]" => BadChar(','); "missing comma")]
        #[test_case("[12]" => Expected(',', '2'); "expected comma")]
        fn error(input: &str) -> ParseError {
            input.parse::<SnailNum>().err().unwrap()
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

    fn reduce(&mut self) {
        while self.explode(0).is_some() || self.split() {}
    }

    fn explode(&mut self, level: usize) -> Option<Explosion> {
        match self {
            Terminal(_) => None,
            &mut Pair(box Terminal(a), box Terminal(b)) if level >= SnailNum::MAX_DEPTH => {
                *self = Terminal(0);
                Some(Explosion(Some(a), Some(b)))
            }
            Pair(left, right) => {
                match left.explode(level + 1) {
                    Some(Explosion(left_exp, Some(carry))) => {
                        right.add_left(carry);
                        Some(Explosion(left_exp, None))
                    }
                    None => match right.explode(level + 1) {
                        Some(Explosion(Some(carry), right_exp)) => {
                            left.add_right(carry);
                            Some(Explosion(None, right_exp))
                        }
                        exp => exp,
                    }
                    exp => exp,
                }
            }
        }
    }

    fn add_left(&mut self, carry: usize) {
        match self {
            Terminal(x) => *x += carry,
            Pair(left, _) => left.add_left(carry),
        }
    }

    fn add_right(&mut self, carry: usize) {
        match self {
            Terminal(x) => *x += carry,
            Pair(_, right) => right.add_right(carry),
        }
    }

    fn split(&mut self) -> bool {
        match self {
            &mut Terminal(n) if n >= SnailNum::MAX_NUM => {
                *self = Pair(Terminal(n / 2).into(), Terminal((n + 1) / 2).into());
                true
            }
            Terminal(_) => false,
            Pair(left, right) => left.split() || right.split(),
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
        let mut pair = Pair(self.into(), rhs.into());
        pair.reduce();
        pair
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
    use test_case::test_case;

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

    #[test_case("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]", Some(Explosion(Some(9), None)); "left carry")]
    #[test_case("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]", Some(Explosion(None, Some(2))); "right carry")]
    #[test_case("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]", Some(Explosion(None, None)); "no carry")]
    #[test_case("[[[[0,1],2],3],4]", "[[[[0,1],2],3],4]", None; "none")]
    fn explode(input: &str, expected_str: &str, result: Option<Explosion>) {
        let mut num: SnailNum = input.parse().unwrap();
        let expected: SnailNum = expected_str.parse().unwrap();
        assert_eq!(num.explode(0), result);
        assert_eq!(num, expected);
    }

    #[test_case(Terminal(9) => Terminal(9); "does not split 9")]
    #[test_case(Terminal(10) => Pair(Terminal(5).into(), Terminal(5).into()); "splits 10")]
    #[test_case(Terminal(11) => Pair(Terminal(5).into(), Terminal(6).into()); "splits 11")]
    #[test_case(Terminal(12) => Pair(Terminal(6).into(), Terminal(6).into()); "splits 12")]
    fn split(mut n: SnailNum) -> SnailNum {
        n.split();
        n
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

    #[test_case("[[1,2],[[3,4],5]]" => 143)]
    #[test_case("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" => 1384)]
    #[test_case("[[[[1,1],[2,2]],[3,3]],[4,4]]" => 445)]
    #[test_case("[[[[3,0],[5,3]],[4,4]],[5,5]]" => 791)]
    #[test_case("[[[[5,0],[7,4]],[5,5]],[6,6]]" => 1137)]
    #[test_case("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" => 3488)]
    #[test_case("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]" => 4140)]
    fn magnitude(input: &str) -> usize {
        let num: SnailNum = input.parse::<SnailNum>().unwrap();
        num.magnitude()
    }
}
