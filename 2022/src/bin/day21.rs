#![feature(test)]

use std::ops::{Add, Div, Mul, Sub};

use aoc2022::collections::*;
use aoc2022::*;
use parse::parse_input;

const DAY: usize = 21;

type Parsed = HashMap<String, Monkey>;

main!();

#[derive(Clone, Copy, Debug)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(Clone, Debug)]
pub enum Monkey {
    Term(Op, String, String),
    Num(i64),
}

mod parse {
    use super::*;
    use std::str::FromStr;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .lines()
            .map(|line| {
                let words: Vec<_> = line.split([':', ' ']).collect();
                match words[..] {
                    [key, _, num] => (key.to_string(), Monkey::Num(num.parse().unwrap())),
                    [key, _, operand1, op, operand2] => (
                        key.to_string(),
                        Monkey::Term(
                            op.parse().unwrap(),
                            operand1.to_string(),
                            operand2.to_string(),
                        ),
                    ),
                    _ => panic!("unrecognized format: {line}"),
                }
            })
            .collect()
    }

    impl FromStr for Op {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            use Op::*;
            Ok(match s {
                "*" => Mul,
                "/" => Div,
                "+" => Add,
                "-" => Sub,
                _ => Err(())?,
            })
        }
    }
}

fn part_1(parsed: &Parsed) -> i64 {
    get_num(parsed, "root")
}

fn part_2(_parsed: &Parsed) -> i64 {
    todo!()
}

fn get_num(parsed: &Parsed, key: &str) -> i64 {
    let mut map = parsed.clone();

    let mut queue: Vec<_> = [key.to_string()].into();
    while let Some(current) = queue.last() {
        let resolved = { map.get(current).unwrap().resolve(&map) };
        match resolved {
            Ok(new) => {
                if queue.len() == 1 {
                    return new.num().unwrap();
                }
                map.insert(current.clone(), new);
                queue.pop();
            }
            Err(to_resolve) => queue.extend(to_resolve),
        };
    }
    unreachable!()
}

impl Monkey {
    fn resolve(&self, map: &Parsed) -> Result<Monkey, Vec<String>> {
        match &self {
            &Monkey::Term(op, key1, key2) => {
                let mut err_vec = vec![];
                let operand1 = map.get(key1).unwrap().num();
                let operand2 = map.get(key2).unwrap().num();
                if let Some((n1, n2)) = operand1.zip(operand2) {
                    return Ok(Monkey::Num(op.eval(n1, n2)));
                }
                if operand1.is_none() {
                    err_vec.push(key1.clone());
                }
                if operand2.is_none() {
                    err_vec.push(key2.clone());
                }
                Err(err_vec)
            }
            _ => Ok(self.clone()),
        }
    }

    fn num(&self) -> Option<i64> {
        match self {
            &Monkey::Num(n) => Some(n),
            _ => None,
        }
    }
}

impl Op {
    fn eval<T>(&self, op1: T, op2: T) -> T
    where
        T: Add<Output = T> + Sub<Output = T> + Div<Output = T> + Mul<Output = T>,
    {
        match self {
            Op::Add => op1 + op2,
            Op::Sub => op1 - op2,
            Op::Div => op1 / op2,
            Op::Mul => op1 * op2,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        root: pppw + sjmn\n\
        dbpl: 5\n\
        cczh: sllz + lgvd\n\
        zczc: 2\n\
        ptdq: humn - dvpt\n\
        dvpt: 3\n\
        lfqf: 4\n\
        humn: 5\n\
        ljgn: 2\n\
        sjmn: drzm * dbpl\n\
        sllz: 4\n\
        pppw: cczh / lfqf\n\
        lgvd: ljgn * ptdq\n\
        drzm: hmdt - zczc\n\
        hmdt: 32\n\
        ";

    test!(part_1() == 152);
    // test!(part_2() == 0);
    // bench_parse!(Vec::len, 0);
    bench!(part_1() == 62386792426088);
    // bench!(part_2() == 0);
}
