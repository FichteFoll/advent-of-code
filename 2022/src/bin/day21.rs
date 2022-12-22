#![feature(test)]

use aoc2022::collections::*;
use aoc2022::*;
use parse::parse_input;

const DAY: usize = 21;

type Parsed = HashMap<String, Option<Monkey>>;

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
                    [key, _, num] => (key.to_string(), Some(Monkey::Num(num.parse().unwrap()))),
                    [key, _, operand1, op, operand2] => (
                        key.to_string(),
                        Some(Monkey::Term(
                            op.parse().unwrap(),
                            operand1.to_string(),
                            operand2.to_string(),
                        )),
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
    get_num(&mut parsed.clone(), "root").unwrap()
}

fn part_2(parsed: &Parsed) -> i64 {
    let mut map = parsed.clone();
    map.insert("humn".to_string(), None);
    let Some(Monkey::Term(_, key1, key2)) = map.get("root").cloned().unwrap() else {
        panic!("bad root");
    };
    let n1 = get_num(&mut map, &key1);
    let n2 = get_num(&mut map, &key2);
    match (n1, n2) {
        (Some(n), None) => find_solution(&mut map, &key2, n),
        (None, Some(n)) => find_solution(&mut map, &key1, n),
        _ => panic!("no humn found"),
    }
}

fn get_num(map: &mut Parsed, key: &str) -> Option<i64> {
    let mut queue: Vec<_> = [key.to_string()].into();
    while let Some(current) = queue.last() {
        let resolved = match map.get(current).unwrap() {
            Some(m) => m.resolve(&map),
            None => return None, // "humn" found (`key` was "humn")
        };
        match resolved {
            Ok(new) => {
                if queue.len() == 1 {
                    return new.num();
                }
                map.insert(current.clone(), Some(new));
                queue.pop();
            }
            Err(to_resolve) if to_resolve.is_empty() => return None, // "humn" found
            Err(to_resolve) => queue.extend(to_resolve),
        };
    }
    unreachable!()
}

fn find_solution(mut map: &mut Parsed, key: &str, target: i64) -> i64 {
    // Recursively resolve operations until n is satisifed
    // (using tail recursion).
    let Monkey::Term(op, key1, key2) = (match map.get(key).unwrap() {
            None => return target,
            Some(m) => m.clone(),
        }) else {
        unreachable!();
    };
    let n1 = get_num(&mut map, &key1);
    let n2 = get_num(&mut map, &key2);
    match (n1, n2) {
        (Some(operand), None) => {
            let next_n = op.resolve_right(operand, target);
            find_solution(&mut map, &key2, next_n)
        }
        (None, Some(operand)) => {
            let next_n = op.resolve_left(operand, target);
            find_solution(&mut map, &key1, next_n)
        }
        _ => panic!("no humn found"),
    }
}

impl Monkey {
    fn resolve(&self, map: &Parsed) -> Result<Monkey, Vec<String>> {
        match &self {
            &Monkey::Term(op, key1, key2) => {
                let mut err_vec = vec![];
                let operand1 = map.get(key1).unwrap().as_ref().ok_or_else(|| vec![])?.num();
                let operand2 = map.get(key2).unwrap().as_ref().ok_or_else(|| vec![])?.num();
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
    fn eval(&self, op1: i64, op2: i64) -> i64 {
        match self {
            Op::Add => op1 + op2,
            Op::Sub => op1 - op2,
            Op::Div => op1 / op2,
            Op::Mul => op1 * op2,
        }
    }

    // Resolve to the right/left-hand side of the operation using n for the other side
    fn resolve_right(&self, operand: i64, result: i64) -> i64 {
        match self {
            Op::Add => result - operand,
            Op::Sub => operand - result,
            Op::Mul => result / operand,
            Op::Div => operand / result,
        }
    }

    fn resolve_left(&self, operand: i64, result: i64) -> i64 {
        match self {
            Op::Add => result - operand,
            Op::Sub => result + operand,
            Op::Mul => result / operand,
            Op::Div => result * operand,
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
    test!(part_2() == 301);
    // bench_parse!(Vec::len, 0);
    bench!(part_1() == 62386792426088);
    bench!(part_2() == 3876027196185);
}
