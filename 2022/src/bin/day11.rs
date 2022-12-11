#![feature(test)]

use itertools::Itertools;

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 11;

type Parsed = Vec<Monkey>;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split("\n\n")
            .map(|block| parse_block(block).unwrap())
            .collect()
    }

    fn parse_block(block: &str) -> Option<Monkey> {
        let mut lines = block.lines();
        // `Monkey 1:`
        lines.next();
        // `  Starting items: 1, 2, 3`
        let (_, items_str) = lines.next()?.split_once(": ")?;
        let items = items_str.split(", ").map(|s| s.parse().unwrap()).collect();
        // `  Operation: new = old * 13`
        let op_line = lines.next()?;
        let num = op_line[25..].parse().ok();
        let operation = match op_line.bytes().nth(23)? {
            b'*' => Operation::Mul(num),
            b'+' => Operation::Add(num),
            _ => panic!("Unexpected operator in {op_line}"),
        };
        // `  Test: divisible by 11`
        let test_num = lines.next()?[21..].parse().ok()?;
        // `    If true: throw to monkey 3`
        let test_true = lines.next()?[29..].parse().ok()?;
        // `    If false: throw to monkey 2`
        let test_false = lines.next()?[30..].parse().ok()?;
        let test = (test_num, test_true, test_false);

        Some(Monkey { items, operation, test, throw_count: 0 })
    }
}

const PART_1_RELIEF: fn(usize) -> usize = |n| n / 3;

fn part_1(parsed: &Parsed) -> usize {
    let mut monkeys = parsed.clone();
    (0..20).for_each(|_| play_round(&mut monkeys, PART_1_RELIEF));
    score(monkeys)
}

fn part_2(parsed: &Parsed) -> usize {
    let mut monkeys = parsed.clone();
    // Multiplication and addition does not affect
    // the modulo rest value when wrapping around.
    // We compute a multiple of all our modulo operands
    // to ensure that the results don't change for any monkey's operand
    // and apply it after each operation.
    // (All numbers in my input and the test are prime numbers,
    // so we don't lose anything by simply using the product.)
    let modulo: usize = monkeys.iter().map(|m| m.test.0).product();
    (0..10000).for_each(|_| play_round(&mut monkeys, |n| n % modulo));
    score(monkeys)
}

fn play_round(monkeys: &mut Parsed, relief: impl Fn(usize) -> usize) {
    for i in 0..monkeys.len() {
        for (target, item) in monkeys[i].throw_all(&relief) {
            monkeys[target].items.push(item);
        }
    }
}

fn score(monkeys: Vec<Monkey>) -> usize {
    monkeys.into_iter()
        .map(|m| m.throw_count)
        .sorted_unstable()
        .rev()
        .take(2)
        .product()
}

#[derive(Clone, Debug)]
pub struct Monkey {
    items: Vec<usize>,
    operation: Operation,
    test: (usize, usize, usize), // divisible by …; If true: …; If false: …
    throw_count: usize,
}

impl Monkey {
    // throw items to monkeys; (monkey_n, item_value)
    fn throw_all(&mut self, relief: impl Fn(usize) -> usize) -> Vec<(usize, usize)> {
        self.items.drain(..)
            .map(|old| {
                let new = relief(self.operation.apply(old));
                self.throw_count += 1;
                let target = if new % self.test.0 == 0 { self.test.1 } else { self.test.2 };
                (target, new)
            })
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Operation {
    // None => multiply with self
    Mul(Option<usize>),
    Add(Option<usize>),
}

impl Operation {
    fn apply(&self, old: usize) -> usize {
        match self {
            Self::Mul(op) => old * op.unwrap_or(old),
            Self::Add(op) => old + op.unwrap_or(old),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    // Cannot inline test input nicely because it has leading whitespace
    const TEST_INPUT: &str = include_str!("../../input/day11_test.txt");

    test!(part_1() == 10605);
    test!(part_2() == 2713310158);
    bench_parse!(Vec::len, 8);
    bench!(part_1() == 121450);
    bench!(part_2() == 28244037010);

    #[test]
    fn test_play_round_part1_1() {
        let mut monkeys = parse_input(TEST_INPUT);
        play_round(&mut monkeys, PART_1_RELIEF);
        assert_eq!(monkeys[0].items, vec![20, 23, 27, 26]);
        assert_eq!(monkeys[1].items, vec![2080, 25, 167, 207, 401, 1046]);
        assert_eq!(monkeys[2].items, vec![]);
        assert_eq!(monkeys[3].items, vec![]);
    }

    #[test]
    fn test_play_round_part1_20() {
        let mut monkeys = parse_input(TEST_INPUT);
        (0..20).for_each(|_| play_round(&mut monkeys, PART_1_RELIEF));
        assert_eq!(monkeys[0].items, vec![10, 12, 14, 26, 34]);
        assert_eq!(monkeys[1].items, vec![245, 93, 53, 199, 115]);
        assert_eq!(monkeys[2].items, vec![]);
        assert_eq!(monkeys[3].items, vec![]);
    }

    #[test]
    fn test_play_round_part2_20() {
        let mut monkeys = parse_input(TEST_INPUT);
        let modulo: usize = monkeys.iter().map(|m| m.test.0).product();
        (0..20).for_each(|_| play_round(&mut monkeys, |n| n % modulo));
        let throws: Vec<_> = monkeys.into_iter().map(|m| m.throw_count).collect();
        assert_eq!(throws, vec![99, 97, 8, 103]);
    }
}
