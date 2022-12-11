#![feature(test)]

use aoc2022::*;
use itertools::Itertools;

const DAY: usize = 11;

type Parsed = Vec<Monkey>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split("\n\n")
        .map(|block| {
            let mut lines = block.lines();
            // `Monkey 1:`
            lines.next();
            // `  Starting items: 1, 2, 3`
            let (_, items_str) = lines.next().unwrap().split_once(": ").unwrap();
            let items = items_str.split(", ").map(|s| s.parse().unwrap()).collect();
            // `  Operation: new = old * 13`
            let op_line = lines.next().unwrap();
            let num = op_line[25..].parse().ok();
            let operation = match op_line.bytes().nth(23).unwrap() {
                b'*' => Operation::Mul(num),
                b'+' => Operation::Add(num),
                _ => panic!("Unexpected operator in {op_line}"),
            };
            // `  Test: divisible by 11`
            let test_num = lines.next().unwrap()[21..].parse().unwrap();
            // `    If true: throw to monkey 3`
            let test_true = lines.next().unwrap()[29..].parse().unwrap();
            // `    If false: throw to monkey 2`
            let test_false = lines.next().unwrap()[30..].parse().unwrap();
            let test = (test_num, test_true, test_false);

            Monkey { items, operation, test, thrown: 0 }
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let mut monkeys = parsed.clone();
    (0..20).for_each(|_| play_round(&mut monkeys));
    monkeys.into_iter()
        .map(|m| m.thrown)
        .sorted_unstable()
        .rev()
        .take(2)
        .product()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn play_round(monkeys: &mut Parsed) {
    for i in 0..monkeys.len() {
        for (target, item) in monkeys[i].throw_all() {
            monkeys[target].items.push(item);
        }
    }
}

#[derive(Clone, Debug)]
struct Monkey {
    items: Vec<usize>,
    operation: Operation,
    test: (usize, usize, usize), // divisible by …; If true: …; If false: …
    thrown: usize,
}

impl Monkey {
    // throw items to monkeys; (monkey_n, item_value)
    fn throw_all(&mut self) -> Vec<(usize, usize)> {
        self.items.drain(..)
            .map(|old| {
                let new = self.operation.apply(old) / 3;
                self.thrown += 1;
                let target = if new % self.test.0 == 0 { self.test.1 } else { self.test.2 };
                (target, new)
            })
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
enum Operation {
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
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 8);
    bench!(part_1() == 121450);
    // bench!(part_2() == 0);

    #[test]
    fn test_play_round_1() {
        let mut monkeys = parse_input(TEST_INPUT);
        play_round(&mut monkeys);
        assert_eq!(monkeys[0].items, vec![20, 23, 27, 26]);
        assert_eq!(monkeys[1].items, vec![2080, 25, 167, 207, 401, 1046]);
        assert_eq!(monkeys[2].items, vec![]);
        assert_eq!(monkeys[3].items, vec![]);
    }

    #[test]
    fn test_play_round_2() {
        let mut monkeys = parse_input(TEST_INPUT);
        (0..20).for_each(|_| play_round(&mut monkeys));
        assert_eq!(monkeys[0].items, vec![10, 12, 14, 26, 34]);
        assert_eq!(monkeys[1].items, vec![245, 93, 53, 199, 115]);
        assert_eq!(monkeys[2].items, vec![]);
        assert_eq!(monkeys[3].items, vec![]);
    }
}
