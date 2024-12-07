#![feature(test)]

use aoc2024::*;

const DAY: usize = 7;

type Parsed = Vec<Equation>;
type Equation = (u64, Vec<u64>);

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let (sum_s, nums_s) = line.split_once(": ").unwrap();
            let nums = nums_s.split(' ').map(|s| s.parse().unwrap()).collect();
            (sum_s.parse().unwrap(), nums)
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> u64 {
    solve(parsed, &[Op::Mul, Op::Add])
}

fn part_2(parsed: &Parsed) -> u64 {
    solve(parsed, &[Op::Mul, Op::Add, Op::Concat])
}

fn solve(parsed: &Parsed, ops: &[Op]) -> u64 {
    parsed
        .iter()
        .filter(|eq| can_be_valid(ops, eq.0, &eq.1))
        .map(|(r, _)| r)
        .sum()
}

enum Op {
    Mul,
    Add,
    Concat,
}

impl Op {
    fn eval(&self, n1: u64, n2: u64) -> u64 {
        match self {
            Op::Mul => n1 * n2,
            Op::Add => n1 + n2,
            Op::Concat => n1 * 10u64.pow(n2.ilog10() + 1) + n2,
        }
    }
}

fn can_be_valid(ops: &[Op], expected: u64, nums: &[u64]) -> bool {
    can_be_valid_rec(ops, expected, nums[0], &nums[1..])
}

fn can_be_valid_rec(ops: &[Op], expected: u64, n: u64, rest: &[u64]) -> bool {
    if rest.len() == 0 {
        return n == expected;
    }
    if n > expected {
        return false;
    }
    ops.iter()
        .any(|op| can_be_valid_rec(ops, expected, op.eval(n, rest[0]), &rest[1..]))
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        190: 10 19\n\
        3267: 81 40 27\n\
        83: 17 5\n\
        156: 15 6\n\
        7290: 6 8 6 15\n\
        161011: 16 10 13\n\
        192: 17 8 14\n\
        21037: 9 7 18 13\n\
        292: 11 6 16 20\n\
        ";

    test!(part_1() == 3749);
    test!(part_2() == 11387);
    bench_parse!(Vec::len, 850);
    bench!(part_1() == 975671981569);
    bench!(part_2() == 223472064194845);
}
