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
    parsed
        .iter()
        .filter(|eq| can_be_valid(eq))
        .map(|(r, _)| r)
        .sum()
}

fn part_2(_parsed: &Parsed) -> u64 {
    todo!()
}

enum Op {
    Mul,
    Add,
}

impl Op {
    const ALL: [Op; 2] = [Op::Mul, Op::Add];

    fn eval(&self, n1: u64, n2: u64) -> u64 {
        match self {
            Op::Mul => n1 * n2,
            Op::Add => n1 + n2,
        }
    }
}

fn can_be_valid((expected, nums): &Equation) -> bool {
    can_be_valid_rec(*expected, nums[0], &nums[1..])
}

fn can_be_valid_rec(expected: u64, n: u64, rest: &[u64]) -> bool {
    if rest.len() == 0 {
        return n == expected;
    }
    if n > expected {
        return false;
    }
    Op::ALL.iter().any(|op| {
        let n2 = op.eval(n, rest[0]);
        can_be_valid_rec(expected, n2, &rest[1..])
    })
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
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 850);
    bench!(part_1() == 975671981569);
    // bench!(part_2() == 0);
}
