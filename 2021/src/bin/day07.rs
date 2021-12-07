#![feature(test)]
#![feature(int_abs_diff)]

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 07;

type Parsed = Vec<usize>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split(',')
            .map(|line| line.parse().unwrap())
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let min = *parsed.iter().min().unwrap();
    let max = *parsed.iter().max().unwrap();
    (min..max)
        .map(|target| calc_fuel_linearly(parsed, target))
        .min()
        .unwrap()
}

fn calc_fuel_linearly(parsed: &Parsed, target: usize) -> usize {
    parsed.iter()
        .map(|n| n.abs_diff(target))
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let min = *parsed.iter().min().unwrap();
    let max = *parsed.iter().max().unwrap();
    (min..max)
        .map(|target| calc_fuel_quadratically(parsed, target))
        .min()
        .unwrap()
}

fn calc_fuel_quadratically(parsed: &Parsed, target: usize) -> usize {
    parsed.iter()
        .map(|n| n.abs_diff(target))
        .map(|n| n * (n + 1) / 2)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "16,1,2,0,4,2,7,1,2,14";

    #[test]
    fn test_calc_fuel_linearly() {
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(calc_fuel_linearly(&parsed, 1), 41);
        assert_eq!(calc_fuel_linearly(&parsed, 3), 39);
        assert_eq!(calc_fuel_linearly(&parsed, 10), 71);
    }

    #[test]
    fn test_calc_fuel_quadratically() {
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(calc_fuel_quadratically(&parsed, 2), 206);
    }

    test!(part_1() == 37);
    test!(part_2() == 168);
    bench_parse!(Vec::len, 1000);
    bench!(part_1() == 336040);
    bench!(part_2() == 94813675);
}
