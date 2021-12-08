#![feature(test)]
#![feature(int_abs_diff)]

use std::iter::Sum;
use std::ops::Div;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 7;

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
    // https://math.stackexchange.com/questions/113270
    let target = median(parsed);
    calc_fuel_linearly(parsed, target)
}

fn part_2(parsed: &Parsed) -> usize {
    let target = avg(parsed);
    // correct rounding error that I don't understand where it's coming from
    (0..=1).map(|i| calc_fuel_quadratically(parsed, target + i))
        .min().unwrap()
}

fn median<T: Ord + Clone>(slice: &[T]) -> T {
    let mut sorted = slice.to_vec();
    sorted.sort();
    sorted[sorted.len() / 2].clone()
}

fn avg<'a, T>(slice: &'a [T]) -> T
    where T: Sum<&'a T> + Div<Output=T> + From<usize> + 'a
{
    let len = slice.len().into();
    slice.iter().sum::<T>() / len
}

fn calc_fuel_linearly(parsed: &Parsed, target: usize) -> usize {
    parsed.iter()
        .map(|n| n.abs_diff(target))
        .sum()
}

fn calc_fuel_quadratically(parsed: &Parsed, target: usize) -> usize {
    parsed.iter()
        .map(|n| n.abs_diff(target))
        .map(|n| (1..=n).sum::<usize>()) // rust knows that this equals n(n+1)/2
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