#![feature(test)]

// use core::range::RangeInclusive;

use std::ops::RangeInclusive;

use aoc2024::*;
use itertools::Itertools;

const DAY: usize = 2;

type Parsed = Vec<Vec<i32>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|ns| ns.parse().unwrap())
                .collect()
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .filter(|line| line_is_safe(line, 1..=3))
        .count()
        + parsed
            .iter()
            .filter(|line| line_is_safe(line, -3..=-1))
            .count()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn line_is_safe(line: &[i32], safe_range: RangeInclusive<i32>) -> bool {
    line.iter()
        .tuple_windows()
        .map(|(a, b)| a - b)
        .all(|diff| safe_range.contains(&diff))
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        7 6 4 2 1\n\
        1 2 7 8 9\n\
        9 7 6 2 1\n\
        1 3 2 4 5\n\
        8 6 4 4 1\n\
        1 3 6 7 9\n\
        ";

    test!(part_1() == 2);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 1000);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
