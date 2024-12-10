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
    parsed.iter().filter(|line| is_safe(line, &(1..=3))).count()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .filter(|line| is_safe_p2(line, &(1..=3)))
        .count()
}

fn is_safe(line: &[i32], safe_range: &RangeInclusive<i32>) -> bool {
    line.iter()
        .tuple_windows()
        .map(|(a, b)| {
            Some(b - a)
                .filter(|d| safe_range.contains(&d.abs()))
                .map(i32::signum)
        })
        .all_equal_value()
        .unwrap_or(None)
        .is_some()
}

fn is_safe_p2(line: &[i32], safe_range: &RangeInclusive<i32>) -> bool {
    (0..=line.len()).any(|i| {
        let mut sub_line = line.iter().cloned().collect_vec();
        if i < line.len() {
            sub_line.remove(i);
        }
        is_safe(&sub_line, safe_range)
    })
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
    test!(part_2() == 4);
    bench_parse!(Vec::len, 1000);
    bench!(part_1() == 299);
    bench!(part_2() == 364);

    #[test]
    pub fn test_p2_removes_correct_number() {
        assert!(is_safe_p2(&[1, 3, 2, 3, 4], &(1..=3)));
    }
}
