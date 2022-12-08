#![feature(test)]

use std::ops::RangeInclusive;

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 4;

type Range = RangeInclusive<usize>;
type Parsed = Vec<(Range, Range)>;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.trim().lines()
            .map(|line| {
                let parts = line.split_once(',').unwrap();
                (parse_range(parts.0), parse_range(parts.1))
            })
            .collect()
    }

    fn parse_range(range_str: &str) -> Range {
        let rs = range_str.split_once('-').unwrap();
        Range::new(rs.0.parse().unwrap(), rs.1.parse().unwrap())
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .filter(|(a, b)| {
            a.start() >= b.start() && a.end() <= b.end()
                || b.start() >= a.start() && b.end() <= a.end()
        })
        .count()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter()
        .filter(|(a, b)| {
            a.contains(b.start())
                || a.contains(b.end())
                || b.contains(a.start())
                || b.contains(a.end())
        })
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        2-4,6-8\n\
        2-3,4-5\n\
        5-7,7-9\n\
        2-8,3-7\n\
        6-6,4-6\n\
        2-6,4-8\n\
        ";

    test!(part_1() == 2);
    test!(part_2() == 4);
    bench_parse!(Vec::len, 1000);
    bench!(part_1() == 431);
    bench!(part_2() == 823);
}
