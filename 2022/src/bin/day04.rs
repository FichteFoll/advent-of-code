#![feature(test)]

use std::ops::RangeInclusive;

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 4;
type Range = RangeInclusive<usize>;
type Parsed = Vec<(Range, Range)>;

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
            .lines()
            .map(|line| {
                let ranges: Vec<_> = line.split(',').map(|range_str| {
                    let rs = range_str.split_once('-').unwrap();
                    Range::new(rs.0.parse().unwrap(), rs.1.parse().unwrap())
                }).collect();
                let mut iter = ranges.into_iter();
                (iter.next().unwrap(), iter.next().unwrap())
            })
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .filter(either_contains)
        .count()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter()
        .filter(overlaps)
        .count()
}

fn either_contains<'a>((a, b): &'a &(Range, Range)) -> bool {
    return a.start() >= b.start() && a.end() <= b.end()
        || b.start() >= a.start() && b.end() <= a.end();
}

fn overlaps<'a>((a, b): &'a &(Range, Range)) -> bool {
    return a.contains(b.start()) || a.contains(b.end())
        || b.contains(a.start()) || b.contains(a.end());
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
