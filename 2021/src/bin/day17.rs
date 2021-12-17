#![feature(test)]

use std::ops::RangeInclusive;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 17;

type Parsed = (RangeInclusive<i32>, RangeInclusive<i32>);

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use std::ops::RangeInclusive;
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let words: Vec<_> = input.trim().split_whitespace().collect();
        (parse_range(words[2]), parse_range(words[3]))
    }

    fn parse_range(s: &str) -> RangeInclusive<i32> {
        let (start, stop) = s[2..].trim_end_matches(',').split_once("..").unwrap();
        RangeInclusive::new(start.parse().unwrap(), stop.parse().unwrap())
    }
}

fn part_1((_, y_range): &Parsed) -> i32 {
    // We can do any amount of iterations
    // because we will always be able to find an appropriate x,
    // so we only look at y.
    // The first half is a parable,
    // meaning we will always land at 0 after we left the ground,
    // assuming y < 0.
    // Thus, the highest vy_0 we can choose is the one
    // that makes us land just at the lower bound.
    assert!(*y_range.start() < 0, "This algorithm doesn't work for y >= 0");
    let v = (-y_range.start()) - 1;
    (1..=v).sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "target area: x=20..30, y=-10..-5";

    test!(part_1() == 45);
    // test!(part_2() == 0);
    bench_parse!(|x| x, &(14i32..=50, -267i32..=-225));
    bench!(part_1() == 35511);
    // bench!(part_2() == 0);
}
