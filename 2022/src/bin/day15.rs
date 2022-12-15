#![feature(test)]

use std::ops::RangeInclusive;

use itertools::{Itertools, MinMaxResult};

use aoc2022::*;
use aoc2022::coord::Point;

const DAY: usize = 15;

type Parsed = Vec<(Point<2>, Point<2>, i32)>;

const PART_1_Y: i32 = 2000000;

fn main() {
    let input = read_file(DAY);
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed, PART_1_Y));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let mut tokens = line.split(&[' ', ',', '=', ':']);
            let s_x = tokens.nth(3).unwrap().parse().unwrap();
            let s_y = tokens.nth(2).unwrap().parse().unwrap();
            let b_x = tokens.nth(6).unwrap().parse().unwrap();
            let b_y = tokens.nth(2).unwrap().parse().unwrap();
            let (s, b) = (Point([s_x, s_y]), Point([b_x, b_y]));
            (s, b, (s - b).manhattan())
        })
        .collect()
}

fn part_1(parsed: &Parsed, y: i32) -> usize {
    let MinMaxResult::MinMax(min_x, max_x) = parsed.iter()
        .flat_map(|(s, _, r)| {
            [s.x() - r, s.x() + r].into_iter()
        })
        .minmax()
    else {
        panic!("expected two results");
    };

    let mut candidates: Vec<_> = vec![false; (max_x - min_x + 1) as usize];
    // collect intersection for each sensor-beacon area
    for (s, _, r) in parsed.iter() {
        let x_range = *r - (s.y() - y).abs();
        if x_range < 0 {
            continue;
        }
        for x_offset in -x_range..=x_range {
            candidates[(s.x() + x_offset - min_x) as usize] = true;
        }
    }
    // unset known beacons
    for (_, b, _) in parsed.iter() {
        if b.y() == y {
            candidates[(b.x() - min_x) as usize] = false;
        }
    }
    candidates.into_iter().filter(|x| *x).count()
}

fn part_2(_parsed: &Parsed, range: RangeInclusive<i32>) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
        Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
        Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
        Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
        Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
        Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
        Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
        Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
        Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
        Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
        Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
        Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
        Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
        Sensor at x=20, y=1: closest beacon is at x=15, y=3\n\
        ";

    test!(part_1(10) == 26);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 31);
    bench!(part_1(PART_1_Y) == 4876693);
    // bench!(part_2() == 0);
}
