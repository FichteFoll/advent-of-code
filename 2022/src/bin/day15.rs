#![feature(test)]

use aoc2022::*;
use aoc2022::coord::Point;
use itertools::{Itertools, MinMaxResult};
use parse::parse_input;

const DAY: usize = 15;

type Parsed = Vec<(Point<2>, Point<2>)>;

fn main() {
    let input = read_file(DAY);
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed, 2000000));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .lines()
            .map(|line| {
                let mut tokens = line.split(&[' ', ',', '=', ':']);
                let s_x = tokens.nth(3).unwrap().parse().unwrap();
                let s_y = tokens.nth(2).unwrap().parse().unwrap();
                let b_x = tokens.nth(6).unwrap().parse().unwrap();
                let b_y = tokens.nth(2).unwrap().parse().unwrap();
                (Point([s_x, s_y]), Point([b_x, b_y]))
            })
            .collect()
    }
}

fn part_1(parsed: &Parsed, y: i32) -> usize {
    let MinMaxResult::MinMax(min_x, max_x) = parsed.iter()
        .flat_map(|(s, b)| {
            let radius = (s - b).manhattan();
            [s.x() - radius, s.x() + radius].into_iter()
        })
        .minmax()
    else {
        panic!("expected two results");
    };

    (min_x..=max_x)
        .filter(|&x| {
            parsed.iter()
                .any(|(s, b)| {
                    let pt = &Point([x, y]);
                    pt != b && (s - pt).manhattan() <= (s - b).manhattan()
                })
        })
        .count()
}

fn part_2(_parsed: &Parsed) -> usize {
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
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
