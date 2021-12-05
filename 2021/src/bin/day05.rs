#![feature(test)]

use std::collections::HashMap;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 5;

type Point = (usize, usize);
type Parsed = Vec<(Point, Point)>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn part_1(parsed: &Parsed) -> usize {
    let mut grid = HashMap::new();
    let filtered = parsed.iter()
        .filter(|(from, to)| from.0 == to.0 || from.1 == to.1);
    for (from, to) in filtered {
        for i in from.0.min(to.0)..=from.0.max(to.0) {
            for j in from.1.min(to.1)..=from.1.max(to.1) {
                *grid.entry((i, j)).or_insert(0) += 1;
            }
        }
    }
    grid.values().filter(|&&n| n >= 2).count()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split('\n')
            .map(|line| {
                let (left, right) = line.split_once(" -> ").expect("no separator");
                (parse_pt(left), parse_pt(right))
            })
            .collect()
    }

    fn parse_pt(s: &str) -> Point {
        let (x, y) = s.split_once(',').expect("no comma");
        (x.parse().unwrap(), y.parse().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        0,9 -> 5,9\n\
        8,0 -> 0,8\n\
        9,4 -> 3,4\n\
        2,2 -> 2,1\n\
        7,0 -> 7,4\n\
        6,4 -> 2,0\n\
        0,9 -> 2,9\n\
        3,4 -> 1,4\n\
        0,0 -> 8,8\n\
        5,5 -> 8,2\n\
        ";

    test!(part_1() == 5);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 500);
    bench!(part_1() == 6311);
    // bench!(part_2() == 0);
}
