#![feature(test)]

use std::collections::HashMap;

use itertools::iproduct;

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

fn part_1(parsed: &Parsed) -> usize {
    let mut grid = HashMap::new();
    let filtered = parsed.iter()
        .filter(|(from, to)| from.0 == to.0 || from.1 == to.1);
    for (from, to) in filtered {
        for pt in pts_for_hv(from, to) {
            *grid.entry(pt).or_insert(0) += 1;
        }
    }
    grid.values().filter(|&&n| n >= 2).count()
}

fn part_2(parsed: &Parsed) -> usize {
    let mut grid = HashMap::new();
    for (from, to) in parsed.iter() {
        let pts = if from.0 == to.0 || from.1 == to.1 {
            pts_for_hv(from, to)
        } else {
            pts_for_diag(from, to)
        };
        for pt in pts {
            *grid.entry(pt).or_insert(0) += 1;
        }
    }
    grid.values().filter(|&&n| n >= 2).count()
}

fn pts_for_hv(from: &Point, to: &Point) -> Vec<Point> {
    iproduct!(
        from.0.min(to.0)..=from.0.max(to.0),
        from.1.min(to.1)..=from.1.max(to.1)
    ).collect()
}

fn pts_for_diag(from: &Point, to: &Point) -> Vec<Point> {
    let mut x_iter: Box<dyn DoubleEndedIterator<Item=_>> = Box::new(from.0.min(to.0)..=from.0.max(to.0));
    if from.0 > to.0 {
        x_iter = Box::new(x_iter.rev());
    }
    let mut y_iter: Box<dyn DoubleEndedIterator<Item=_>> = Box::new(from.1.min(to.1)..=from.1.max(to.1));
    if from.1 > to.1 {
        y_iter = Box::new(y_iter.rev());
    }
    x_iter.zip(y_iter).collect()
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
    test!(part_2() == 12);
    bench_parse!(Vec::len, 500);
    bench!(part_1() == 6311);
    bench!(part_2() == 19929);
}
