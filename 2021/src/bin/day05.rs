#![feature(test)]

use std::collections::HashMap;

use either::Either::{self, Left, Right};
use itertools::iproduct;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 5;

type Point = (usize, usize);
type Line = (Point, Point);
// left: vertical/horizontal, right: diagonal
type ELine = Either<Line, Line>;
type Parsed = Vec<ELine>;

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
                let line = (parse_pt(left), parse_pt(right));
                if line.0.0 == line.1.0 || line.0.1 == line.1.1 {
                    Left(line)
                } else {
                    Right(line)
                }
            })
            .collect()
    }

    fn parse_pt(s: &str) -> Point {
        let (x, y) = s.split_once(',').expect("no comma");
        (x.parse().unwrap(), y.parse().unwrap())
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let pts = parsed.iter()
        .filter_map(|eline| eline.left())
        .flat_map(pts_for_hv);
    count_overlaps(pts)
}

fn part_2(parsed: &Parsed) -> usize {
    let pts = parsed.iter()
        .flat_map(|eline| eline.either(pts_for_hv, pts_for_diag));
    count_overlaps(pts)
}

fn pts_for_hv(line: Line) -> Vec<Point> {
    iproduct!(
        line.0.0.min(line.1.0)..=line.0.0.max(line.1.0),
        line.0.1.min(line.1.1)..=line.0.1.max(line.1.1)
    ).collect()
}

fn pts_for_diag(line: Line) -> Vec<Point> {
    let mut x_iter: Box<dyn DoubleEndedIterator<Item=_>>
        = Box::new(line.0.0.min(line.1.0)..=line.0.0.max(line.1.0));
    if line.0.0 > line.1.0 {
        x_iter = Box::new(x_iter.rev());
    }
    let mut y_iter: Box<dyn DoubleEndedIterator<Item=_>>
        = Box::new(line.0.1.min(line.1.1)..=line.0.1.max(line.1.1));
    if line.0.1 > line.1.1 {
        y_iter = Box::new(y_iter.rev());
    }
    x_iter.zip(y_iter).collect()
}

fn count_overlaps(pts: impl Iterator<Item=Point>) -> usize {
    let mut grid = HashMap::new();
    for pt in pts {
        *grid.entry(pt).or_insert(0) += 1;
    }
    grid.into_values().filter(|&n| n >= 2).count()
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
