#![feature(test)]

use std::collections::HashMap;

use either::Either::{self, Left, Right};
use itertools::iproduct;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 5;

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct Point { x: usize, y: usize }

#[derive(Clone, Copy, Hash)]
pub struct Line { from: Point, to: Point }

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
                let line = Line { from: parse_pt(left), to: parse_pt(right) };
                if line.from.x == line.to.x || line.from.y == line.to.y {
                    Left(line)
                } else {
                    Right(line)
                }
            })
            .collect()
    }

    fn parse_pt(s: &str) -> Point {
        let (x, y) = s.split_once(',').expect("no comma");
        Point { x: x.parse().unwrap(), y: y.parse().unwrap() }
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
        line.from.x.min(line.to.x)..=line.from.x.max(line.to.x),
        line.from.y.min(line.to.y)..=line.from.y.max(line.to.y)
    )
        .map(Point::from)
        .collect()
}

fn pts_for_diag(line: Line) -> Vec<Point> {
    let mut x_iter: Box<dyn DoubleEndedIterator<Item=_>>
        = Box::new(line.from.x.min(line.to.x)..=line.from.x.max(line.to.x));
    if line.from.x > line.to.x {
        x_iter = Box::new(x_iter.rev());
    }
    let mut y_iter: Box<dyn DoubleEndedIterator<Item=_>>
        = Box::new(line.from.y.min(line.to.y)..=line.from.y.max(line.to.y));
    if line.from.y > line.to.y {
        y_iter = Box::new(y_iter.rev());
    }
    x_iter.zip(y_iter).map(Point::from).collect()
}

fn count_overlaps(pts: impl Iterator<Item=Point>) -> usize {
    let mut grid = HashMap::new();
    for pt in pts {
        *grid.entry(pt).or_insert(0) += 1;
    }
    grid.into_values().filter(|&n| n >= 2).count()
}

impl From<(usize, usize)> for Point {
    fn from(tpl: (usize, usize)) -> Self {
        Self { x: tpl.0, y: tpl.1 }
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
    test!(part_2() == 12);
    bench_parse!(Vec::len, 500);
    bench!(part_1() == 6311);
    bench!(part_2() == 19929);
}
