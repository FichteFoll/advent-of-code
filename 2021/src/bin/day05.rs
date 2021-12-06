#![feature(test)]
#![feature(bool_to_option)]

use std::collections::HashMap;
use std::iter::successors;

use aoc2021::*;
use aoc2021::coord::Point;
use parse::parse_input;

const DAY: usize = 5;

#[derive(Clone, Copy, Hash, Debug)]
pub struct Line { from: Point<2>, to: Point<2> }

type Parsed = Vec<Line>;

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
            .filter_map(|line| line.split_once(" -> "))
            .map(|(left, right)| Line { from: parse_pt(left), to: parse_pt(right) })
            .collect()
    }

    fn parse_pt(s: &str) -> Point<2> {
        let (x, y) = s.split_once(',').expect("no comma");
        Point::new([x.parse().unwrap(), y.parse().unwrap()])
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let lines = parsed.iter()
        .filter(|line| line.from.x() == line.to.x() || line.from.y() == line.to.y());
    count_overlaps(lines)
}

fn part_2(parsed: &Parsed) -> usize {
    count_overlaps(parsed.iter())
}

fn count_overlaps<'a>(lines: impl Iterator<Item=&'a Line>) -> usize {
    let mut grid = HashMap::new();
    for pt in lines.flat_map(expand_line) {
        *grid.entry(pt).or_insert(0) += 1;
    }
    grid.into_values().filter(|&n| n >= 2).count()
}

fn expand_line(line: &'_ Line) -> impl Iterator<Item=Point<2>> + '_ {
    let dir = |a, b| (a < b) as i32 - (a > b) as i32;
    let step = Point::new([
        dir(line.from.x(), line.to.x()),
        dir(line.from.y(), line.to.y()),
    ]);
    successors(Some(line.from), move |&prev| {
        (prev != line.to).then_some(prev + step)
    })
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
