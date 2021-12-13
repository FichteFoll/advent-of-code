#![feature(test)]

use std::collections::HashSet;

use aoc2021::*;
use aoc2021::coord::Point;
use parse::parse_input;

const DAY: usize = 13;

type Grid = HashSet<Point<2>>;
type Parsed = (Grid, Vec<Line>);

#[derive(Clone, Copy, Debug)]
pub enum Line{
    X(i32),
    Y(i32),
}

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use std::num::ParseIntError;
    use std::str::FromStr;
    use thiserror::Error;

    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let (pt_lines, fold_lines) = input.trim().split_once("\n\n").unwrap();
        let pts = pt_lines.lines().map(|line| {
            let coords: Vec<_> = line.split(',').map(|s| s.parse().unwrap()).collect();
            assert_eq!(coords.len(), 2);
            Point::new([coords[0], coords[1]])
        }).collect();

        let folds = fold_lines.lines()
            .map(|line| line.parse().unwrap())
            .collect();
        (pts, folds)
    }

    #[derive(Debug, Error)]
    pub enum ParseError {
        #[error("Unable to parse integer")]
        BadInt(#[from] ParseIntError),
        #[error("Couldn't find '='")]
        NoEquals,
        #[error("Axis wasn't 'x' or 'y'")]
        BadAxis,
    }

    impl FromStr for Line {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let split = s.split_once('=').ok_or(ParseError::NoEquals)?;
            let n = split.1.parse()?;
            assert!(n > 0);
            match split.0.chars().last().unwrap() {
                'x' => Ok(Line::X(n)),
                'y' => Ok(Line::Y(n)),
                _ => Err(ParseError::BadAxis),
            }
        }
    }
}

fn part_1((grid, folds): &Parsed) -> usize {
    let grid = grid.clone();
    fold(grid, folds[0]).len()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

fn fold(grid: Grid, line: Line) -> Grid {
    grid.into_iter()
        .map(|mut pt| {
            match line {
                Line::X(n) if pt.x() > n => *pt.x_mut() -= (pt.x() - n) * 2,
                Line::Y(n) if pt.y() > n => *pt.y_mut() -= (pt.y() - n) * 2,
                _ => (),
            };
            pt
        })
        .collect()
}

fn print_grid(grid: &Grid) {
    let max_x = grid.iter().map(|pt| pt.x()).max().unwrap();
    let max_y = grid.iter().map(|pt| pt.y()).max().unwrap();
    for y in 0..=max_y {
        for x in 0..=max_x {
            print!("{}", match grid.contains(&Point::new([x, y])) {
                true => '#',
                false => '.',
            });
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        6,10\n\
        0,14\n\
        9,10\n\
        0,3\n\
        10,4\n\
        4,11\n\
        6,0\n\
        6,12\n\
        4,1\n\
        0,13\n\
        10,12\n\
        3,4\n\
        3,0\n\
        8,4\n\
        1,10\n\
        2,14\n\
        8,10\n\
        9,0\n\
        \n\
        fold along y=7\n\
        fold along x=5\n\
        ";

    test!(part_1() == 17);
    // test!(part_2() == 0);
    bench_parse!(|x: &Parsed| (x.0.len(), x.1.len()), (897, 12));
    bench!(part_1() == 759);
    // bench!(part_2() == 0);
}
