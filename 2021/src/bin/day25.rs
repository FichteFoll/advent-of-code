#![feature(bool_to_option)]
#![feature(test)]

use std::fmt::{Display, Write};

use aoc2021::*;
use aoc2021::coord::Point;
use aoc2021::grid2d::{Grid2D, Size};

const DAY: usize = 25;

type Parsed = Grid2D<Cucumber>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Cucumber {
    East,
    South,
    Empty,
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .lines()
        .map(|line|
            line.chars()
                .map(|c| match c {
                    '>' => Cucumber::East,
                    'v' => Cucumber::South,
                    '.' => Cucumber::Empty,
                    _ => panic!("bad char: {c}"),
                })
        )
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    (1..)
        .scan(parsed.clone(), |grid, i| {
            let (new_grid, moved) = step(grid);
            *grid = new_grid;
            // println!("{i}: {moved}");
            (moved > 0).then_some(i)
        })
        .last() // last index when something changed
        .unwrap() + 1
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn step(grid: &Grid2D<Cucumber>) -> (Grid2D<Cucumber>, usize) {
    let mut new_grid = grid.clone();
    let mut moved = 0;
    for test_cuc in [Cucumber::East, Cucumber::South] {
        let reference = new_grid.clone();
        for (pt, &cuc) in reference.iter_enumerate() {
            if cuc == test_cuc {
                let next_pt = cuc.next_pt(&pt, &grid.size);
                if reference.get(&next_pt) == Some(&Cucumber::Empty) {
                    new_grid.set(&pt, Cucumber::Empty);
                    new_grid.set(&next_pt, cuc);
                    moved += 1;
                }
            }
        }
    }
    (new_grid, moved)
}

impl Display for Cucumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            Cucumber::East => '>',
            Cucumber::South => 'v',
            Cucumber::Empty => '.',
        })
    }
}

impl Cucumber {
    fn next_pt(&self, pt: &Point<2>, size: &Size) -> Point<2> {
        match &self {
            Cucumber::East => Point::new([(pt.x() + 1) % size.0 as i32, pt.y()]),
            Cucumber::South => Point::new([pt.x(), (pt.y() + 1) % size.1 as i32]),
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        v...>>.vv>\n\
        .vv>>.vv..\n\
        >>.>v>...v\n\
        >>v>>.>.v.\n\
        v>v.vv.v..\n\
        >.>>..v...\n\
        .vv..>.>v.\n\
        v.v..>>v.v\n\
        ....v..v.>\n\
        ";

    test!(part_1() == 58);
    // test!(part_2() == 0);
    bench_parse!(|x: &Parsed| x.size, Size(139, 137));
    bench!(part_1() == 453);
    // bench!(part_2() == 0);

    #[test]
    fn step_single_line() {
        let mut grid = parse_input("...>>>>>...");
        let expected_steps = [
            (parse_input("...>>>>.>.."), 1),
            (parse_input("...>>>.>.>."), 2),
            (parse_input("...>>.>.>.>"), 3),
            (parse_input(">..>.>.>.>."), 4), // wraps around
            (parse_input(".>..>.>.>.>"), 5),
            (parse_input(">.>..>.>.>."), 5),
        ];
        for expected in expected_steps {
            let result = step(&grid);
            assert_eq!(&result, &expected);
            grid = result.0;
        }
    }

    #[test]
    fn step_simple_2d() {
        let grid = parse_input("\
            ..........\n\
            .>v....v..\n\
            .......>..\n\
            ..........\n\
        ");
        let expected = parse_input("\
            ..........\n\
            .>........\n\
            ..v....v>.\n\
            ..........\n\
        ");
        let result = step(&grid);
        assert_eq!(&result, &(expected.clone(), 3), "expected:\n{expected}\n, got:\n{}", result.0);
    }
}
