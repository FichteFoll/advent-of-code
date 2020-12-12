#![feature(test, bool_to_option, exclusive_range_pattern)]

use std::{fmt::{Display, Error, Formatter}, iter::successors};

use aoc2020::*;
use aoc2020::grid::*;


const DAY: usize = 11;
type Input = Grid<Seat>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Seat {
    Floor,
    Empty,
    Occupied,
}

impl Display for Seat {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Seat::*;
        f.write_str(match self {
            Floor => ".",
            Empty => "L",
            Occupied => "#",
        })
    }
}

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| line.bytes()
            .map(|b| match b {
                b'.' => Seat::Floor,
                b'L' => Seat::Empty,
                b'#' => Seat::Occupied,
                _ => panic!("Bad input"),
            })
        )
        .collect()
}

fn next_layout(grid: &Input) -> Grid<Seat> {
    use Seat::*;
    grid.new_map_enumerate(|pt, seat| {
        let occupied = grid.neighbor_values(pt).into_iter()
            .filter(|s| **s == Occupied)
            .count();
        match (seat, occupied) {
            (Empty, 0) => Occupied,
            (Occupied, 4..=8) => Empty,
            _ => *seat,
        }
    })
}

fn part_1(input: &Input) -> usize {
    successors(
        Some(input.clone()),
        |prev| {
            let next = next_layout(&prev);
            (*prev != next).then_some(next)
        }
    )
        .last()
        .map(|g| g.iter().filter(|s| **s == Seat::Occupied).count())
        .unwrap()
}

fn part_2(_input: &Input) -> usize {
    0
}

fn main() {
    let input_str = read_input!();
    let input = parse_input(&input_str);
    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT_STR: &str = "\
        L.LL.LL.LL\n\
        LLLLLLL.LL\n\
        L.L.L..L..\n\
        LLLL.LL.LL\n\
        L.LL.LL.LL\n\
        L.LLLLL.LL\n\
        ..L.L.....\n\
        LLLLLLLLLL\n\
        L.LLLLLL.L\n\
        L.LLLLL.LL\n\
        ";

    test!(part_1() == 37);
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    bench!(part_1() == 2468);
    // bench!(part_2() == 0);
}
