#![feature(test, exclusive_range_pattern)]

use std::{fmt::{Display, Error, Formatter}, iter::successors};

use aoc2020::*;
use aoc2020::grid2d::*;

const DAY: usize = 11;
type Input = Grid2D<Seat>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Seat {
    Floor,
    Empty,
    Occupied,
}
use Seat::*;

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

fn part_1(input: &Input) -> usize {
    successors(
        Some(input.clone()),
        |prev|
            Some(prev.new_map_enumerate(|pt, seat| {
                let num_occupied = prev.neighbor_values(pt).into_iter()
                    .filter(|s| **s == Occupied)
                    .count();
                match (seat, num_occupied) {
                    (Empty, 0) => Occupied,
                    (Occupied, 4..=8) => Empty,
                    _ => *seat,
                }
            })).filter(|x| x != prev)
    )
        .last()
        .map(|g| g.iter().filter(|s| **s == Occupied).count())
        .unwrap()
}

fn part_2(input: &Input) -> usize {
    let directions = Point::default().neighbors();
    successors(
        Some(input.clone()),
        |prev|
            Some(prev.new_map_enumerate(|pt, seat| {
                let num_occupied = directions.iter()
                    .filter(|&dir| {
                        successors(Some(pt + dir), |pos| Some(pos + dir))
                            .map_while(|pos| prev.get(&pos))
                            .find_map(|&seat| match seat {
                                Floor => None,
                                Empty => Some(false),
                                Occupied => Some(true),
                            })
                            .unwrap_or(false)
                    })
                    .count();
                match (seat, num_occupied) {
                    (Empty, 0) => Occupied,
                    (Occupied, 5..=8) => Empty,
                    _ => *seat,
                }
            })).filter(|x| x != prev)
    )
        .last()
        .map(|g| g.iter().filter(|&s| *s == Occupied).count())
        .unwrap()
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
    test!(part_2() == 26);
    bench_parse!();
    bench!(part_1() == 2468);
    bench!(part_2() == 2214);
}
