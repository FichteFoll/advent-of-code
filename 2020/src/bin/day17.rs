#![feature(test)]

use std::collections::HashSet;

use aoc2020::{*, hashgrid::*, coord::*};

const DAY: usize = 17;
type Input = Vec<((usize, usize), bool)>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .enumerate()
        .flat_map(|(y, line)|
            line.bytes().enumerate()
                .map(move |(x, c)| ((x, y), c == b'#'))
        )
        .collect()
}

fn part_1(input: &Input) -> usize {
    let mut grid: HashGrid<Point3D, ()> =
        input.iter()
            .filter(|(_, b)| *b)
            .map(|(pos, _)| ((*pos).into(), ()))
            .collect();
    for _ in 0..6 {
        let points_of_interest: HashSet<_> =
            grid.keys()
                .flat_map(|pos| pos.neighbors())
                .collect();
        grid = points_of_interest.iter()
            .filter_map(|pos| {
                let active_neighbors = pos.neighbors().iter().flat_map(|p| grid.get(p)).count();
                match (grid.get(pos), active_neighbors) {
                    (Some(_), 2..=3) | (None, 3) => Some((pos.clone(), ())),
                    _ => None,
                }
            })
            .collect();
    }
    grid.values().count()
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
        .#.\n\
        ..#\n\
        ###\n\
        ";

    test!(part_1() == 112);
    // test!(part_2() == 0);
    bench!(part_1() == 375);
    // bench!(part_2() == 0);
}
