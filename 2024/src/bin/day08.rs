#![feature(test)]

use std::iter::successors;

use aoc2024::*;
use collections::{HashMap, HashSet};
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 8;

type Parsed = Grid2D<char>;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(str::chars).collect()
}

fn part_1(grid: &Parsed) -> usize {
    solve(grid, |&a1, &a2| {
        let diff = a1 - a2;
        [a1 + diff, a2 - diff]
            .into_iter()
            .filter(|a| grid.size.contains(a))
            .collect()
    })
}

fn part_2(grid: &Parsed) -> usize {
    solve(grid, |&a1, &a2| {
        let diff = a1 - a2;
        let left = successors(Some(a1), |p| Some(p + &diff)).take_while(|p| grid.size.contains(p));
        let right = successors(Some(a2), |p| Some(p - &diff)).take_while(|p| grid.size.contains(p));
        left.chain(right).collect()
    })
}

fn solve<F>(grid: &Parsed, generate_antinodes: F) -> usize
where
    F: Fn(&Point<2>, &Point<2>) -> Vec<Point<2>>,
{
    let mut antennas: HashMap<char, Vec<Point<2>>> = Default::default();
    let mut antinodes: HashSet<Point<2>> = Default::default();
    for (pt, &c) in grid.iter_enumerate() {
        if c == '.' {
            continue;
        }
        let ant = antennas.entry(c).or_default();
        let new_nodes = ant.iter().flat_map(|a| generate_antinodes(a, &pt));
        antinodes.extend(new_nodes);
        ant.push(pt);
    }
    antinodes.len()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    use grid2d::Size;

    const TEST_INPUT: &str = "\
        ............\n\
        ........0...\n\
        .....0......\n\
        .......0....\n\
        ....0.......\n\
        ......A.....\n\
        ............\n\
        ............\n\
        ........A...\n\
        .........A..\n\
        ............\n\
        ............\n\
        ";

    test!(part_1() == 14);
    test!(part_2() == 34);
    bench_parse!(|p: &Parsed| p.size, Size(50, 50));
    bench!(part_1() == 376);
    bench!(part_2() == 1352);
}
