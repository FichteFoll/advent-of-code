#![feature(test)]

use std::collections::{VecDeque, BTreeSet};

use itertools::Itertools;

use aoc2021::*;
use aoc2021::grid2d::Grid2D;
use aoc2021::coord::Point;

const DAY: usize = 9;

type Parsed = Grid2D<u8>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split('\n')
        .map(|line| line.bytes().map(|b| b - b'0'))
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    low_points(parsed)
        .map(|(_, &n)| n as usize + 1)
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    basins(parsed).iter()
        .sorted_unstable()
        .rev()
        .take(3)
        .product()
}

fn low_points(parsed: &Parsed) -> impl Iterator<Item=(Point<2>, &u8)> {
    parsed.iter_enumerate()
        .filter(|(pt, &n)|
            pt.direct_neighbors().into_iter()
                .filter_map(|pt| parsed.get(&pt))
                .all(|&m| n < m)
        )
}

fn basins(parsed: &Parsed) -> Vec<usize> {
    low_points(parsed)
        .map(|(pt, _)| {
            // use A* to fill basins
            let mut queue: VecDeque<_> = [pt].into();
            let mut pts = BTreeSet::new();
            while let Some(cur_pt) = queue.pop_front() {
                if !pts.insert(cur_pt) {
                    continue;
                }
                let new = cur_pt.direct_neighbors().into_iter()
                    .filter(|pt| parsed.get(pt).filter(|&&n| n != 9).is_some());
                queue.extend(new);
            }
            pts.len()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        2199943210\n\
        3987894921\n\
        9856789892\n\
        8767896789\n\
        9899965678\n\
        ";

    test!(part_1() == 15);
    test!(part_2() == 1134);
    bench_parse!(|x: &Grid2D<_>| x.size, (100, 100));
    bench!(part_1() == 452);
    bench!(part_2() == 1263735);
}
