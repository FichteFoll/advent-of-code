#![feature(test)]

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;
use parse::parse_input;

const DAY: usize = 18;

type Parsed = HashSet<Point<3>>;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .lines()
            .map(|line| {
                line.split(',')
                    .map(|s| s.parse().unwrap())
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap()
            })
            .collect()
    }
}

fn part_1(points: &Parsed) -> usize {
    // Just a simple O(n**2) solution.
    points
        .iter()
        .map(|pt| {
            pt.direct_neighbors()
                .into_iter()
                .filter(|nb| !points.contains(nb))
                .count()
        })
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        2,2,2\n\
        1,2,2\n\
        3,2,2\n\
        2,1,2\n\
        2,3,2\n\
        2,2,1\n\
        2,2,3\n\
        2,2,4\n\
        2,2,6\n\
        1,2,5\n\
        3,2,5\n\
        2,1,5\n\
        2,3,5\n\
        ";

    test!(part_1() == 64);
    // test!(part_2() == 0);
    bench_parse!(HashSet::len, 2741);
    bench!(part_1() == 4628);
    // bench!(part_2() == 0);
}
