#![feature(bool_to_option)]
#![feature(test)]

use aoc2021::*;
use aoc2021::grid2d::Grid2D;
use parse::parse_input;

const DAY: usize = 9;

type Parsed = Grid2D<u8>;

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
            .map(|line| line.bytes().map(|b| b - b'0'))
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter_enumerate()
        .filter_map(|(pt, n)|
            pt.direct_neighbors().into_iter()
                .filter_map(|pt| parsed.get(&pt))
                .all(|m| n < m)
                .then_some(*n as usize + 1)
        )
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
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
    // test!(part_2() == 0);
    bench_parse!(|x: &Grid2D<_>| x.size, (100, 100));
    bench!(part_1() == 452);
    // bench!(part_2() == 0);
}
