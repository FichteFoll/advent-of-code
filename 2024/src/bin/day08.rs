#![feature(test)]

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
    let mut antennas: HashMap<char, Vec<Point<2>>> = Default::default();
    let mut antinodes: HashSet<Point<2>> = Default::default();
    for (pt, &c) in grid.iter_enumerate() {
        if c == '.' {
            continue;
        }
        let mut ant = antennas.entry(c).or_default();
        antinodes.extend(
            ant.iter()
                .flat_map(|&a| {
                    let diff = a - pt;
                    [a + diff, pt - diff].into_iter()
                })
                .filter(|a| grid.size.contains(a)),
        );
        ant.push(pt);
    }
    antinodes.len()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
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
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| p.size, Size(50, 50));
    bench!(part_1() == 376);
    // bench!(part_2() == 0);
}
