#![feature(test)]

use aoc2024::*;
use itertools::Itertools;
use point::Point;
use grid2d::Grid2D;

const DAY: usize = 6;

type Parsed = Grid2D<char>;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(str::chars).collect()
}

fn part_1(grid: &Parsed) -> usize {
    let start = grid.position(|c| *c == '^').unwrap();
    std::iter::successors(Some((start, Point::<2>::N)), |&(mut pos, mut dir)| {
        if grid.get(&(pos + dir))? == &'#' {
            dir.rotate_right(90);
        }
        pos += dir;
        Some((pos, dir))
    })
        .map(|(pt, _)| pt)
        .unique()
        .count()
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
        ....#.....\n\
        .........#\n\
        ..........\n\
        ..#.......\n\
        .......#..\n\
        ..........\n\
        .#..^.....\n\
        ........#.\n\
        #.........\n\
        ......#...\n\
        ";

    test!(part_1() == 41);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| p.size, Size(130, 130));
    bench!(part_1() == 5239);
    // bench!(part_2() == 0);
}
