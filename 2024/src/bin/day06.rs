#![feature(test)]

use std::iter::successors;

use aoc2024::*;
use collections::HashSet;
use grid2d::Grid2D;
use itertools::Itertools;
use point::Point;

const DAY: usize = 6;

type Parsed = Grid2D<char>;

static OBJECT: char = '#';
static GUARD: char = '^';

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(str::chars).collect()
}

fn part_1(grid: &Parsed) -> usize {
    walk_guard(grid).map(|(pt, _)| pt).unique().count()
}

fn part_2(grid: &Parsed) -> usize {
    let mut walked = HashSet::default();
    walk_guard(grid)
        .filter(|&state| {
            walked.insert(state.0.clone());
            let object = state.0 + state.1;
            !walked.contains(&object)
                && grid.get(&object).is_some_and(|c| *c != OBJECT)
                && !is_finite(grid, state, &object)
        })
        .count()
}

fn walk_guard(grid: &Parsed) -> impl Iterator<Item = (Point<2>, Point<2>)> {
    successors(
        Some((guard_pos(grid), Point::<2>::N)),
        |&(mut pos, mut dir)| {
            if grid.get(&(pos + dir))? == &OBJECT {
                dir.rotate_right(90);
            } else {
                pos += dir;
            }
            Some((pos, dir))
        },
    )
}

fn is_finite(grid: &Parsed, start: (Point<2>, Point<2>), object: &Point<2>) -> bool {
    successors(Some(start), |&(mut pos, mut dir)| {
        let next = pos + dir;
        if &next == object || grid.get(&next)? == &OBJECT {
            dir.rotate_right(90);
        } else {
            pos += dir;
        }
        Some((pos, dir))
    })
    .all_unique()
}

fn guard_pos(grid: &Grid2D<char>) -> Point<2> {
    grid.position(|c| *c == GUARD).unwrap()
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
    test!(part_2() == 6);
    bench_parse!(|p: &Parsed| p.size, Size(130, 130));
    bench!(part_1() == 5239);
    bench!(part_2() == 1753);
}
