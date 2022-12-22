#![feature(array_windows)]
#![feature(let_chains)]
#![feature(test)]

use itertools::iproduct;
use std::ops::RangeInclusive;

use aoc2022::*;

const DAY: usize = 14;

type Grid = Vec<Vec<bool>>;
type Parsed = (Grid, usize);

const START: (usize, usize) = (500, 0);

main!();

fn parse_input(input: &str) -> Parsed {
    let mut max_y = 0;
    let paths: Vec<Vec<(usize, usize)>> = input
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|pt_s| {
                    let split = pt_s.split_once(',').unwrap();
                    let pt = (split.0.parse().unwrap(), split.1.parse().unwrap());
                    max_y = max_y.max(pt.1);
                    pt
                })
                .collect()
        })
        .collect();

    debug_assert!(max_y < START.0, "won't fit our grid");
    // +2 and +3 respectively to fit part_2
    let mut grid = vec![vec![false; START.0 + max_y + 2]; max_y + 3];

    let wall_points = paths.iter().flat_map(|path| {
        path.array_windows().flat_map(|[s, e]| {
            iproduct!(range_incl_any_dir(s.0, e.0), range_incl_any_dir(s.1, e.1))
        })
    });
    for (x, y) in wall_points {
        grid[y][x] = true;
    }
    (grid, max_y)
}

fn part_1((paths, max_y): &Parsed) -> usize {
    drop_sand(paths, *max_y, false)
}

fn part_2((paths, max_y): &Parsed) -> usize {
    drop_sand(paths, *max_y + 2, true)
}

const X_STEPS: [isize; 3] = [0, -1, 1];

fn drop_sand(grid: &Grid, max_y: usize, has_floor: bool) -> usize {
    let mut grid = grid.clone();
    let mut count = 0;

    'outer: loop {
        let mut cur = START;
        loop {
            if !has_floor && cur.1 == max_y {
                break 'outer;
            }
            let maybe_next = X_STEPS.iter().find_map(|xstep| {
                let next = ((cur.0 as isize + xstep) as usize, cur.1 + 1);
                (!grid[next.1][next.0]).then_some(next)
            });
            if let Some(next) = maybe_next && (!has_floor || next.1 != max_y) {
                cur = next;
            } else {
                grid[cur.1][cur.0] = true;
                count += 1;
                if cur == START {
                    break 'outer;
                }
                break;
            }
        }
    }
    count
}

#[inline(always)]
fn range_incl_any_dir<T: Ord + Copy>(start: T, end: T) -> RangeInclusive<T> {
    start.min(end)..=end.max(start)
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        498,4 -> 498,6 -> 496,6\n\
        503,4 -> 502,4 -> 502,9 -> 494,9\n\
        ";

    test!(part_1() == 24);
    test!(part_2() == 93);
    bench_parse!(
        |p: &Parsed| { p.0.iter().flat_map(|l| l.iter()).filter(|x| **x).count() },
        614
    );
    bench!(part_1() == 614);
    bench!(part_2() == 26170);
}
