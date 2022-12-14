#![feature(cell_update)]
#![feature(let_chains)]
#![feature(test)]

use itertools::Itertools;
use itertools::iproduct;
use std::cell::Cell;
use std::ops::RangeInclusive;

use aoc2022::*;
use aoc2022::collections::*;
use aoc2022::coord::Point;


const DAY: usize = 14;

type Parsed = (HashSet<Point<2>>, i32);

main!();

fn parse_input(input: &str) -> Parsed {
    let max_y = Cell::new(0);
    let occupied = input
        .lines()
        .flat_map(|line| {
            line.split(" -> ")
                .map(|pt_s| {
                    let split = pt_s.split_once(',').unwrap();
                    let pt: (i32, i32) = (split.0.parse().unwrap(), split.1.parse().unwrap());
                    max_y.update(|y| y.max(pt.1));
                    pt
                })
                .tuple_windows()
                .flat_map(|(s, e) | {
                    iproduct!(range_incl_any_dir(s.0, e.0), range_incl_any_dir(s.1, e.1))
                })
                .map(|(x,  y)| Point([x, y]))
        }).collect();
    (occupied, max_y.into_inner())
}

const START: Point<2> = Point([500, 0]);
const STEPS: [Point<2>; 3] = [
    Point::<2>::S,
    Point::<2>::SW,
    Point::<2>::SE,
];

fn part_1((paths, max_y): &Parsed) -> usize {
    drop_sand(paths, *max_y, false)
}

fn part_2((paths, max_y): &Parsed) -> usize {
    drop_sand(paths, *max_y + 2, true)
}

fn drop_sand(occupied: &HashSet<Point<2>>, max_y: i32, has_floor: bool) -> usize {
    let mut occupied = occupied.clone();
    let num_wall_tiles = occupied.len();

    'outer: loop {
        let mut current = START;
        loop {
            if current.y() > max_y {
                break 'outer;
            }
            let maybe_next = STEPS.iter()
                .find_map(|step| {
                    let next = &current + step;
                    (!occupied.contains(&next)).then_some(next)
                });
            if let Some(next) = maybe_next && (!has_floor || next.y() != max_y) {
                current = next;
            } else {
                occupied.insert(current);
                if current == START {
                    break 'outer;
                }
                break;
            }
        }
    }
    occupied.len() - num_wall_tiles
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
    bench_parse!(|p: &Parsed| p.0.len(), 614);
    bench!(part_1() == 614);
    bench!(part_2() == 26170);
}
