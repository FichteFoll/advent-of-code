#![feature(array_windows)]
#![feature(let_chains)]
#![feature(test)]

use itertools::iproduct;
use std::ops::RangeInclusive;

use aoc2022::*;
use aoc2022::collections::*;
use aoc2022::coord::Point;


const DAY: usize = 14;

type Parsed = (Vec<Vec<Point<2>>>, i32);

#[derive(Clone, Copy)]
enum Tile {
    // Air,
    Wall,
    Sand,
}

main!();

fn parse_input(input: &str) -> Parsed {
    let mut max_y = 0;
    let paths = input
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|pt_s| {
                    let split = pt_s.split_once(',').unwrap();
                    let (x, y) = (split.0.parse().unwrap(), split.1.parse().unwrap());
                    max_y = max_y.max(y);
                    Point([x, y])
                })
                .collect()
        }).collect();
    (paths, max_y)
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

fn drop_sand(paths: &Vec<Vec<Point<2>>>, max_y: i32, has_floor: bool) -> usize {
    let mut the_map: HashMap<Point<2>, Tile> =
        paths.iter()
            .flat_map(|path| path.array_windows())
            .flat_map(|[s, e]| {
                iproduct!(range_incl_any_dir(s.x(), e.x()), range_incl_any_dir(s.y(), e.y()))
            })
            .map(|pt| (pt.into(), Tile::Wall))
            .collect();
    let num_wall_tiles = the_map.len();

    'outer: loop {
        let mut current = START;
        loop {
            if current.y() > max_y {
                break 'outer;
            }
            let maybe_next = STEPS.iter()
                .find_map(|step| {
                    let next = &current + step;
                    (!the_map.contains_key(&next)).then_some(next)
                });
            if let Some(next) = maybe_next && (!has_floor || next.y() != max_y) {
                current = next;
            } else {
                the_map.insert(current, Tile::Sand);
                if current == START {
                    break 'outer;
                }
                break;
            }
        }
    }
    the_map.len() - num_wall_tiles
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
    bench_parse!(|p: &Parsed| p.0.len(), 151);
    bench!(part_1() == 614);
    bench!(part_2() == 26170);
}
