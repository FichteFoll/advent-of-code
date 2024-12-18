#![feature(let_chains)]
#![feature(test)]

use std::{cmp::Reverse, collections::BTreeSet};

use aoc2024::*;
use collections::{HashMap, HashSet};
use grid2d::Size;
use point::Point;

const DAY: usize = 18;

type Parsed = Vec<P>;
type P = Point<2>;
type I = i32;

main!(Size(71, 71), 1024);

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(',').unwrap();
            Point([x.parse().unwrap(), y.parse().unwrap()])
        })
        .collect()
}

fn part_1(parsed: &Parsed, size: Size, n_items: usize) -> I {
    let obstacles: HashSet<_> = parsed.iter().cloned().take(n_items).collect();
    find_path(&obstacles, size).expect("No solution found")
}

fn part_2(parsed: &Parsed, size: Size, min_n_items: usize) -> P {
    (min_n_items..parsed.len())
        .filter_map(|n_items| {
            let obstacles: HashSet<_> = parsed.iter().cloned().take(n_items).collect();
            find_path(&obstacles, size).map(|_| parsed[n_items])
        })
        .last()
        .expect("No solution found")
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug)]
struct State {
    min_steps: I,
    steps: Reverse<I>,
    pos: P,
}

fn find_path(obstacles: &HashSet<P>, size: Size) -> Option<I> {
    let end = Point([size.0 as i32 - 1, size.1 as i32 - 1]);
    let mut cache: HashMap<P, I> = Default::default();
    let mut queue: BTreeSet<State> = [State {
        min_steps: end.manhattan(),
        steps: Reverse(0),
        pos: Point([0, 0]),
    }]
    .into();
    while let Some(s) = queue.pop_first() {
        if s.pos == end {
            return Some(s.steps.0);
        }
        let entry = cache.entry(s.pos).or_insert(I::MAX);
        if *entry <= s.steps.0 {
            continue;
        }
        *entry = s.steps.0;
        let next = s
            .pos
            .direct_neighbors()
            .into_iter()
            .filter(|pt| size.contains(pt))
            .filter(|pt| !obstacles.contains(pt))
            .map(|pos| State {
                min_steps: s.steps.0 + 1 + pos.manhattan_to(&end),
                steps: Reverse(s.steps.0 + 1),
                pos,
            });
        queue.extend(next);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        5,4\n\
        4,2\n\
        4,5\n\
        3,0\n\
        2,1\n\
        6,3\n\
        2,4\n\
        1,5\n\
        0,6\n\
        3,3\n\
        2,6\n\
        5,1\n\
        1,2\n\
        5,5\n\
        2,5\n\
        6,5\n\
        1,4\n\
        0,4\n\
        6,4\n\
        1,1\n\
        6,1\n\
        1,0\n\
        0,5\n\
        1,6\n\
        2,0\n\
        ";

    test!(part_1(Size(7, 7), 12) == 22);
    test!(part_2(Size(7, 7), 12) == Point([6, 1]));
    bench_parse!(Vec::len, 3450);
    bench!(part_1(Size(71, 71), 1024) == 304);
    // Takes about 1.4s, which is not comfortably benchable.
    // bench!(part_2(Size(71, 71), 1024) == Point([50, 28]));
}
