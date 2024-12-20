#![feature(let_chains)]
#![feature(test)]

use std::{cmp::Reverse, collections::BTreeSet};

use aoc2024::*;
use collections::HashMap;
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 20;

type Parsed = Grid2D<char>;
type P = Point<2>;
type I = i32;

main!(100);

fn parse_input(input: &str) -> Parsed {
    input.lines().map(|line| line.chars()).collect()
}

fn part_1(grid: &Parsed, minimum_saved: I) -> usize {
    let mut solutions = find_paths_with_cheats(grid, minimum_saved);
    assert!(!solutions.is_empty());
    dbg!(&solutions.len());
    let reference = solutions.remove(&vec![]).unwrap();
    solutions
        .into_values()
        .filter(|&length| minimum_saved <= reference - length)
        .count()
}

fn part_2(_parsed: &Parsed, _unused: usize) -> usize {
    todo!()
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug)]
struct State {
    // prioritize the no-cheats solution as an early cut-off point
    cheats: Vec<P>,
    min_steps: I,
    steps: Reverse<I>,
    pos: P,
}

fn find_paths_with_cheats(grid: &Parsed, minimum_saved: I) -> HashMap<Vec<P>, I> {
    let end = grid.position(|c| c == &'E').unwrap();
    let start = grid.position(|c| c == &'S').unwrap();
    let mut cache: HashMap<(P, Vec<P>), I> = Default::default();
    let mut queue: BTreeSet<State> = [State {
        min_steps: start.manhattan_to(&end),
        steps: Reverse(0),
        pos: start,
        cheats: Vec::with_capacity(2),
    }]
    .into();
    let no_cheats_key = (end, vec![]);
    while let Some(s) = queue.pop_first() {
        let entry = cache.entry((s.pos, s.cheats.clone())).or_insert(I::MAX);
        if *entry <= s.steps.0 {
            continue;
        }
        *entry = s.steps.0;
        if s.pos == end {
            println!("solution: {:?}", s);
            continue;
        }
        if let Some(&shortest) = cache.get(&(end, s.cheats.clone()))
            && shortest < s.min_steps
        {
            continue;
        }
        if let Some(&shortest_no_cheats) = cache.get(&no_cheats_key)
            && shortest_no_cheats < s.min_steps + minimum_saved
        {
            continue;
        }
        let can_cheat = s.cheats.len() == 0;
        let is_cheating = s.cheats.len() == 1;
        let next = s.pos.direct_neighbors().into_iter().filter_map(|pos| {
            let is_wall = grid.get(&pos)? == &'#';
            let mut cheats = s.cheats.clone();
            if is_wall && !can_cheat {
                return None;
            }
            if is_cheating || is_wall {
                cheats.push(pos);
            }
            Some(State {
                min_steps: s.steps.0 + 1 + pos.manhattan_to(&end),
                steps: Reverse(s.steps.0 + 1),
                pos,
                cheats,
            })
        });
        queue.extend(next);
    }
    dbg!(cache.len());
    cache
        .into_iter()
        .filter_map(|((pt, c), v)| (pt == end).then_some((c, v)))
        .collect()
}

#[cfg(test)]
mod tests {
    use grid2d::Size;

    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        ###############\n\
        #...#...#.....#\n\
        #.#.#.#.#.###.#\n\
        #S#...#.#.#...#\n\
        #######.#.#.###\n\
        #######.#.#...#\n\
        #######.#.###.#\n\
        ###..E#...#...#\n\
        ###.#######.###\n\
        #...###...#...#\n\
        #.#####.#.###.#\n\
        #.#...#.#.#...#\n\
        #.#.#.#.#.#.###\n\
        #...#...#...###\n\
        ###############\n\
        ";

    test!(part_1(2) == 44);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| p.size, Size(141, 141));
    // bench!(part_1(100) == 1327); // takes 1.5 minutes to compute
    // bench!(part_2() == 0);
}
