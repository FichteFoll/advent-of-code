#![feature(let_chains)]
#![feature(test)]

use std::{cmp::Reverse, collections::BTreeSet};

use aoc2024::*;
use collections::HashMap;
use grid2d::Grid2D;
use itertools::Itertools;
use point::Point;

const DAY: usize = 20;

type Parsed = Grid2D<char>;
type P = Point<2>;
type I = i32;

main!(100);

fn parse_input(input: &str) -> Parsed {
    input.lines().map(|line| line.chars()).collect()
}

fn part_1(grid: &Parsed, min_saved: I) -> usize {
    count_paths_with_cheats(grid, min_saved, 2)
}

fn part_2(grid: &Parsed, min_saved: I) -> usize {
    count_paths_with_cheats(grid, min_saved, 20)
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug)]
struct State {
    // Prioritize the no-cheats solution as an early cut-off point
    cheat: Cheat,
    // Priorizite shortest possible task (estimated)
    min_steps: I,
    // Prioritize longest current path (to get a "good" solution early)
    steps: Reverse<I>,
    // Position is irrelevant for ordering
    pos: P,
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug, Clone, Copy, Hash)]
enum Cheat {
    None,
    // TODO consider if different order makes it faster
    Active(usize, P),
    Done(P, P),
}

impl Cheat {
    fn is_done(&self) -> bool {
        if let Cheat::Done(..) = self {
            true
        } else {
            false
        }
    }
}

fn count_paths_with_cheats(grid: &Parsed, min_saved: I, allowed_cheats: usize) -> usize {
    let end = grid.position(|c| c == &'E').unwrap();
    let start = grid.position(|c| c == &'S').unwrap();
    let mut cache: HashMap<(P, Cheat), I> = Default::default();
    let mut queue: BTreeSet<State> = [State {
        cheat: Cheat::None,
        min_steps: start.manhattan_to(&end),
        steps: Reverse(0),
        pos: start,
    }]
    .into();
    let no_cheats_key = (end, Cheat::None);
    while let Some(s) = queue.pop_first() {
        let entry = cache.entry((s.pos, s.cheat)).or_insert(I::MAX);
        if *entry <= s.steps.0 {
            continue;
        }
        *entry = s.steps.0;
        if s.pos == end {
            println!("solution: {:?}", s);
            continue;
        }
        if let Some(&shortest) = cache.get(&(end, s.cheat))
            && shortest < s.steps.0 // TODO why does < vs <= make such a difference
        {
            continue;
        }
        if let Some(&shortest_no_cheats) = cache.get(&no_cheats_key)
            && shortest_no_cheats < s.min_steps + min_saved
        {
            continue;
        }
        queue.extend(successor_states(grid, allowed_cheats, end, s));
    }
    dbg!(cache.len());
    let reference = cache.remove(&no_cheats_key).unwrap();
    cache
        .into_iter()
        // .filter_map(|((pt, c), v)| (pt == end && c.is_done()).then_some(v))
        // .filter(|&length| reference - length >= min_saved)
        // .count()
        .filter_map(|((pt, c), v)| (pt == end && c.is_done()).then_some(v))
        .map(|length| reference - length)
        .filter(|saved| *saved >= min_saved)
        .counts()
        .into_iter()
        .sorted_unstable_by_key(|(k, _)| *k)
        .map(|(k, v)| {
            println!("{v} cheats that save {k} picoseconds");
            v
        })
        .sum()
}

fn successor_states(
    grid: &Grid2D<char>,
    allowed_cheats: usize,
    end: Point<2>,
    s: State,
) -> impl Iterator<Item = State> {
    s.pos.direct_neighbors().into_iter().flat_map(move |pos| {
        let Some(tile) = grid.get(&pos) else {
            return vec![];
        };
        let is_wall = tile == &'#';
        let cheats = match (is_wall, s.cheat) {
            (_, Cheat::Active(count, _)) if count == allowed_cheats => vec![],
            (true, Cheat::None) => vec![Cheat::Active(1, s.pos)],
            (true, Cheat::Done(..)) => vec![],
            (true, Cheat::Active(count, start)) => {
                vec![Cheat::Active(count + 1, start)]
            }
            (false, Cheat::Active(count, start)) => {
                vec![Cheat::Active(count + 1, start), Cheat::Done(start, pos)]
            }
            (false, c) => vec![c],
        };
        let min_steps = s.steps.0 + 1 + pos.manhattan_to(&end);
        let steps = Reverse(s.steps.0 + 1);
        cheats
            .into_iter()
            .map(|cheat| State {
                cheat,
                min_steps,
                steps,
                pos,
            })
            .collect()
    })
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
    test!(part_2(50) == 285);
    bench_parse!(|p: &Parsed| p.size, Size(141, 141));
    // bench!(part_1(100) == 1327); // takes 1.5 minutes to compute
    // bench!(part_2(100) == 0);
}
