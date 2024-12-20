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

fn part_1(grid: &Parsed, min_saved: I) -> usize {
    count_paths_with_cheats(grid, min_saved, 2)
}

fn part_2(grid: &Parsed, min_saved: I) -> usize {
    count_paths_with_cheats(grid, min_saved, 20)
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Debug)]
struct State {
    // prioritize the no-cheats solution as an early cut-off point
    cheated: usize,
    min_steps: I,
    steps: Reverse<I>,
    // rest is irrelevant for ordering
    pos: P,
    cheats: Vec<P>,
}

fn count_paths_with_cheats(grid: &Parsed, min_saved: I, allowed_cheats: usize) -> usize {
    let end = grid.position(|c| c == &'E').unwrap();
    let start = grid.position(|c| c == &'S').unwrap();
    let mut cache: HashMap<(P, Vec<P>), I> = Default::default();
    let mut queue: BTreeSet<State> = [State {
        cheated: 0,
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
            && shortest_no_cheats < s.min_steps + min_saved
        {
            continue;
        }
        queue.extend(successor_states(s, grid, allowed_cheats, end));
    }
    dbg!(cache.len());
    let reference = cache.remove(&no_cheats_key).unwrap();
    cache
        .into_iter()
        .filter_map(|((pt, _), v)| (pt == end).then_some(v))
        .filter(|&length| min_saved <= reference - length)
        .count()
}

fn successor_states(
    s: State,
    grid: &Grid2D<char>,
    allowed_cheats: usize,
    end: Point<2>,
) -> impl Iterator<Item = State> {
    let can_cheat = s.cheats.len() == 0;
    s.pos.direct_neighbors().into_iter().filter_map(move |pos| {
        let mut is_cheating = s.cheats.len() == 1;
        let is_wall = grid.get(&pos)? == &'#';
        let mut cheats = s.cheats.clone();
        match (is_wall, can_cheat, is_cheating) {
            (_, _, true) if s.cheated == allowed_cheats => return None,
            (true, true, _) => {
                cheats.push(s.pos);
                is_cheating = true;
            }
            (false, _, true) => {
                cheats.push(pos);
                is_cheating = false;
            }
            (true, false, false) => return None,
            _ => (),
        }
        Some(State {
            cheated: s.cheated + is_cheating as usize,
            min_steps: s.steps.0 + 1 + pos.manhattan_to(&end),
            steps: Reverse(s.steps.0 + 1),
            pos,
            cheats,
        })
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
