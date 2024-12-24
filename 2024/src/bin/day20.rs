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
    // Priorizite shortest possible task (estimated)
    min_steps: I,
    // Prioritize longest current path (to get a "good" solution early)
    steps: Reverse<I>,
    // Position is irrelevant for ordering
    pos: P,
}

fn count_paths_with_cheats(grid: &Parsed, min_saved: I, allowed_cheats: I) -> usize {
    let end = grid.position(|c| c == &'E').unwrap();
    let start = grid.position(|c| c == &'S').unwrap();
    // Save the minimum amount of steps to finish for each tile.
    // using a BFS from the end.
    let cache = bfs(grid, end, start);
    let reference = *cache.get(&start).unwrap();
    // Each cheat starts on a valid tile and ends on a floor tile
    // within a manhattan distance of N,
    // so we look at all valid permutations of floor tiles
    // and filter by the calculated saved steps.
    cache
        .iter()
        .tuple_combinations()
        .filter_map(|((p1, &d1), (p2, &d2))| {
            let dist = p1.manhattan_to(p2) as I;
            (dist != 0 && dist <= allowed_cheats).then_some(d1.abs_diff(d2) as I - dist)
        })
        .filter(|&saved| saved >= min_saved)
        .count()
}

fn bfs(grid: &Grid2D<char>, start: Point<2>, end: Point<2>) -> HashMap<Point<2>, I> {
    let mut cache: HashMap<P, I> = Default::default();
    let mut queue: BTreeSet<State> = [State {
        min_steps: start.manhattan_to(&end),
        steps: Reverse(0),
        pos: start,
    }]
    .into();
    while let Some(s) = queue.pop_first() {
        let entry = cache.entry(s.pos).or_insert(I::MAX);
        if *entry <= s.steps.0 {
            continue;
        }
        *entry = s.steps.0;
        let neighbors = s.pos.direct_neighbors().into_iter();
        let successors = neighbors
            .filter(|pos| grid.get(pos).is_some_and(|c| c != &'#'))
            .map(move |pos| State {
                min_steps: s.steps.0 + 1 + pos.manhattan_to(&end),
                steps: Reverse(s.steps.0 + 1),
                pos,
            });
        queue.extend(successors);
    }
    cache
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
    bench!(part_1(100) == 1327);
    bench!(part_2(100) == 985737);
}
