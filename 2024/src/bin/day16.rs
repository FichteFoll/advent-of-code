#![feature(gen_blocks)]
#![feature(test)]

use std::{collections::BTreeSet, iter::Peekable};

use aoc2024::*;
use collections::{HashSet, HashMap};
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 16;

type Parsed = Grid2D<char>;
type P = Point<2>;
type I = usize;
type ItItem = (I, Vec<P>);

const COST_MOVE: I = 1;
const COST_ROTATE: I = 1000;

// Non-standard `main`
// since we only need to compute once that way
// and this code takes time.
fn main() {
    let input = read_file(DAY);
    let grid = parse_input(&input);
    let mut it = solve(&grid).peekable();
    println!("Part 1: {}", part_1_it(&mut it));
    println!("Part 2: {}", part_2_it(it));
}

fn parse_input(input: &str) -> Parsed {
    input.lines().map(str::chars).collect()
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
struct State {
    cost: I,
    /// Prevent turning twice in a row.
    just_turned: bool,
    pos: P,
    dir: P,
    path: Vec<P>,
}

fn part_1_it<T: Iterator<Item = ItItem>>(it: &mut Peekable<T>) -> I {
    it.peek().unwrap().0
}

fn part_2_it<T: Iterator<Item = ItItem>>(it: T) -> usize {
    it.map(|(_, s)| s)
        .fold(HashSet::default(), |mut acc, pts| {
            acc.extend(pts);
            acc
        })
        .len()
}

fn solve(grid: &Parsed) -> impl Iterator<Item = ItItem> {
    // Basically Dijkstra.
    let start_pos = grid.position(|c| c == &'S').unwrap();
    let start = State {
        pos: start_pos,
        dir: P::E,
        path: [start_pos].into(),
        ..Default::default()
    };
    let end_pt = grid.position(|c| c == &'E').unwrap();
    let mut queue: BTreeSet<State> = [start].into();
    let mut min_map: HashMap<(P, P), I> = Default::default();
    let mut result = None;
    // Use the experimental `gen` block feature because why not.
    gen move {
        while let Some(s) = queue.pop_first() {
            let min_entry = min_map.entry((s.pos, s.dir)).or_insert(I::MAX);
            if *min_entry < s.cost {
                continue;
            }
            *min_entry = s.cost;
            if result.is_some_and(|r| r < s.cost) {
                break;
            }
            if s.pos == end_pt {
                // print_path(grid, &s.path);
                result = Some(s.cost);
                yield (s.cost, s.path);
                continue;
            }
            let n_pos = s.pos + s.dir;
            match grid.get(&n_pos).unwrap() {
                &'.' | &'E' => {
                    let mut n_path = s.path.clone();
                    n_path.push(n_pos);
                    queue.insert(State {
                        cost: s.cost + COST_MOVE,
                        just_turned: false,
                        pos: n_pos,
                        dir: s.dir,
                        path: n_path,
                    });
                }
                &'#' | &'S' => (), // it makes no sense to go over S again
                c => panic!("Unexpected char {c}"),
            }
            if !s.just_turned {
                let left = State {
                    cost: s.cost + COST_ROTATE,
                    dir: s.dir.rotated_left(90),
                    just_turned: true,
                    ..s
                };
                let right = State {
                    dir: -left.dir,
                    ..left.clone()
                };
                queue.insert(left);
                queue.insert(right);
            }
        }
    }
}

#[allow(dead_code)]
fn print_path(grid: &Parsed, path: &[P]) {
    let mut grid = grid.clone();
    for p in path {
        grid[p] = 'O';
    }
    println!("{grid}");
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use grid2d::Size;

    // Implement part functions for testing macros.
    fn part_1(grid: &Parsed) -> I {
        part_1_it(&mut solve(grid).peekable())
    }

    fn part_2(grid: &Parsed) -> usize {
        part_2_it(solve(grid))
    }

    const TEST_INPUT_1: &str = "\
        ###############\n\
        #.......#....E#\n\
        #.#.###.#.###.#\n\
        #.....#.#...#.#\n\
        #.###.#####.#.#\n\
        #.#.#.......#.#\n\
        #.#.#####.###.#\n\
        #...........#.#\n\
        ###.#.#####.#.#\n\
        #...#.....#.#.#\n\
        #.#.#.###.#.#.#\n\
        #.....#...#.#.#\n\
        #.###.#.#.#.#.#\n\
        #S..#.....#...#\n\
        ###############\n\
        ";

    const TEST_INPUT_2: &str = "\
        #################\n\
        #...#...#...#..E#\n\
        #.#.#.#.#.#.#.#.#\n\
        #.#.#.#...#...#.#\n\
        #.#.#.#.###.#.#.#\n\
        #...#.#.#.....#.#\n\
        #.#.#.#.#.#####.#\n\
        #.#...#.#.#.....#\n\
        #.#.#####.#.###.#\n\
        #.#.#.......#...#\n\
        #.#.###.#####.###\n\
        #.#.#...#.....#.#\n\
        #.#.#.#####.###.#\n\
        #.#.#.........#.#\n\
        #.#.#.#########.#\n\
        #S#.............#\n\
        #################\n\
        ";

    test!(ex1, TEST_INPUT_1, part_1() == 7036);
    test!(ex2, TEST_INPUT_2, part_1() == 11048);
    test!(ex1, TEST_INPUT_1, part_2() == 45);
    test!(ex2, TEST_INPUT_2, part_2() == 64);
    bench_parse!(|p: &Parsed| p.size, Size(141, 141));
    bench!(part_1() == 109496);
    bench!(part_2() == 551);
}
