#![feature(gen_blocks)]
#![feature(test)]

use std::{collections::BTreeSet, iter::Peekable};

use aoc2024::*;
use collections::HashSet;
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 16;

type Parsed = Grid2D<char>;
type P = Point<2>;
type ItItem = (usize, Vec<P>);

const COST_MOVE: usize = 1;
const COST_ROTATE: usize = 1000;

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

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct State {
    cost: usize,
    pos: P,
    dir: P,
    /// Tracks whether we rotated in the last operation
    /// to prevent non-90-degree turns.
    just_rotated: bool,
    path: Vec<P>,
}

fn part_1_it<I: Iterator<Item = ItItem>>(it: &mut Peekable<I>) -> usize {
    it.peek().unwrap().0
}

fn part_2_it<F: Iterator<Item = ItItem>>(it: F) -> usize {
    it
        .map(|(_, s)| s)
        .fold(HashSet::default(), |mut acc, pts| {
            acc.extend(pts);
            acc
        })
        .len()
}

fn solve(grid: &Parsed) -> impl Iterator<Item = ItItem> {
    // Basically Dijkstra.
    // Using the experimental `gen` block feature because why not.
    gen {
        let start_pos = grid.position(|c| c == &'S').unwrap();
        let start = State {
            cost: 0,
            pos: start_pos,
            dir: P::E,
            just_rotated: false,
            path: [start_pos].into(),
        };
        let end_pt = grid.position(|c| c == &'E').unwrap();
        let mut queue: BTreeSet<State> = [start].into();
        let mut result = None;
        while let Some(mut s) = queue.pop_first() {
            if result.is_some_and(|r| r < s.cost) {
                break;
            }
            if s.pos == end_pt {
                s.path.push(s.pos);
                dbg!(&queue.len());
                println!("result: {}", s.cost);
                print_path(grid, &s.path);
                result = Some(s.cost);
                yield (s.cost, s.path);
                continue;
            }
            let n_pos = s.pos + s.dir;
            match grid.get(&n_pos).unwrap() {
                &'.' | &'E' => {
                    let mut n_path = s.path.clone();
                    n_path.push(s.pos);
                    queue.insert(State {
                        cost: s.cost + COST_MOVE,
                        pos: n_pos,
                        dir: s.dir,
                        just_rotated: false,
                        path: n_path,
                    });
                }
                &'#' | &'S' => (), // it makes no sense to go over S again
                c => panic!("Unexpected char {c}"),
            }
            if !s.just_rotated {
                queue.insert(State {
                    cost: s.cost + COST_ROTATE,
                    pos: s.pos,
                    dir: s.dir.rotated_left(90),
                    just_rotated: true,
                    path: s.path.clone(),
                });
                queue.insert(State {
                    cost: s.cost + COST_ROTATE,
                    pos: s.pos,
                    dir: s.dir.rotated_right(90),
                    just_rotated: true,
                    path: s.path,
                });
            }
        }
    }
}

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
    fn part_1(grid: &Parsed) -> usize {
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
    // bench!(part_1() == 109496); // needs ~10s, not benchable
    // bench!(part_2() == 0);
}
