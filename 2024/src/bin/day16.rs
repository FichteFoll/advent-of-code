#![feature(test)]

use std::collections::BTreeSet;

use aoc2024::*;
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 16;

type Parsed = Grid2D<char>;
type P = Point<2>;

const COST_MOVE: usize = 1;
const COST_ROTATE: usize = 1000;

main!();

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
}

fn part_1(grid: &Parsed) -> usize {
    // Basically Dijkstra
    let start = State {
        cost: 0,
        pos: grid.position(|c| c == &'S').unwrap(),
        dir: P::E,
        just_rotated: false,
    };
    let end_pt = grid.position(|c| c == &'E').unwrap();
    let mut queue: BTreeSet<_> = [start].into();
    while let Some(s) = queue.pop_first() {
        if s.pos == end_pt {
            return s.cost;
        }
        let n_pos = s.pos + s.dir;
        match grid.get(&n_pos).unwrap() {
            &'.' | &'E' => {
                queue.insert(State {
                    cost: s.cost + COST_MOVE,
                    pos: n_pos,
                    dir: s.dir,
                    just_rotated: false,
                });
            }
            &'#' | &'S' => (),
            c => panic!("Unexpected char {c}"),
        }
        if !s.just_rotated {
            queue.insert(State {
                cost: s.cost + COST_ROTATE,
                pos: s.pos,
                dir: s.dir.rotated_left(90),
                just_rotated: true,
            });
            queue.insert(State {
                cost: s.cost + COST_ROTATE,
                pos: s.pos,
                dir: s.dir.rotated_right(90),
                just_rotated: true,
            });
        }
    }
    panic!("No solution found");
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use grid2d::Size;

    const TEST_INPUT: &str = "\
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

    test!(part_1() == 7036);
    test!(ex2, TEST_INPUT_2, part_1() == 11048);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| p.size, Size(141, 141));
    // bench!(part_1() == 109496); // needs ~10s, not benchable
    // bench!(part_2() == 0);
}
