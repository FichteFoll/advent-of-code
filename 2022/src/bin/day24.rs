#![feature(test)]

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::iter::once;

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;
use parse::parse_input;

const DAY: usize = 24;

// Represents the direction of a blizzard.
type Blizzard = u8;
// A 2-dimensional grid with a variable number of blizzards.
type Grid = Vec<Vec<Option<Blizzard>>>;
// both indexed by x, then y coordinate
type Parsed = (Grid, Grid);

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let lines: Vec<_> = input.lines().collect();
        let size = (lines[0].len() - 2, lines.len() - 2);
        let mut horizontal: Grid = vec![vec![None; size.1]; size.0];
        let mut vertical: Grid = horizontal.clone();
        for (y, line) in lines[1..=size.1].iter().enumerate() {
            for (x, &b) in line.as_bytes()[1..=size.0].into_iter().enumerate() {
                match b {
                    b'.' => (),
                    b'>' | b'<' => horizontal[x][y] = Some(b),
                    b'v' | b'^' => vertical[x][y] = Some(b),
                    _ => panic!("unrecognized char {b}"),
                }
            }
        }
        (horizontal, vertical)
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let start_pt = Point([0, -1]);
    let end_pt = Point([parsed.0.len() as i32 - 1, parsed.0[0].len() as i32]);

    let start = State {
        steps: Reverse(0),
        pt: start_pt,
    };
    // let mut best: HsahMap<Point<2>, usize> = Default::default();
    let mut queue: BinaryHeap<_> = Default::default();
    queue.push(start);
    let mut seen: HashSet<_> = Default::default();

    while let Some(current) = queue.pop() {
        for next_pt in once(current.pt).chain(current.pt.direct_neighbors()) {
            if next_pt == end_pt {
                return current.steps.0 + 1;
            }
            if is_free(parsed, &next_pt, current.steps.0 + 1) {
                let next_state = State {
                    steps: Reverse(current.steps.0 + 1),
                    pt: next_pt,
                };
                if seen.insert(next_state.clone()) {
                    queue.push(next_state);
                }
            }
        }
    }
    unreachable!();
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn is_free((horizontal, vertical): &Parsed, pt: &Point<2>, step: usize) -> bool {
    let (len_x, len_y) = (horizontal.len(), horizontal[0].len());
    if pt == &Point([0, -1]) {
        return true;
    } else if pt.x() < 0 || pt.y() < 0 || pt.x() as usize >= len_x || pt.y() as usize >= len_y {
        // Assumes we checked for the end point earlier
        return false;
    }
    let (x, y) = (pt.x() as usize, pt.y() as usize);
    // West and south directions use "math" to prevent underflows.
    let east_free = horizontal[(x + step) % len_x][y] != Some(b'<');
    let west_free = horizontal[(x + step * (len_x - 1)) % len_x][y] != Some(b'>');
    let north_free = vertical[x][(y + step) % len_y] != Some(b'^');
    let south_free = vertical[x][(y + step * (len_y - 1)) % len_y] != Some(b'v');
    west_free && east_free && south_free && north_free
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct State {
    steps: Reverse<usize>,
    pt: Point<2>,
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        #.######\n\
        #>>.<^<#\n\
        #.<..<<#\n\
        #>v.><>#\n\
        #<^v^^>#\n\
        ######.#\n\
        ";

    test!(part_1() == 18);
    // test!(part_2() == 0);
    // bench_parse!(Vec::len, 0);
    bench!(part_1() == 305);
    // bench!(part_2() == 0);
}
