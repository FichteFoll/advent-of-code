#![feature(test)]

use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::iter::once;

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;

const DAY: usize = 24;

// Represents the direction of a blizzard.
type Blizzard = u8;
// A 2-dimensional grid with a variable number of blizzards,
// indexed by x, then y coordinate.
type Grid = Vec<Vec<Option<Blizzard>>>;

#[derive(Clone)]
struct Parsed {
    horizontal: Grid,
    vertical: Grid,
    len_x: usize,
    len_y: usize,
    start_pt: Point<2>,
    end_pt: Point<2>,
}

main!();

fn parse_input(input: &str) -> Parsed {
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
    let (len_x, len_y) = (horizontal.len(), horizontal[0].len());
    Parsed {
        horizontal,
        vertical,
        len_x,
        len_y,
        start_pt: Point([0, -1]),
        end_pt: Point([len_x as i32 - 1, len_y as i32]),
    }
}

fn part_1(parsed: &Parsed) -> usize {
    solve(parsed, 0)
}

fn part_2(parsed: &Parsed) -> usize {
    let mut parsed = parsed.clone();
    let mut steps = 0;
    for _ in 0..3 {
        steps = solve(&parsed, steps);
        std::mem::swap(&mut parsed.start_pt, &mut parsed.end_pt);
    }
    steps
}

fn solve(parsed: &Parsed, start_steps: usize) -> usize {
    let mut queue: BinaryHeap<_> = Default::default();
    let start = State {
        steps: Reverse(start_steps),
        pt: parsed.start_pt,
    };
    queue.push(start);
    let mut seen: HashSet<_> = Default::default();
    while let Some(current) = queue.pop() {
        for next_pt in once(current.pt).chain(current.pt.direct_neighbors()) {
            if next_pt == parsed.end_pt {
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
    unreachable!()
}

fn is_free(p: &Parsed, pt: &Point<2>, step: usize) -> bool {
    if pt == &p.start_pt {
        return true;
    } else if pt.x() < 0 || pt.y() < 0 || pt.x() as usize >= p.len_x || pt.y() as usize >= p.len_y {
        // Assumes we checked for the end point earlier
        return false;
    }
    let (x, y) = (pt.x() as usize, pt.y() as usize);
    // West and south directions use "math" to prevent underflows.
    let east_free = p.horizontal[(x + step) % p.len_x][y] != Some(b'<');
    let west_free = p.horizontal[(x + step * (p.len_x - 1)) % p.len_x][y] != Some(b'>');
    let north_free = p.vertical[x][(y + step) % p.len_y] != Some(b'^');
    let south_free = p.vertical[x][(y + step * (p.len_y - 1)) % p.len_y] != Some(b'v');
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
    test!(part_2() == 54);
    bench_parse!(|p: &Parsed| p.horizontal.len(), 120);
    bench!(part_1() == 305);
    bench!(part_2() == 905);
}
