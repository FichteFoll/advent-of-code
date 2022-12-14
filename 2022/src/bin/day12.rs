#![feature(test)]

use std::cmp::Reverse;
use std::collections::BinaryHeap;

use aoc2022::*;
use aoc2022::coord::Point;
use parse::parse_input;

const DAY: usize = 12;

type Grid = Vec<Vec<u8>>;
type Parsed = (Grid, Point<2>, Point<2>);

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let mut start = Default::default();
        let mut end = start;
        let mut grid: Grid = input
            .lines()
            .map(|line| line.bytes().collect())
            .collect();
        for (y, line) in grid.iter_mut().enumerate() {
            for (x, b) in line.iter_mut().enumerate() {
                if *b == b'S' {
                    *b = b'a';
                    start = (x, y).into();
                } else if *b == b'E' {
                    *b = b'z';
                    end = (x, y).into();
                }
            }
        }
        (grid, start, end)
    }
}

fn part_1((grid, start, end): &Parsed) -> u32 {
    let mut queue = BinaryHeap::new();
    let mut visited = vec![vec![u32::MAX; grid[0].len()]; grid.len()];
    queue.push((Reverse(0), *start));

    while let Some((Reverse(count), point)) = queue.pop() {
        if &point == end {
            // since the queue is ordered, this must be (one of) the shortest path(s)
            return count;
        }
        let best_count = &mut visited[point.y() as usize][point.x() as usize];
        if *best_count <= count {
            continue;
        }
        *best_count = count;
        let height = grid[point.y() as usize][point.x() as usize];
        queue.extend(
            point.direct_neighbors()
                .into_iter()
                .filter(|pt| {
                    let next_height = grid.get(pt.y() as usize).and_then(|line| line.get(pt.x() as usize));
                    next_height.map(|&x| x <= height + 1).unwrap_or(false)
                })
                .map(|pt| (Reverse(count + 1), pt))
        );
    }
    unreachable!()
}

fn part_2((grid, _, end): &Parsed) -> u32 {
    let mut queue = BinaryHeap::new();
    let mut visited = vec![vec![u32::MAX; grid[0].len()]; grid.len()];
    queue.push((Reverse(0), *end));
    while let Some((Reverse(count), point)) = queue.pop() {
        let best_count = &mut visited[point.y() as usize][point.x() as usize];
        if *best_count <= count {
            continue;
        }
        *best_count = count;
        let height = grid[point.y() as usize][point.x() as usize];
        if height == b'a' {
            return count;
        }
        queue.extend(
            point.direct_neighbors()
                .into_iter()
                .filter(|pt| {
                    let next_height = grid.get(pt.y() as usize).and_then(|line| line.get(pt.x() as usize));
                    next_height.map(|&x| height <= x + 1).unwrap_or(false)
                })
                .map(|pt| (Reverse(count + 1), pt))
        );
    }
    unreachable!()
}

#[allow(unused)]
fn print_visited(visited: &[Vec<u32>], heigth: usize, width: usize) {
    for line in visited {
        for count in line {
            print!("|{count:03}");
        }
        println!("|");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Sabqponm\n\
        abcryxxl\n\
        accszExk\n\
        acctuvwj\n\
        abdefghi\n\
        ";

    test!(part_1() == 31);
    test!(part_2() == 29);
    bench_parse!(|p: &Parsed| (p.0.len(), p.1, p.2), (41, (0, 20).into(), (72, 20).into()));
    bench!(part_1() == 420);
    bench!(part_2() == 414);
}
