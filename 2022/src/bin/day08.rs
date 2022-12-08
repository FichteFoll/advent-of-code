#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(test)]

use aoc2022::*;
use aoc2022::collections::*;
use itertools::iproduct;
use itertools::Itertools;

const DAY: usize = 8;

type Point = (usize, usize);
type Parsed = Vec<Vec<u8>>;

main!();

pub fn parse_input(input: &str) -> Parsed {
    input.trim().as_bytes()
        .split(|b| b == &b'\n')
        .map(|line| line.iter().map(|b| b - b'0' as u8).collect())
        .collect()
}

fn part_1(grid: &Parsed) -> usize {
    // count trees visible from the left
    let mut points = Default::default();
    count_visible_horizontal(grid, &mut points, false);
    let transposed_grid = transpose(&grid);
    count_visible_horizontal(&transposed_grid, &mut points, true);
    points.len()
}

fn part_2(grid: &Parsed) -> usize {
    let transposed_grid = transpose(&grid);
    iproduct!(0..grid.len(), 0..transposed_grid.len())
        .map(|(y, x)| {
            let start = grid[y][x];
            [
                count_visible(start, transposed_grid[x][..y].iter().rev()), // to top
                count_visible(start, grid[y][..x].iter().rev()), // to left
                count_visible(start, transposed_grid[x][y + 1..].iter()), // to bottom
                count_visible(start, grid[y][x + 1..].iter()), // to right
            ].into_iter().product()
        })
        .max()
        .unwrap()
}

fn count_visible_horizontal(grid: &Vec<Vec<u8>>, positions: &mut HashSet<Point>, transpose: bool) {
    grid.iter()
        .enumerate()
        .for_each(|(y, line)| {
            let closure = |mut state: (usize, u8, Vec<Point>), (x, &t): (usize, &u8)| {
                if t >= state.1 {
                    state.0 = x;
                    state.1 = t + 1;
                    state.2.push(if transpose { (y, x) } else { (x, y) });
                }
                state
            };
            let from_left = line.iter().enumerate()
                .fold((0, 0, Default::default()), closure);
            let from_right = line.iter().enumerate()
                .skip(from_left.0 + 1)
                .rev()
                .fold((line.len(), 0, Default::default()), closure);
            positions.extend(from_left.2);
            positions.extend(from_right.2);
        });
}

fn count_visible(start: u8, remaining: impl Iterator<Item=&u8>) -> usize {
    let mut heights = remaining.copied().peekable();
    heights.peeking_take_while(|&t| t < start).count()
        + heights.peek().map_or(0, |_| 1) // add the last tree if any
}

fn transpose<T: Clone>(grid: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let height = grid.len();
    let width = grid[0].len();
    let mut new_grid = vec![Vec::with_capacity(height); width];

    for line in grid.iter() {
        for (x, cell) in line.iter().enumerate() {
            new_grid[x].push(cell.clone())
        }
    }
    new_grid
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        30373\n\
        25512\n\
        65332\n\
        33549\n\
        35390\n\
        ";

    test!(part_1() == 21);
    test!(part_2() == 8);
    bench_parse!(Vec::len, 99);
    bench!(part_1() == 1843);
    bench!(part_2() == 180000);
}
