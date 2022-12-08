#![feature(test)]

use aoc2022::*;

const DAY: usize = 8;

type Parsed = Vec<Vec<u8>>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

pub fn parse_input(input: &str) -> Parsed {
    input.trim().as_bytes()
        .split(|b| b == &b'\n')
        .map(|line| line.iter().map(|b| b - b'0' as u8).collect())
        .collect()
}

fn part_1(grid: &Parsed) -> usize {
    // count trees visible from the left
    let horizontal: usize = count_visible_horizontal(grid);
    let transposed_grid = transpose(&grid);
    dbg!(&grid, &transposed_grid);
    // TODO highest are counted twice
    let vertical: usize = count_visible_horizontal(&transposed_grid);
    dbg!(horizontal, vertical);
    horizontal + vertical
}

fn count_visible_horizontal(grid: &Vec<Vec<u8>>) -> usize {
    grid.iter()
        .map(|line| {
            let closure = |mut state: (usize, usize, u8), (i, &t): (usize, &u8)| {
                if t >= state.2 {
                    state.0 += 1;
                    state.1 = i;
                    state.2 = t + 1;
                }
                state
            };
            let (from_left, highest_index, _) = line.iter().enumerate().fold((0, 0, 0), closure);
            let (from_right, ..) = line.iter().enumerate().skip(highest_index + 1).rev().fold((0, 0, 0), closure);
            dbg!(line, from_left, from_right);
            from_left + from_right
        })
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
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
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 99);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
