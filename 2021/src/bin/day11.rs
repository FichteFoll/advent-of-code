#![feature(test)]

use std::collections::VecDeque;

use aoc2021::*;
use aoc2021::grid2d::Grid2D;
use aoc2021::coord::Point;

const DAY: usize = 11;

type Parsed = Grid2D<u8>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split('\n')
        .map(|line| line.bytes().map(|b| b - b'0'))
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let mut grid = parsed.clone();
    (0..100).map(|_| explode(&mut grid)).sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let mut grid = parsed.clone();
    let max = grid.size.0 * grid.size.1;
    for i in 1.. {
        if explode(&mut grid) == max {
            return i;
        }
    }
    panic!("didn't terminate");
}

fn explode(board: &mut Parsed) -> usize {
    let size = board.size;
    let mut count = 0;
    let mut queue: VecDeque<Point<2>> = VecDeque::with_capacity(100);
    // visit every point at least once and increase
    for (pt, n) in board.iter_enumerate_mut() {
        *n = (*n + 1) % 10;
        if *n == 0 {
            queue.extend(size.contained(pt.neighbors()));
            count += 1;
        }
    }
    // visit neighbors and increase them again,
    // but not if they flashed already!
    while let Some(pt) = queue.pop_front() {
        let n = board.get_mut(&pt).unwrap();
        if *n == 0 {
            continue;
        }
        *n = (*n + 1) % 10;
        if *n == 0 {
            queue.extend(size.contained(pt.neighbors()));
            count += 1;
        }
    }
    count
}


#[cfg(test)]
mod tests {
    use super::*;
    use aoc2021::grid2d::Size;
    extern crate test;

    #[test]
    fn test_explode_cascading() {
        let input = "\
            11111\n\
            19991\n\
            19191\n\
            19991\n\
            11111\n\
            ";
        let mut parsed = parse_input(input);
        assert_eq!(explode(&mut parsed), 9)
    }

    #[test]
    fn test_explode_10() {
        let mut parsed = parse_input(TEST_INPUT);
        let result = (0..10).fold(0, |acc, _| acc + explode(&mut parsed));
        assert_eq!(result, 204)
    }

    const TEST_INPUT: &str = "\
        5483143223\n\
        2745854711\n\
        5264556173\n\
        6141336146\n\
        6357385478\n\
        4167524645\n\
        2176841721\n\
        6882881134\n\
        4846848554\n\
        5283751526\n\
        ";

    test!(part_1() == 1656);
    test!(part_2() == 195);
    bench_parse!(|x: &Grid2D<u8>| x.size, Size(10, 10));
    bench!(part_1() == 1729);
    bench!(part_2() == 237);
}
