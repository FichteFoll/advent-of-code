#![feature(test)]

use std::collections::VecDeque;

use aoc2024::*;
use collections::HashSet;
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 10;

type Parsed = Grid2D<u8>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| line.bytes().map(|c| c - b'0'))
        .collect()
}

fn part_1(grid: &Parsed) -> usize {
    grid
        .iter_enumerate()
        .filter_map(|(pt, &c)| (c == 0u8).then_some((pt, c)))
        .map(|head| count_trails(grid, head))
        .sum()
}

fn count_trails(grid: &Parsed, head: (Point<2>, u8)) -> usize {
    let mut queue: VecDeque<_> = [head].into();
    let mut seen: HashSet<_> = Default::default();
    let mut result = 0;

    while let Some(item) = queue.pop_front() {
        if !seen.insert(item) {
            continue;
        }
        let (pt, h) = item;
        if h == 9 {
            result += 1;
            continue;
        }

        let next = pt.direct_neighbors().into_iter().filter_map(|pt2| {
            let h2 = *grid.get(&pt2)?;
            (h + 1 == h2).then_some((pt2, h2))
        });
        queue.extend(next);
    }
    result
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
        89010123\n\
        78121874\n\
        87430965\n\
        96549874\n\
        45678903\n\
        32019012\n\
        01329801\n\
        10456732\n\
        ";

    test!(part_1() == 36);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| p.size, Size(47, 47));
    bench!(part_1() == 607);
    // bench!(part_2() == 0);

    #[test]
    fn test_p1_simple() {
        let input = "\
            0123\n\
            1234\n\
            8765\n\
            9876\n\
            ";
        let parsed = parse_input(input);
        assert_eq!(part_1(&parsed), 1);
    }

    #[test]
    fn test_count_trails_2_5() {
        let head = (Point([2, 5]), 0);
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(count_trails(&parsed, head), 1);
    }
}
