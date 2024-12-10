#![feature(test)]

use std::{collections::VecDeque, hash::Hash};

use aoc2024::*;
use collections::HashSet;
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 10;

type Parsed = Grid2D<u8>;
type Position = (Point<2>, u8);

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| line.bytes().map(|c| c - b'0'))
        .collect()
}

fn part_1(grid: &Parsed) -> usize {
    grid.iter_enumerate()
        .filter_map(|(pt, &c)| (c == 0u8).then_some((pt, c)))
        .map(|head| {
            score(
                grid,
                head,
                |x| *x,
                |_, item| item,
            )
        })
        .sum()
}

fn part_2(grid: &Parsed) -> usize {
    grid.iter_enumerate()
        .filter_map(|(pt, &c)| (c == 0u8).then_some((pt, c)))
        .map(|head| {
            score(
                grid,
                vec![head],
                |path| *path.last().unwrap(),
                |path, item| {
                    let mut n_path = path.clone();
                    n_path.push(item);
                    n_path
                },
            )
        })
        .sum()
}

fn score<S, Fpfs, Fds>(
    grid: &Parsed,
    head_state: S,
    pos_from_state: Fpfs,
    derive_state: Fds,
) -> usize
where
    S: Clone + Eq + Hash,
    Fpfs: Fn(&S) -> Position,
    Fds: Fn(&S, Position) -> S,
{
    let mut queue: VecDeque<_> = [head_state].into();
    let mut seen: HashSet<_> = Default::default();
    let mut result = 0;

    while let Some(state) = queue.pop_front() {
        if !seen.insert(state.clone()) {
            continue;
        }
        let (pt, h) = pos_from_state(&state);
        if h == 9 {
            result += 1;
            continue;
        }

        let next = pt
            .direct_neighbors()
            .into_iter()
            .filter_map(|pt2| {
                let h2 = *grid.get(&pt2)?;
                (h + 1 == h2).then_some((pt2, h2))
            })
            .map(|item| derive_state(&state, item));
        queue.extend(next);
    }
    result
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
    test!(part_2() == 81);
    bench_parse!(|p: &Parsed| p.size, Size(47, 47));
    bench!(part_1() == 607);
    bench!(part_2() == 1384);

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
}
