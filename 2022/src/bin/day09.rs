#![feature(test)]

use aoc2022::*;
use aoc2022::collections::*;
use aoc2022::coord::Point;
use parse::parse_input;

const DAY: usize = 9;

type Parsed = Vec<(Point<2>, i32)>;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .lines()
            .map(|line| {
                let split = line.split_once(' ').unwrap();
                let direction = match split.0 {
                    "U" => Point::<2>::N,
                    "L" => Point::<2>::W,
                    "R" => Point::<2>::E,
                    "D" => Point::<2>::S,
                    _ => panic!("unexpected direction {}", split.0),
                };
                let count = split.1.parse().unwrap();
                (direction, count)
            })
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    move_rope::<2>(parsed)
}

fn part_2(parsed: &Parsed) -> usize {
    move_rope::<10>(parsed)
}

fn move_rope<const N: usize>(parsed: &Parsed) -> usize {
    let mut visited = HashSet::default();
    let mut knots: [Point<2>; N] = [Default::default(); N];
    visited.insert(knots[N - 1]);
    for (dir, count) in parsed {
        for _ in 0..*count {
            knots[0] += dir;
            for i in 1..N {
                let diff = &knots[i - 1] - &knots[i];
                if diff.0.iter().any(|n| n.abs() > 1) {
                    knots[i] += diff.signum();
                    if i + 1 == N {
                        visited.insert(knots[i]);
                    }
                } else {
                    break;
                }
            }
        }
    }
    visited.len()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        R 4\n\
        U 4\n\
        L 3\n\
        D 1\n\
        R 4\n\
        D 1\n\
        L 5\n\
        R 2\n\
        ";

    const TEST_INPUT_2: &str = "\
        R 5\n\
        U 8\n\
        L 8\n\
        D 3\n\
        R 17\n\
        D 10\n\
        L 25\n\
        U 20\n\
        ";

    test!(part_1() == 13);
    test!(part_2() == 1);
    test!(extended, TEST_INPUT_2, part_2() == 36);
    bench_parse!(Vec::len, 2000);
    bench!(part_1() == 5695);
    bench!(part_2() == 2434);
}
