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
    let mut visited = HashSet::default();
    let mut head: Point<2> = [0, 0].into();
    let mut tail = head;
    visited.insert(tail);
    for (dir, count) in parsed {
        for _ in 0..*count {
            head += dir;
            let diff = head - tail;
            if diff.x().abs() > 1 || diff.y().abs() > 1 {
                tail += diff.signum();
                visited.insert(tail);
            }
        }
    }
    visited.len()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
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

    test!(part_1() == 13);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 2000);
    bench!(part_1() == 5695);
    // bench!(part_2() == 0);
}
