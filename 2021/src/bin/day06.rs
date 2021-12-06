#![feature(test)]

use std::collections::VecDeque;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 06;

type Parsed = Vec<usize>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split(',')
            .map(|line| line.parse().unwrap())
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let mut queue = VecDeque::from(vec![0; 9]);
    for n in parsed.iter() {
        queue[*n] += 1;
    }
    for _ in 0..80 {
        queue.rotate_left(1);
        queue[6] += queue[8];
    }
    queue.into_iter().sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "3,4,3,1,2";

    test!(part_1() == 5934);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 300);
    bench!(part_1() == 387413);
    // bench!(part_2() == 0);
}
