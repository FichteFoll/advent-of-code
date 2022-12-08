#![feature(test)]
#![feature(binary_heap_into_iter_sorted)]

use std::collections::BinaryHeap;

use aoc2022::*;

const DAY: usize = 1;

type Parsed = Vec<Vec<u64>>;

main!();

pub fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split("\n\n")
        .map(|block| block.lines().map(|line| line.parse().unwrap()).collect())
        .collect()
}

fn part_1(parsed: &Parsed) -> u64 {
    parsed.iter()
        .map(|block| block.iter().sum())
        .max()
        .unwrap()
}

fn part_2(parsed: &Parsed) -> u64 {
    let sums: BinaryHeap<u64> = parsed.iter()
        .map(|block| block.iter().sum())
        .collect();

    sums.into_iter_sorted().take(3).sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        1000\n\
        2000\n\
        3000\n\
        \n\
        4000\n\
        \n\
        5000\n\
        6000\n\
        \n\
        7000\n\
        8000\n\
        9000\n\
        \n\
        10000\n\
        ";

    test!(part_1() == 24_000);
    test!(part_2() == 45_000);
    bench_parse!(Vec::len, 266);
    bench!(part_1() == 69281);
    bench!(part_2() == 201524);
}
