#![feature(test)]

use aoc2024::*;
use itertools::Itertools;

const DAY: usize = 1;

type Parsed = Vec<(i32, i32)>;

main!();

pub fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let pair_s = line.split_once("   ").unwrap();
            (pair_s.0.parse().unwrap(), pair_s.1.parse().unwrap())
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> i32 {
    let nums1 = parsed.iter().map(|(x, _)| x).sorted_unstable();
    let nums2 = parsed.iter().map(|(_, x)| x).sorted_unstable();
    nums1.zip(nums2).map(|(n1, n2)| (n1 - n2).abs()).sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let nums1 = parsed.iter().map(|(x, _)| x).sorted_unstable();
    let nums2_map = parsed.iter().map(|(_, x)| x).counts_by(|x| x);
    nums1.map(|n| *n as usize * nums2_map.get(n).unwrap_or(&0)).sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        3   4\n\
        4   3\n\
        2   5\n\
        1   3\n\
        3   9\n\
        3   3\n\
        ";

    test!(part_1() == 11);
    test!(part_2() == 31);
    bench_parse!(Vec::len, 1000);
    bench!(part_1() == 2264607);
    bench!(part_2() == 19457120);
}
