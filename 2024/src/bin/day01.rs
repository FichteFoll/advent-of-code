#![feature(test)]

use aoc2024::*;
use itertools::Itertools;

const DAY: usize = 1;

type Parsed = (Vec<i32>, Vec<i32>);

main!();

pub fn parse_input(input: &str) -> Parsed {
    input.lines().fold((vec![], vec![]), |mut vs, line| {
        let pair_s = line.split_once("   ").unwrap();
        vs.0.push(pair_s.0.parse().unwrap());
        vs.1.push(pair_s.1.parse().unwrap());
        vs
    })
}

fn part_1(parsed: &Parsed) -> i32 {
    let nums1 = parsed.0.iter().sorted_unstable();
    let nums2 = parsed.1.iter().sorted_unstable();
    nums1.zip(nums2).map(|(n1, n2)| (n1 - n2).abs()).sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let nums1 = parsed.0.iter().sorted_unstable();
    let nums2_map = parsed.1.iter().counts_by(|x| x);
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
    bench_parse!(|vs: &Parsed| (vs.0.len(), vs.1.len()), (1000, 1000));
    bench!(part_1() == 2264607);
    bench!(part_2() == 19457120);
}
