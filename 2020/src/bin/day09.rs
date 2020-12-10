#![feature(test, core_intrinsics)]

use aoc2020::*;
use itertools::{Itertools, MinMaxResult};

const DAY: usize = 9;
type Input = Vec<usize>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(input: &Input, preamble: usize) -> usize {
    for i in preamble..input.len() {
        let n = input[i];
        if !input[i - preamble..i].iter().tuple_combinations().any(|(a, b)| a + b == n) {
            return n;
        }
    }
    panic!("all numbers are valid");
}

fn part_2(input: &Input, n: usize) -> usize {
    let (mut start, mut end) = (0, 2);
    let mut acc = input[0] + input[1];
    loop {
        if acc == n && start + 1 < end {
            return match input[start..end].iter().minmax() {
                MinMaxResult::MinMax(a, b) => a + b,
                _ => unreachable!(),
            };
        } else if acc > n {
            acc -= input[start];
            start += 1;
        } else {
            acc += input[end];
            end += 1;
        }
    }
}

fn main() {
    let input_str = read_input!();
    let input = parse_input(&input_str);
    let n = part_1(&input, 25);
    println!("Part 1: {}", n);
    println!("Part 2: {}", part_2(&input, n));
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT_STR: &str = "\
        35\n\
        20\n\
        15\n\
        25\n\
        47\n\
        40\n\
        62\n\
        55\n\
        65\n\
        95\n\
        102\n\
        117\n\
        150\n\
        182\n\
        127\n\
        219\n\
        299\n\
        277\n\
        309\n\
        576\n\
        ";

    test!(part_1(5) == 127);
    test!(part_2(127) == 62);
    bench_parse!(len, 1000);
    bench!(part_1(25) == 1930745883);
    bench!(part_2(1930745883) == 268878261);

    #[test]
    fn test_part_2_window_large_enough() {
        assert_eq!(part_2(&vec![4, 2, 1, 1], 2), 2);
    }
}
