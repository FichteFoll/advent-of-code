#![feature(test)]

use aoc2021::*;

const DAY: usize = 03;
struct Parsed {
    nums: Vec<usize>,
    digits: usize,
}

fn parse_input(input: &str) -> Parsed {
    let mut lines = input
        .trim()
        .split("\n")
        .peekable();
    let digits = lines.peek().expect("file was empty").len();
    Parsed {
        nums: lines
            .map(|line| usize::from_str_radix(line, 2).unwrap())
            .collect(),
        digits: digits,
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let mut ones = vec![0usize; parsed.digits];
    for num in parsed.nums.iter() {
        for i in 0..ones.len() {
            ones[i] += (num >> (parsed.digits - 1 - i)) & 1;
        }
    }
    let most_common: Vec<_> = ones.iter()
        .map(|&one| (one > parsed.nums.len() / 2) as usize)
        .collect();
    let gamma = most_common.iter().fold(0, |x, &one| (x << 1) + one);
    let epsilon = gamma ^ (2usize.pow(parsed.digits as u32) - 1);
    gamma * epsilon
}

#[inline]
fn digit_at(n: usize, pos: usize) -> usize {
    n >> pos & 1
}

fn most_common_digit_at(nums: &Vec<usize>, pos: usize) -> usize {
    let one = nums.iter().filter(|&&n| digit_at(n, pos) > 0).count();
    (one * 2 >= nums.len()) as usize
}

fn rating(parsed: &Parsed, keep_most_common: bool) -> usize {
    // I need to deref the numbers for comparisons anyway
    let mut nums = parsed.nums.clone();
    let mut pos = parsed.digits - 1;
    loop {
        let most_common_digit = most_common_digit_at(&nums, pos);
        nums = nums.into_iter()
            .filter(|&n| (digit_at(n, pos) == most_common_digit) == keep_most_common)
            .collect();
        if nums.len() == 1 {
            return nums[0];
        }
        pos -= 1;
    }
}

fn part_2(parsed: &Parsed) -> usize {
    let oxygen = rating(parsed, true);
    let co2 = rating(parsed, false);
    oxygen * co2
}

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        00100\n\
        11110\n\
        10110\n\
        10111\n\
        10101\n\
        01111\n\
        00111\n\
        11100\n\
        10000\n\
        11001\n\
        00010\n\
        01010\n\
        ";

    test!(part_1() == 198);
    test!(part_2() == 230);
    bench_parse!(|x: &Parsed| x.nums.len(), 1000);
    bench!(part_1() == 2583164);
    bench!(part_2() == 2784375);
}
