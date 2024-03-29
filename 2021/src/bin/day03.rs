#![feature(test)]

use aoc2021::*;

const DAY: usize = 3;
struct Parsed {
    nums: Vec<usize>,
    digits: usize,
}

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    let mut lines = input
        .trim()
        .split('\n')
        .peekable();
    let digits = lines.peek().expect("file was empty").len();
    Parsed {
        nums: lines
            .map(|line| usize::from_str_radix(line, 2).unwrap())
            .collect(),
        digits,
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let mut ones = vec![0usize; parsed.digits];
    for num in parsed.nums.iter() {
        for (i, one) in ones.iter_mut().enumerate() {
            *one += (num >> (parsed.digits - 1 - i)) & 1;
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
fn bit_at(n: usize, pos: usize) -> usize {
    n >> pos & 1
}

fn most_common_digit_at(nums: &[usize], pos: usize) -> usize {
    let one = nums.iter().filter(|&&n| bit_at(n, pos) > 0).count();
    (one * 2 >= nums.len()) as usize
}

fn rating(parsed: &Parsed, keep_most_common: bool) -> usize {
    // I need to deref the numbers for comparisons anyway
    let mut nums = parsed.nums.clone();
    for pos in (0..parsed.digits).rev() {
        let most_common_digit = most_common_digit_at(&nums, pos);
        // Vec::retain is significantly slower
        nums = nums.into_iter()
            .filter(|&n| (bit_at(n, pos) == most_common_digit) == keep_most_common)
            .collect();
        if nums.len() == 1 {
            return nums[0];
        }
    }
    panic!("more than one number left");
}

fn part_2(parsed: &Parsed) -> usize {
    let oxygen = rating(parsed, true);
    let co2 = rating(parsed, false);
    oxygen * co2
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
    bench_parse!(|x: &Parsed| (x.nums.len(), x.digits), (1000, 12));
    bench!(part_1() == 2583164);
    bench!(part_2() == 2784375);
}
