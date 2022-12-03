#![feature(test)]

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 3;

type Parsed = Vec<(u64, u64)>;

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
            .lines()
            .map(|line| {
                let bytes = line.as_bytes();
                let mut iter = bytes.iter();
                let a = into_bitset(&mut iter, bytes.len() / 2);
                let b = into_bitset(&mut iter, bytes.len() / 2);
                (a, b)
            })
            .collect()
    }

    fn into_bitset(iter: &mut std::slice::Iter<u8>, count: usize) -> u64 {
        (0..count)
            .map(|_| match iter.next() {
                Some(&c) if c >= b'a' => c - b'a',
                Some(&c) => c - b'A' + 26,
                _ => panic!("unexpected end of line")
            })
            .fold(0, |a, n| a | 1 << n)
    }
}

fn part_1(parsed: &Parsed) -> u32 {
    parsed.iter()
        .map(|(a, b)| (a & b).trailing_zeros() + 1)
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        vJrwpWtwJgWrhcsFMMfFFhFp\n\
        jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
        PmmdzqPrVvPwwTWBwg\n\
        wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
        ttgJtRGJQctTZtZT\n\
        CrZsJsPPZsGzwwsLwLmpwMDw\n\
        ";

    test!(part_1() == 157);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 300);
    bench!(part_1() == 8202);
    // bench!(part_2() == 0);
}
