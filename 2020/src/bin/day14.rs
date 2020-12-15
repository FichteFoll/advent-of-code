#![feature(test, bool_to_option)]

use std::collections::HashMap;

use aoc2020::*;

use text_io::scan;

const DAY: usize = 14;
type Input = Vec<Statement>;

#[derive(Debug)]
enum Statement {
    Mask(Vec<Option<u8>>),
    Mem(usize, u64),
}
use Statement::*;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| match &line[..3] {
            "mas" => Mask(
                line[7..].bytes().rev()
                    .map(|x| match x {
                        b'1' => Some(1),
                        b'0' => Some(0),
                        b'X' => None,
                        _ => panic!("bad mask"),
                    })
                    .collect()
            ),
            "mem" => {
                let (i, n): (usize, u64);
                scan!(line.bytes() => "mem[{}] = {}", i, n);
                Mem(i, n)
            },
            _ => panic!("unexpected input"),
        })
        .collect()
}

fn u64_with_bits<I: IntoIterator<Item=usize>>(ones: I) -> u64 {
    ones.into_iter().fold(0, |n, i| n | 1 << i)
}

fn usize_with_bits<I: IntoIterator<Item=usize>>(ones: I) -> usize {
    ones.into_iter().fold(0, |n, i| n | 1 << i)
}

fn part_1(input: &Input) -> u64 {
    let (mut or_mask, mut and_mask) = (0u64, 0u64);
    let mut memory = HashMap::new();
    for stmt in input {
        match stmt {
            Mask(mask) => {
                let ones: Vec<_> = mask.iter().enumerate()
                    .filter_map(|(i, x)| (x == &Some(1)).then_some(i))
                    .collect();
                let zeros: Vec<_> = mask.iter().enumerate()
                    .filter_map(|(i, x)| (x == &Some(0)).then_some(i))
                    .collect();
                or_mask = u64_with_bits(ones);
                and_mask = u64_with_bits(zeros) ^ (2u64.pow(mask.len() as u32) - 1);
            },
            Mem(i, n) => {
                memory.insert(i, (n | or_mask) & and_mask);
            },
        }
    }
    memory.values().sum()
}

fn part_2(input: &Input) -> u64 {
    let mut memory = HashMap::new();
    let (mut or_mask, mut and_mask) = (0usize, 0usize);
    let mut extra_masks = vec![];
    for stmt in input {
        match stmt {
            Mask(mask) => {
                let ones: Vec<_> = mask.iter().enumerate()
                    .filter_map(|(i, x)| (x == &Some(1)).then_some(i))
                    .collect();
                let wildcards: Vec<_> = mask.iter().enumerate()
                    .filter_map(|(i, x)| x.is_none().then_some(i))
                    .collect();
                or_mask = usize_with_bits(ones);
                extra_masks = powerset(&wildcards).into_iter()
                    .map(|indices| usize_with_bits(indices))
                    .collect();
                and_mask = usize_with_bits(wildcards) ^ (2usize.pow(mask.len() as u32) - 1);
            },
            Mem(i, n) => {
                for extra_mask in extra_masks.iter() {
                    memory.insert((i & and_mask) | or_mask | extra_mask, *n);
                }
            },
        }
    }
    memory.values().sum()
}

fn main() {
    let input_str = read_input!();
    let input = parse_input(&input_str);
    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT_STR: &str = "\
        mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
        mem[8] = 11\n\
        mem[7] = 101\n\
        mem[8] = 0\n\
        ";

    const TEST_INPUT_STR_2: &str = "\
        mask = 000000000000000000000000000000X1001X\n\
        mem[42] = 100\n\
        mask = 00000000000000000000000000000000X0XX\n\
        mem[26] = 1\n\
        ";

    test!(part_1() == 165);
    test!(TEST_INPUT_STR_2, part_2() == 208);
    bench_parse!(len, 577);
    bench!(part_1() == 18630548206046);
    bench!(part_2() == 4254673508445);
}
