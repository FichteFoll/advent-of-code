#![feature(test)]

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

fn part_1(input: &Input) -> u64 {
    let (mut or_mask, mut and_mask) = (0u64, 0u64);
    let mut memory = HashMap::new();
    for stmt in input {
        match stmt {
            Mask(mask) => {
                let (ones, zeros): (Vec<_>, Vec<_>) = mask.iter().enumerate()
                    .filter(|(_, x)| x.is_some())
                    .partition(|(_, bit)| *bit == &Some(1));
                or_mask = ones.into_iter().fold(0u64, |n, (i, _)| n | 1 << i);
                and_mask = zeros.into_iter().fold(0u64, |n, (i, _)| n | 1 << i) ^ (2u64.pow(mask.len() as u32) - 1);
            },
            Mem(i, n) => {
                memory.insert(i, (n | or_mask) & and_mask);
            },
        }
    }
    memory.values().sum()
}

fn part_2(_input: &Input) -> usize {
    0
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

    test!(part_1() == 165);
    // test!(part_2() == 0);
    bench_parse!(len, 577);
    bench!(part_1() == 18630548206046);
    // bench!(part_2() == 0);
}
