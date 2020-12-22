#![feature(test, str_split_once)]

use std::{collections::HashMap, ops::RangeInclusive};

use aoc2020::*;

const DAY: usize = 16;
type Ticket = Vec<usize>;
type Rule = Vec<RangeInclusive<usize>>;
type Input = (HashMap<String, Rule>, Ticket, Vec<Ticket>);

fn parse_input(input_str: &str) -> Input {
    let (rule_block, rest) = input_str
        .trim()
        .split_once("\n\n")
        .unwrap();
    let rules = rule_block.split("\n")
        .map(|line| {
            let (field, range_str) = line.split_once(": ").unwrap();
            let ranges: Vec<_> = range_str.split(" or ")
                .map(|range| {
                    let (a, b) = range.split_once('-').unwrap();
                    a.parse::<usize>().unwrap()..=b.parse::<usize>().unwrap()
                })
                .collect();
            (field.to_string(), ranges)
        })
        .collect();
    let (my_block, other_block) = rest.split_once("\n\n").unwrap();
    fn line_to_ticket(line: &str) -> Vec<usize> {
        line.split(',').map(|n| n.parse().unwrap()).collect()
    }
    let my = line_to_ticket(my_block.split("\n").nth(1).unwrap());
    let other = other_block.split("\n").skip(1).map(|line| line_to_ticket(line)).collect();

    (rules, my, other)
}

fn part_1(input: &Input) -> usize {
    input.2.iter()
        .flatten()
        .filter(|num| !input.0.values().flatten().any(|range| range.contains(num)))
        .sum()
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
        class: 1-3 or 5-7\n\
        row: 6-11 or 33-44\n\
        seat: 13-40 or 45-50\n\
        \n\
        your ticket:\n\
        7,1,14\n\
        \n\
        nearby tickets:\n\
        7,3,47\n\
        40,4,50\n\
        55,2,20\n\
        38,6,12\n\
        ";

    test!(part_1() == 71);
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    bench!(part_1() == 27850);
    // bench!(part_2() == 0);
}
