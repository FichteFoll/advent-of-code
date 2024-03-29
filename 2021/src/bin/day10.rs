#![feature(test)]

use core::panic;
use std::collections::{HashMap, VecDeque};

use lazy_static::lazy_static;

use aoc2021::*;
use LineError::*;

const DAY: usize = 10;

type Parsed = Vec<String>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split('\n')
        .map(|s| s.into())
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let points: HashMap<_, usize> = [
        (')', 3),
        (']', 57),
        ('}', 1197),
        ('>', 25137),
    ].into();

    parsed.iter()
        .map(|line| parse_line(line))
        .filter_map(|err| match err {
            Corrupt { c } => points.get(&c),
            Incomplete { .. } => None,
        })
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let points = [')', ']', '}', '>'];

    let mut scores: Vec<_> = parsed.iter()
        .map(|line| parse_line(line))
        .filter_map(|err| match err {
            Corrupt { .. } => None,
            Incomplete { stack } => Some(stack),
        })
        .map(|stack|
            stack.into_iter()
                .map(|c| points.iter().position(|&x| x == c).unwrap() + 1)
                .fold(0, |acc, x| acc * 5 + x)
        )
        .collect();
    let i = scores.len() / 2;
    *scores.select_nth_unstable(i).1
}

enum LineError {
    Corrupt { c: char },
    Incomplete { stack: VecDeque<char> },
}

lazy_static!{
    static ref PAIRS: HashMap<char, char> = [
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('<', '>'),
    ].into();
}

fn parse_line(line: &str) -> LineError {
    let mut stack: VecDeque<char> = VecDeque::with_capacity(110);
    for c in line.chars() {
        if let Some(&new_closing) = PAIRS.get(&c) {
            stack.push_front(new_closing);
        } else if let Some(wanted) = stack.pop_front() {
            if wanted != c {
                return Corrupt { c };
            }
        } else {
            panic!("unexpected char {c:?}");
        }
    }
    if !stack.is_empty() {
        return Incomplete { stack };
    }
    panic!("unexpectedly valid lines");
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        [({(<(())[]>[[{[]{<()<>>\n\
        [(()[<>])]({[<{<<[]>>(\n\
        {([(<{}[<>[]}>{[]{[(<()>\n\
        (((({<>}<{<{<>}{[]{[]{}\n\
        [[<[([]))<([[{}[[()]]]\n\
        [{[{({}]{}}([{[{{{}}([]\n\
        {<[[]]>}<{[{[{[]{()[[[]\n\
        [<(<(<(<{}))><([]([]()\n\
        <{([([[(<>()){}]>(<<{{\n\
        <{([{{}}[<[[[<>{}]]]>[]]\n\
        ";

    test!(part_1() == 26397);
    test!(part_2() == 288957);
    bench_parse!(Vec::len, 102);
    bench!(part_1() == 389589);
    bench!(part_2() == 1190420163);
}
