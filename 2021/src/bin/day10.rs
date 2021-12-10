#![feature(test)]

use core::panic;
use std::collections::{HashMap, VecDeque};

use lazy_static::lazy_static;

use aoc2021::*;
use parse::parse_input;
use thiserror::Error;

const DAY: usize = 10;

type Parsed = Vec<String>;

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
            .split('\n')
            .map(|s| s.into())
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|line| parse_line(line).err().unwrap())
        .filter_map(|err| match &err {
            LineError::Corrupt { c, .. } => POINTS.get(&c),
            _ => None,
        })
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

#[derive(Error, Debug)]
enum LineError {
    #[error("expected {expected}, found {c}")]
    Corrupt { expected: char, c: char },

    #[error("expected {expected:?}, found EOL")]
    Incomplete { expected: VecDeque<char> },

    // #[error("unexpected character {c}")]
    // Unknown { c: char },
}

lazy_static!{
    static ref PAIRS: HashMap<char, char> = [
        ('(', ')'),
        ('[', ']'),
        ('{', '}'),
        ('<', '>'),
    ].into();

    static ref POINTS: HashMap<char, usize> = [
        (')', 3),
        (']', 57),
        ('}', 1197),
        ('>', 25137),
    ].into();
    // static ref OPENING: Vec<char> = PAIRS.keys().cloned().collect();
    // static ref CLOSING: Vec<char> = PAIRS.values().cloned().collect();
}

fn parse_line(line: &str) -> Result<(), LineError> {
    let mut expected: VecDeque<char> = VecDeque::new();
    for c in line.chars() {
        if let Some(&new_closing) = PAIRS.get(&c) {
            expected.push_front(new_closing);
        } else if let Some(next_expected) = expected.pop_front() {
            if next_expected != c {
                return Err(LineError::Corrupt { expected: next_expected, c });
            }
        } else {
            panic!("shouldn't reach this");
        }
    }
    if !expected.is_empty() {
        return Err(LineError::Incomplete { expected });
    }
    panic!("expected all lines to be invalid");
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
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 102);
    bench!(part_1() == 389589);
    // bench!(part_2() == 0);
}
