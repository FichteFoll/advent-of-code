#![feature(test)]

use std::str::FromStr;
use std::num::ParseIntError;

use aoc2021::*;

use thiserror::Error;

const DAY: usize = 2;
type Parsed = Vec<Command>;

#[derive(Debug)]
enum Command {
    Forward(usize),
    Up(usize),
    Down(usize),
}

#[derive(Debug, Error)]
enum ParseError {
    #[error("no space found")]
    NoSpace,
    #[error("bad command: {0}")]
    BadCommand(String),
    #[error("unable to parse integer: {0}")]
    BadInt(#[from] ParseIntError),
}

impl FromStr for Command {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Command::*;
        let (word, step_str) = s.split_once(" ").ok_or(ParseError::NoSpace)?;
        let step = step_str.parse()?;
        match word {
            "forward" => Ok(Forward(step)),
            "up" => Ok(Up(step)),
            "down" => Ok(Down(step)),
            _ => Err(ParseError::BadCommand(word.to_owned())),
        }
    }
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split('\n')
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    use Command::*;
    let (distance, depth) = parsed.iter()
        .fold((0usize, 0usize), |(dist, dep), cmd| {
            match cmd {
                Forward(n) => (dist + n, dep),
                Up(n) => (dist, dep - n),
                Down(n) => (dist, dep + n),
            }
        });
    distance * depth
}

fn part_2(parsed: &Parsed) -> usize {
    use Command::*;
    let (distance, depth, _) = parsed.iter()
        .fold((0usize, 0usize, 0usize), |(dist, dep, aim), cmd| {
            match cmd {
                Forward(n) => (dist + n, dep + aim * n, aim),
                Up(n) => (dist, dep, aim - n),
                Down(n) => (dist, dep, aim + n),
            }
        });
    distance * depth
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
        forward 5\n\
        down 5\n\
        forward 8\n\
        up 3\n\
        down 8\n\
        forward 2\n\
        ";

    test!(part_1() == 150);
    test!(part_2() == 900);
    bench_parse!(Vec::len, 1000);
    bench!(part_1() == 2117664);
    bench!(part_2() == 2073416724);
}
