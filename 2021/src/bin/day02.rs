#![feature(test)]

use std::str::FromStr;

use aoc2021::*;

const DAY: usize = 02;
type Parsed = Vec<Command>;

#[derive(Debug)]
enum Command {
    Forward(usize),
    Up(usize),
    Down(usize),
}

impl FromStr for Command {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Command::*;
        let (word, step_str) =  s.split_once(" ").ok_or_else(|| ParseError::new("no space found"))?;
        let step = step_str.parse().expect("expected number"); // TODO proper error handling?
        match word {
            "forward" => Ok(Forward(step)),
            "up" => Ok(Up(step)),
            "down" => Ok(Down(step)),
            _ => Err(ParseError::new("unexpected keyword")),
        }
    }
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    use Command::*;
    let (distance, depth) = parsed.iter()
        .fold((0usize, 0usize), |(dist, dep), cmd| {
            println!("dist: {dist}, dep: {dep}, cmd: {cmd:?}");
            match cmd {
                &Forward(n) => (dist + n, dep),
                &Up(n) => (dist, dep.saturating_sub(n)),
                &Down(n) => (dist, dep + n),
            }
        });
    let mul = distance * depth;
    println!("distance: {distance}, depth: {depth}, mul: {mul}");
    distance * depth
}

fn part_2(_parsed: &Parsed) -> usize {
    0
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
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    bench!(part_1() == 2117664);
    // bench!(part_2() == 0);
}
