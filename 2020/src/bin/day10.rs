#![feature(test)]

use aoc2020::*;

const DAY: usize = 10;
type Input = Vec<usize>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(_input: &Input) -> usize {
    0
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

    const TEST_INPUT_STR: &str = "
        ";

    // test!(part_1() == 0);
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
