#![feature(test)]

use aoc2020::*;
use aoc2020::grid::*;

const DAY: usize = 12;
type Input = Vec<(char, i32)>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| (line.chars().nth(0).unwrap(), line[1..].parse().unwrap()))
        .collect()
}

fn part_1(input: &Input) -> i32 {
    let (mut dir, mut pos) = (Point::E, Point::ZERO);
    for (cmd, arg) in input.iter() {
        match cmd {
            'N' => pos += Point::N * arg,
            'S' => pos += Point::S * arg,
            'E' => pos += Point::E * arg,
            'W' => pos += Point::W * arg,
            'L' => dir.rotate_left(*arg),
            'R' => dir.rotate_right(*arg),
            'F' => pos += dir * arg,
            _   => panic!("unexpected command"),
        };
    }
    pos.manhattan()
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
        F10\n\
        N3\n\
        F7\n\
        R90\n\
        F11\n\
        ";

    test!(part_1() == 25);
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    // bench!(part_1() == 757);
    // bench!(part_2() == 0);
}
