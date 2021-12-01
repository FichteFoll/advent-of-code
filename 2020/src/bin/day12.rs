#![feature(test)]

use aoc2020::*;
use aoc2020::grid2d::*;

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

fn part_2(input: &Input) -> i32 {
    let (mut ship, mut wp) = (Point::ZERO, Point::new(10, -1));
    for (cmd, arg) in input.iter() {
        match cmd {
            'N' => wp += Point::N * arg,
            'S' => wp += Point::S * arg,
            'E' => wp += Point::E * arg,
            'W' => wp += Point::W * arg,
            'L' => wp.rotate_left(*arg),
            'R' => wp.rotate_right(*arg),
            'F' => ship += wp * arg,
            _   => panic!("unexpected command"),
        };
    }
    ship.manhattan()
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
    test!(part_2() == 286);
    bench_parse!(len, 791);
    bench!(part_1() == 757);
    bench!(part_2() == 51249);
}
