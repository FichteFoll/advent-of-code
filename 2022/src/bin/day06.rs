#![feature(array_windows)]
#![feature(test)]

use aoc2022::*;
use itertools::Itertools;
use parse::parse_input;

const DAY: usize = 6;

type Parsed = String;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.trim().to_string()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    find_unique_sequence::<4>(parsed)
}

fn part_2(parsed: &Parsed) -> usize {
    find_unique_sequence::<14>(parsed)
}

fn find_unique_sequence<const SEQUENCE_LEN: usize>(parsed: &String) -> usize {
    parsed.as_bytes()
        .array_windows::<SEQUENCE_LEN>()
        .find_position(|a| a.iter().unique().count() == SEQUENCE_LEN)
        .unwrap().0 + SEQUENCE_LEN
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    test!(part_1() == 7);
    bench_parse!(String::len, 4095);
    bench!(part_1() == 1582);
    bench!(part_2() == 3588);

    test!(ex1, "bvwbjplbgvbhsrlpgdmjqwftvncz", part_1() == 5);
    test!(ex2, "nppdvjthqldpwncqszvftbrmjlhg", part_1() == 6);
    test!(ex3, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", part_1() == 10);
    test!(ex4, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", part_1() == 11);

    test!(ex1, "mjqjpqmgbljsphdztnvjfqwrcgsmlb", part_2() == 19);
    test!(ex2, "bvwbjplbgvbhsrlpgdmjqwftvncz", part_2() == 23);
    test!(ex3, "nppdvjthqldpwncqszvftbrmjlhg", part_2() == 23);
    test!(ex4, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", part_2() == 29);
    test!(ex5, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", part_2() == 26);
}
