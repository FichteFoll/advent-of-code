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
    parsed.as_bytes()
        .array_windows::<4>()
        .find_position(|a| a.iter().unique().count() == 4)
        .unwrap().0 + 4
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    test!(part_1() == 7);
    // test!(part_2() == 0);
    bench_parse!(String::len, 4095);
    // bench!(part_1() == 1582);
    // bench!(part_2() == 0);

    #[test]
    fn test_part_1_more() {
        assert_eq!(part_1(&parse_input("bvwbjplbgvbhsrlpgdmjqwftvncz")), 5);
        assert_eq!(part_1(&parse_input("nppdvjthqldpwncqszvftbrmjlhg")), 6);
        assert_eq!(part_1(&parse_input("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")), 10);
        assert_eq!(part_1(&parse_input("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")), 11);
    }
}
