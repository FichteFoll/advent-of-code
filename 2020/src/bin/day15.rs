#![feature(test)]

use aoc2020::*;

const DAY: usize = 15;
type Input = Vec<usize>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split(",")
        .map(|num| num.parse().unwrap())
        .collect()
}

fn memory_game(input: &Input, rounds: usize) -> usize {
    let mut last_spoken: Vec<Option<usize>> = vec![None; rounds];
    for (i, &n) in input.iter().enumerate().rev().skip(1) {
        last_spoken[n] = Some(i + 1);
    }
    let mut last_num = *input.last().unwrap();
    for i in input.len()..rounds {
        let previous_use = last_spoken[last_num];
        let next_num = match previous_use {
            Some(last_i) => i - last_i,
            None => 0,
        };
        last_spoken[last_num] = Some(i);
        last_num = next_num;
    }
    last_num
}

fn part_1(input: &Input) -> usize {
    memory_game(input, 2020)
}

fn part_2(input: &Input) -> usize {
    memory_game(input, 30_000_000)
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

    const TEST_INPUT_STR: &str = "0,3,6";

    test!(part_1() == 436);
    test!(part_2() == 175594);
    bench!(part_1() == 206);
    bench!(part_2() == 955);

    #[test]
    fn test_part_1_more() {
        assert_eq!(part_1(&parse_input("1,3,2")), 1);
        assert_eq!(part_1(&parse_input("2,1,3")), 10);
        assert_eq!(part_1(&parse_input("1,2,3")), 27);
        assert_eq!(part_1(&parse_input("2,3,1")), 78);
        assert_eq!(part_1(&parse_input("3,2,1")), 438);
        assert_eq!(part_1(&parse_input("3,1,2")), 1836);
    }

    #[test]
    fn test_part_2_more() {
        assert_eq!(part_1(&parse_input("1,3,2")), 2578);
        assert_eq!(part_1(&parse_input("2,1,3")), 3544142);
        assert_eq!(part_1(&parse_input("1,2,3")), 261214);
        assert_eq!(part_1(&parse_input("2,3,1")), 6895259);
        assert_eq!(part_1(&parse_input("3,2,1")), 18);
        assert_eq!(part_1(&parse_input("3,1,2")), 362);
    }
}
