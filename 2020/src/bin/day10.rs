#![feature(test)]

use aoc2020::*;

use itertools::Itertools;

const DAY: usize = 10;
type Input = Vec<usize>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(input: &Input) -> usize {
    let mut sorted_input = input.clone();
    sorted_input.push(0);
    sorted_input.sort_unstable();
    let diff = sorted_input.into_iter()
        .tuple_windows()
        .map(|(a, b)| b - a - 1)
        .fold(vec![0; 3], |mut acc, x| {acc[x] += 1; acc});
    diff.first().unwrap() * (diff.last().unwrap() + 1)
}

fn part_2(input: &Input) -> usize {
    let mut sorted_input = input.clone();
    sorted_input.sort_unstable();
    let mut routes = vec![0; *sorted_input.last().unwrap() + 1];
    routes[0] = 1;
    for &i in sorted_input.iter() {
        routes[i] = (&routes[i.saturating_sub(3)..i]).iter().sum();
    }
    *routes.last().unwrap()
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

    const SIMPLE_INPUT_STR: &str = "\
        16\n\
        10\n\
        15\n\
        5\n\
        1\n\
        11\n\
        7\n\
        19\n\
        6\n\
        12\n\
        4\n\
        ";

    #[test]
    fn test_part_1_simple() {
        let input = parse_input(SIMPLE_INPUT_STR);
        assert_eq!(part_1(&input), 5 * 7)
    }

    #[test]
    fn test_part_2_simple() {
        let input = parse_input(SIMPLE_INPUT_STR);
        assert_eq!(part_2(&input), 8)
    }

    const TEST_INPUT_STR: &str = "\
        28\n\
        33\n\
        18\n\
        42\n\
        31\n\
        14\n\
        46\n\
        20\n\
        48\n\
        47\n\
        24\n\
        23\n\
        49\n\
        45\n\
        19\n\
        38\n\
        39\n\
        11\n\
        1\n\
        32\n\
        25\n\
        35\n\
        8\n\
        17\n\
        7\n\
        9\n\
        4\n\
        2\n\
        34\n\
        10\n\
        3\n\
        ";

    test!(part_1() == 10 * 22);
    test!(part_2() == 19208);
    bench_parse!(len, 101);
    bench!(part_1() == 2277);
    bench!(part_2() == 37024595836928);
}
