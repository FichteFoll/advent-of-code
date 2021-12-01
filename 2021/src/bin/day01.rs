#![feature(test)]

use aoc2021::*;

const DAY: usize = 01;
type Input = Vec<usize>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(input: &Input) -> usize {
    input.windows(2)
        .filter(|w| w[0] < w[1])
        .count()
}

fn part_2(input: &Input) -> usize {
    input.windows(4)
        .filter(|w| w[0] < w[3])
        .count()
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
        199\n\
        200\n\
        208\n\
        210\n\
        200\n\
        207\n\
        240\n\
        269\n\
        260\n\
        263\n\
        ";

    test!(part_1() == 7);
    test!(part_2() == 5);
    bench_parse!(len, 2000);
    bench!(part_1() == 1195);
    bench!(part_2() == 1235);
}
