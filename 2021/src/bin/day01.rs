#![feature(test)]

use aoc2021::*;

const DAY: usize = 01;
type Parsed = Vec<usize>;

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.windows(2)
        .filter(|w| w[0] < w[1])
        .count()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.windows(4)
        .filter(|w| w[0] < w[3])
        .count()
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
