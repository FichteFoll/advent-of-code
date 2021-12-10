#![feature(test)]

use aoc2021::*;

const DAY: usize = 6;

type Parsed = Vec<usize>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split(',')
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    population(parsed, 80)
}

fn part_2(parsed: &Parsed) -> usize {
    population(parsed, 256)
}

fn population(parsed: &Parsed, days: usize) -> usize {
    let mut queue = [0; 9];
    for n in parsed.iter() {
        queue[*n] += 1;
    }
    for i in 0..days {
        queue[(i + 7) % 9] += queue[i % 9];
    }
    queue.into_iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "3,4,3,1,2";

    test!(part_1() == 5934);
    test!(part_2() == 26984457539);
    bench_parse!(Vec::len, 300);
    bench!(part_1() == 387413);
    bench!(part_2() == 1738377086345);
}
