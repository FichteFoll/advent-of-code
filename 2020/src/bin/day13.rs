#![feature(test)]

use aoc2020::*;

const DAY: usize = 13;

struct Input {
    start: usize,
    buses: Vec<Option<usize>>,
}

fn parse_input(input_str: &str) -> Input {
    let (start_line, bus_line) = input_str.trim().split_once("\n").unwrap();
    let start = start_line.parse().unwrap();
    let buses = bus_line.split(",")
        .map(|line| line.parse().ok())
        .collect();
    Input { start, buses }
}

fn part_1(input: &Input) -> usize {
    (input.start..)
        .find_map(|i|
            input.buses.iter()
                .flatten()
                .find(|&bus| i % bus == 0)
                .map(|bus| (i - input.start) * bus)
        )
        .unwrap()
}

fn part_2(input: &Input) -> usize {
    let buses: Vec<_> = input.buses.iter()
        .enumerate()
        .filter_map(|(i, ob)| ob.map(|b| (i, b)))
        .collect();
    // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
    let (mut t, mut step) = (0, 1);
    for (j, b) in buses.iter() {
        while (t + j) % b != 0 {
            t += step;
        }
        step *= b;
    }
    t
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
        939\n\
        7,13,x,x,59,x,31,19\n\
        ";

    test!(part_1() == 295);
    test!(part_2() == 1068781);
    bench!(part_1() == 3269);
    bench!(part_2() == 672754131923874);
}
