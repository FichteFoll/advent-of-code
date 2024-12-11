#![feature(test)]

use aoc2024::*;

const DAY: usize = 11;

type I = u64;
type Parsed = Vec<I>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let mut stones = parsed.clone();
    for _ in 0..25 {
        stones = blink(stones);
    }
    stones.len()
}

fn blink(prev: Parsed) -> Parsed {
    let mut stones = Vec::with_capacity(prev.len());
    for s in prev {
        if s == 0 {
            stones.push(1);
            continue;
        }
        let ndigits = s.ilog10() + 1;
        if ndigits % 2 == 0 {
            let divider = (10 as I).pow(ndigits / 2);
            stones.push(s / divider);
            stones.push(s % divider);
        } else {
            stones.push(s * 2024);
        }
    }
    stones
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "125 17";

    test!(part_1() == 55312);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 8);
    bench!(part_1() == 224529);
    // bench!(part_2() == 0);
}
