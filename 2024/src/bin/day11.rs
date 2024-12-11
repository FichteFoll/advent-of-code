#![feature(test)]

use aoc2024::*;
use collections::HashMap;

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

fn part_1(parsed: &Parsed) -> I {
    solve(parsed, 25)
}

fn part_2(parsed: &Parsed) -> I {
    solve(parsed, 75)
}

fn solve(stones: &Parsed, steps: usize) -> I {
    let mut cache: HashMap<_, _> = Default::default();
    stones
        .iter()
        .map(move |&s| blink_rec(&mut cache, s, steps))
        .sum()
}

fn blink_rec(cache: &mut HashMap<(I, usize), I>, stone: I, rem: usize) -> I {
    let key = (stone, rem);
    let res = if rem == 0 {
        1
    } else if let Some(&x) = cache.get(&key) {
        x
    } else if stone == 0 {
        blink_rec(cache, 1, rem - 1)
    } else {
        let ndigits = stone.ilog10() + 1;
        if ndigits % 2 == 0 {
            let divider = (10 as I).pow(ndigits / 2);
            blink_rec(cache, stone / divider, rem - 1)
                + blink_rec(cache, stone % divider, rem - 1)
        } else {
            blink_rec(cache, stone * 2024, rem - 1)
        }
    };
    cache.insert(key, res);
    res
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
    bench!(part_2() == 266820198587914);
}
