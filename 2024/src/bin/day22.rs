#![feature(test)]
#![feature(gen_blocks)]

use aoc2024::*;
use itertools::Itertools;

const DAY: usize = 22;

type Parsed = Vec<I>;
type I = i64;
const MODULO: I = 16777216; // 1 << 24

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(|line| line.parse().unwrap()).collect()
}

fn part_1(parsed: &Parsed) -> I {
    parsed
        .iter()
        .map(|n| iter_secrets(*n).nth(2000).unwrap())
        .sum()
}

fn part_2(parsed: &Parsed) -> I {
    parsed
        .iter()
        .map(|secret| {
            // (Box::new(iter_secrets(*secret)) as Box<dyn Iterator<Item = I>>) // this is to get rust-analyzer to recognize the type
            iter_secrets(*secret)
                .take(2001)
                .map(|n| n % 10)
                .tuple_windows()
                .map(|(a, b)| (b, b - a))
                .tuple_windows()
                .map(|(t1, t2, t3, t4)| ((t1.1, t2.1, t3.1, t4.1), t4.0))
                .into_grouping_map()
                .reduce(|acc, _, _| acc)
        })
        .flatten()
        .into_grouping_map()
        .sum()
        .into_values()
        .max()
        .unwrap()
}

gen fn iter_secrets(mut secret: I) -> I {
    loop {
        yield secret;
        secret ^= secret * 64;
        secret %= MODULO;
        secret ^= secret / 32;
        secret %= MODULO;
        secret ^= secret * 2048;
        secret %= MODULO;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        1\n\
        10\n\
        100\n\
        2024\n\
        ";

    const TEST_INPUT_2: &str = "\
        1\n\
        2\n\
        3\n\
        2024\n\
        ";

    test!(part_1() == 37327623);
    test!(TEST_INPUT_2, part_2() == 23);
    bench_parse!(Vec::len, 2256);
    bench!(part_1() == 19241711734);
    bench!(part_2() == 2058);
}
