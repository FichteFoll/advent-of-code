#![feature(test)]
#![feature(gen_blocks)]

use aoc2024::*;

const DAY: usize = 22;

type Parsed = Vec<I>;
type I = u64;
const MODULO: I = 16777216;

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

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
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

    test!(part_1() == 37327623);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 2256);
    bench!(part_1() == 19241711734);
    // bench!(part_2() == 0);
}
