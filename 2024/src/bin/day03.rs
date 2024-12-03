#![feature(test)]
#![feature(let_chains)]

use aoc2024::*;
use itertools::Itertools;
use regex::Regex;

const DAY: usize = 3;

type Parsed = String;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().join(" ")
}

fn part_1(parsed: &Parsed) -> usize {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    re.captures_iter(parsed)
        .map(|cap| {
            cap.get(1).unwrap().as_str().parse::<usize>().unwrap()
                * cap.get(2).unwrap().as_str().parse::<usize>().unwrap()
        })
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let re = Regex::new(r"mul\((\d+),(\d+)\)|don't\(\).*?do\(\)").unwrap();
    re.captures_iter(parsed)
        .map(|cap| {
            dbg!(cap.get(0).unwrap());
            if let Some(g1) = cap.get(1) && let Some(g2) = cap.get(2) {
                g1.as_str().parse::<usize>().unwrap()
                    * g2.as_str().parse::<usize>().unwrap()
            } else {
                0
            }
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))\n\
        ";

    const TEST_INPUT2: &str = "\
        xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))\n\
        ";

    test!(part_1() == 161);
    test!(TEST_INPUT2, part_2() == 48);
    bench_parse!(String::len, 18683);
    bench!(part_1() == 161085926);
    bench!(part_2() == 82045421);
}
