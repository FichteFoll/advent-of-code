#![feature(test)]

use std::collections::HashMap;

use aoc2021::*;
use itertools::{Itertools, MinMaxResult};

const DAY: usize = 14;

type Rules = HashMap<(char, char), char>;
type Parsed = (Vec<char>, Rules);

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    let (template, rule_lines) = input.trim().split_once("\n\n").unwrap();
    let rules = rule_lines.lines()
        .map(|line| {
            let chars: Vec<_> = line.chars().collect();
            ((chars[0], chars[1]), chars[6])
        })
        .collect();
    (template.chars().collect(), rules)
}

fn part_1((template, rules): &Parsed) -> usize {
    let mut map = template.iter().cloned().tuple_windows()
        .map(|pair| explode_once(rules, pair, 10))
        .fold(HashMap::<char, usize>::new(), |mut a, b| {
            for (c, n) in b {
                *a.entry(c).or_default() += n;
            };
            a
        });

    let last_char = *template.iter().last().unwrap();
    *map.entry(last_char).or_default() += 1;

    match map.into_values().minmax() {
        MinMaxResult::MinMax(min, max) => max - min,
        _ => panic!("not enough elements"),
    }
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

// Expand the given `pair` `times` times and return the char counts.
// Does not count the right-hand side of the pair, however.
fn explode_once(rules: &Rules, pair: (char, char), times: usize) -> HashMap<char, usize> {
    let middle = *rules.get(&pair).unwrap();
    if times == 1 {
        let mut result = HashMap::new();
        for c in [pair.0, middle/*, pair.1*/] {
            *result.entry(c).or_default() += 1;
        }
        return result;
    }
    let mut left_map = explode_once(rules, (pair.0, middle), times - 1);
    let right_map = explode_once(rules, (middle, pair.1), times - 1);
    for (c, n) in right_map {
        *left_map.entry(c).or_default() += n;
    }
    left_map
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        NNCB\n\
        \n\
        CH -> B\n\
        HH -> N\n\
        CB -> H\n\
        NH -> C\n\
        HB -> C\n\
        HC -> B\n\
        HN -> C\n\
        NN -> C\n\
        BH -> H\n\
        NC -> B\n\
        NB -> B\n\
        BN -> B\n\
        BB -> N\n\
        BC -> B\n\
        CC -> N\n\
        CN -> C\n\
        ";

    test!(part_1() == 1588);
    // test!(part_2() == 0);
    bench_parse!(|x: &Parsed| (x.0.len(), x.1.len()), (20, 100));
    bench!(part_1() == 2621);
    // bench!(part_2() == 0);
}
