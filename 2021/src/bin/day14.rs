#![feature(array_windows)]
#![feature(test)]

// use std::collections::HashMap;
use fnv::FnvHashMap as HashMap;

use aoc2021::*;
use itertools::{Itertools, MinMaxResult};

const DAY: usize = 14;

type Rules = HashMap<(u8, u8), u8>;
type Parsed<'a> = (&'a [u8], Rules);

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    let (template, rule_lines) = input.trim().split_once("\n\n").unwrap();
    let rules = rule_lines.lines()
        .map(|line| line.as_bytes())
        .map(|b| ((b[0], b[1]), b[6]))
        .collect();
    (template.as_bytes(), rules)
}

fn part_1(parsed: &Parsed) -> usize {
    explode(parsed, 10)
}

fn part_2(parsed: &Parsed) -> usize {
    explode(parsed, 40)
}

type CharMap = HashMap<u8, usize>;
type Cache = HashMap<((u8, u8), usize), CharMap>;

fn explode((template, rules): &Parsed, times: usize) -> usize {
    let mut cache: Cache = HashMap::default();
    let mut map = template.array_windows()
        .map(|&[a, b]| explode_once(rules, &mut cache, (a, b), times))
        .fold(CharMap::default(), join_map);
    let last_char = *template.iter().last().unwrap();
    *map.entry(last_char).or_default() += 1;
    match map.into_values().minmax() {
        MinMaxResult::MinMax(min, max) => max - min,
        _ => panic!("not enough elements"),
    }
}

// Expand the given `pair` `times` times and return the char counts.
// Does not count the right-hand side of the pair, however.
fn explode_once(rules: &Rules, cache: &mut Cache, pair: (u8, u8), times: usize) -> CharMap {
    if let Some(result) = cache.get(&(pair, times)) {
        result.clone()
    } else {
        let middle = *rules.get(&pair).unwrap();
        let result = if times == 1 {
            let mut result = CharMap::default();
            for c in [pair.0, middle] {
                *result.entry(c).or_default() += 1;
            }
            result
        } else {
            let left = explode_once(rules, cache, (pair.0, middle), times - 1);
            let right = explode_once(rules, cache, (middle, pair.1), times - 1);
            join_map(right, left)
        };
        cache.insert((pair, times), result.clone());
        result
    }
}

fn join_map(mut a: CharMap, b: CharMap) -> CharMap {
    for (c, n) in b {
        *a.entry(c).or_default() += n;
    };
    a
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
    test!(part_2() == 2188189693529);
    bench_parse!(|x: &Parsed| (x.0.len(), x.1.len()), (20, 100));
    bench!(part_1() == 2621);
    bench!(part_2() == 2843834241366);
}
