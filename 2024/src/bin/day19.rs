#![feature(test)]

use std::collections::VecDeque;

use aoc2024::*;
use collections::{HashMap, HashSet};
use parse::parse_input;

const DAY: usize = 19;

type Parsed<'a> = (Vec<&'a str>, Vec<&'a str>);

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let (towels_block, patterns_block) = input.trim().split_once("\n\n").unwrap();
        (
            towels_block.split(", ").collect(),
            patterns_block.lines().collect(),
        )
    }
}

fn part_1((towels, patterns): &Parsed) -> usize {
    patterns
        .iter()
        .filter(|&&ptrn| {
            let mut seen: HashSet<_> = Default::default();
            let mut queue: VecDeque<_> = [ptrn].into();
            while let Some(item) = queue.pop_front() {
                if item.is_empty() {
                    return true;
                }
                if !seen.insert(item) {
                    continue;
                }
                queue.extend(towels.iter().filter_map(|twl| item.strip_prefix(twl)));
            }
            false
        })
        .count()
}

fn part_2((towels, patterns): &Parsed) -> usize {
    let mut suff_cache: HashMap<&str, usize> = Default::default();
    patterns
        .iter()
        .map(|&ptrn| num_solutions(&mut suff_cache, &towels, ptrn))
        .sum()
}

fn num_solutions<'a>(
    suff_cache: &mut HashMap<&'a str, usize>,
    towels: &[&str],
    suffix: &'a str,
) -> usize {
    let res = if suffix.is_empty() {
        1
    } else {
        towels
            .iter()
            .filter_map(|twl| suffix.strip_prefix(twl))
            .map(|n_suffix| match suff_cache.get(n_suffix) {
                Some(&n) => n,
                _ => num_solutions(suff_cache, towels, n_suffix),
            })
            .sum()
    };
    suff_cache.insert(suffix, res);
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        r, wr, b, g, bwu, rb, gb, br\n\
        \n\
        brwrr\n\
        bggr\n\
        gbbr\n\
        rrbgbr\n\
        ubwu\n\
        bwurrg\n\
        brgr\n\
        bbrgwb\n\
        ";

    test!(part_1() == 6);
    test!(part_2() == 16);
    bench_parse!(|p: &Parsed| (p.0.len(), p.1.len()), (447, 400));
    bench!(part_1() == 333);
    bench!(part_2() == 678536865274732);
}
