#![feature(test)]

use std::collections::VecDeque;

use aoc2024::*;
use collections::HashSet;
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

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
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
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| (p.0.len(), p.1.len()), (447, 400));
    bench!(part_1() == 333);
    // bench!(part_2() == 0);
}
