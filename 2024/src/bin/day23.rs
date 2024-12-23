#![feature(test)]

use aoc2024::*;
// FnvHashSet does not implement Hash
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

const DAY: usize = 23;

type Parsed<'a> = HashMap<&'a str, HashSet<&'a str>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .flat_map(|line| {
            let cmps = line.split_once('-').unwrap();
            [cmps, (cmps.1, cmps.0)]
        })
        .into_grouping_map()
        .aggregate(|acc: Option<HashSet<_>>, _, val| {
            let mut opt = acc.unwrap_or_default();
            opt.insert(val);
            Some(opt)
        })
    // .into_group_map()
    // .into_iter()
    // .map(|(k, vec)| (k, vec.into_iter().collect()))
    // .collect()
}

fn part_1(graph: &Parsed) -> usize {
    let triplets: Vec<HashSet<_>> = graph
        .iter()
        .flat_map(|(&first, connected)| {
            connected
                .iter()
                .tuple_combinations()
                .filter_map(move |(&second, &third)| {
                    graph[second]
                        .contains(third)
                        .then_some([first, second, third].into())
                })
        })
        .collect();
    triplets
        .into_iter()
        .filter(|triplet| triplet.into_iter().any(|c| c.starts_with('t')))
        .count()
        / 3 // divide by three because we collect each tripled three times
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        kh-tc\n\
        qp-kh\n\
        de-cg\n\
        ka-co\n\
        yn-aq\n\
        qp-ub\n\
        cg-tb\n\
        vc-aq\n\
        tb-ka\n\
        wh-tc\n\
        yn-cg\n\
        kh-ub\n\
        ta-co\n\
        de-co\n\
        tc-td\n\
        tb-wq\n\
        wh-td\n\
        ta-ka\n\
        td-qp\n\
        aq-cg\n\
        wq-ub\n\
        ub-vc\n\
        de-ta\n\
        wq-aq\n\
        wq-vc\n\
        wh-yn\n\
        ka-de\n\
        kh-ta\n\
        co-tc\n\
        wh-qp\n\
        tb-vc\n\
        td-yn\n\
        ";

    test!(part_1() == 7);
    // test!(part_2() == 0);
    // bench_parse!(Vec::len, 0);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
