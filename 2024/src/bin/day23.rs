#![feature(test)]
#![feature(iter_intersperse)]

use aoc2024::*;
// FnvHashSet does not implement Hash
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

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
        / 3 // divide by three because we collect each triplet three times
}

fn part_2(graph: &Parsed) -> String {
    let mut queue: VecDeque<BTreeSet<_>> = graph.keys().map(|&c| [c].into()).collect();
    let mut seen: HashSet<BTreeSet<_>> = Default::default();
    let mut biggest: BTreeSet<_> = Default::default();
    while let Some(current) = queue.pop_front() {
        if !seen.insert(current.clone()) {
            continue;
        }
        if current.len() > biggest.len() {
            biggest = current.clone();
        }
        let connected: HashSet<_> = current.iter().flat_map(|&c| &graph[c]).cloned().collect();
        let successors = connected
            .into_iter()
            .filter(|c| !current.contains(c))
            .filter(|c| current.iter().all(|c2| graph[c2].contains(c)))
            .map(|c| {
                let mut new = current.clone();
                new.insert(c);
                new
            });
        queue.extend(successors);
    }
    Iterator::intersperse(biggest.into_iter(), ",").collect()
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
    test!(part_2() == "co,de,ka,ta");
    bench_parse!(HashMap::len, 520);
    bench!(part_1() == 1154);
    // bench!(part_2() == "aj,ds,gg,id,im,jx,kq,nj,ql,qr,ua,yh,zn"); // Takes 1.3s, which is noch benchable
}
