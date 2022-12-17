#![feature(option_zip)]
#![feature(test)]

use std::cmp::Reverse;

use aoc2022::collections::*;
use aoc2022::*;
use parse::parse_input;

const DAY: usize = 16;

type Parsed<'a> = HashMap<&'a str, Valve<'a>>;

const START: &str = "AA";

#[derive(Debug)]
pub struct Valve<'a> {
    rate: usize,
    // Next valve names with a cost; default is 1.
    connections: HashMap<&'a str, usize>,
}

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let mut map: HashMap<_, _> = input
            .lines()
            .map(|line| {
                let mut tokens = line.split([' ', ',', '=', ';']);
                let label = tokens.nth(1).unwrap();
                let rate = tokens.nth(3).unwrap().parse().unwrap();
                let connections = tokens
                    .skip(5)
                    .filter(|s| !s.is_empty())
                    .map(|s| (s, 1))
                    .collect();
                (label, Valve { rate, connections })
            })
            .collect();
        resolve_zero_rates(&mut map);
        map
    }

    fn resolve_zero_rates(map: &mut Parsed) {
        // Go through valves with zero rates and resolve them to their connections
        // to reduce the number of nodes in the graph.
        let zero_rates: Vec<_> = map
            .iter()
            .filter_map(|(l, v)| (v.rate == 0).then_some(l))
            .cloned()
            .collect();
        for l_resolve in zero_rates.iter() {
            let connections = map.get(l_resolve).unwrap().connections.clone();
            for (l, v) in map.iter_mut() {
                let Some(&l_cost) = v.connections.get(l_resolve) else { continue; };
                for (l_next, c_next) in connections.iter() {
                    if l_next == l {
                        continue;
                    }
                    let cur_cost = v.connections.entry(l_next).or_insert(usize::MAX);
                    *cur_cost = (*cur_cost).min(*c_next + l_cost);
                }
            }
        }
        // Remove zeroed valves from the graph, except "AA".
        for label in zero_rates {
            for v in map.values_mut() {
                v.connections.remove(label);
            }
            if label != START {
                map.remove(label);
            }
        }
    }
}

fn part_1(map: &Parsed) -> usize {
    // dbg!(map);
    let initial = State {
        position: START,
        remaining: 30,
        ..State::default()
    };
    let mut cache: HashMap<_, usize> = Default::default();
    let mut path: Vec<_> = vec![initial.clone()];

    'path: while let Some(current) = path.last() {
        let mut best = None;
        for branch in current.branches(map) {
            if let Some(&b_to_release) = cache.get(&branch) {
                let new = Some((b_to_release, Reverse(branch.remaining)));
                best = best.zip_with(new, Ord::max).or(new);
            } else {
                path.push(branch);
                continue 'path;
            }
        }
        // println!("{} {best:?}", current.remaining);
        let to_release = best
            .map(|(b_to_release, Reverse(b_remaining))| {
                b_to_release + current.rate * (current.remaining - b_remaining)
            })
            .unwrap_or_else(|| current.remaining * current.rate);
        // println!("inserting {current:?} {to_release:?}");
        cache.insert(path.pop().unwrap(), to_release);
        if cache.len() % 100000 == 0 {
            println!("path len {}; cache size {}", path.len(), cache.len());
        }
    }
    println!("final cache size {}", cache.len());
    *cache.get(&initial).unwrap()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[derive(Clone, Hash, PartialEq, Eq, Default, Debug)]
struct State<'a> {
    position: &'a str,
    remaining: usize,
    rate: usize,
    open: Vec<&'a str>,
}

impl<'a> State<'a> {
    fn branches(&self, map: &'a Parsed) -> Vec<Self> {
        let valve = map.get(self.position).unwrap();
        if self.open.len() == map.len() - 1 {
            return vec![];
        }
        let mut vec: Vec<_> = valve
            .connections
            .iter()
            .filter(|(_, cost)| self.remaining > **cost)
            .map(|(next_pos, cost)| State {
                position: next_pos,
                remaining: self.remaining - cost,
                ..self.clone()
            })
            .collect();
        // Check if rate is > 0 because the initial valve has a rate of 0
        // and whether it makes sense to even open the valve.
        if !self.is_open() && valve.rate > 0 && self.remaining > 1 {
            let mut next = self.clone();
            next.rate += valve.rate;
            next.open.push(self.position);
            next.remaining -= 1;
            vec.push(next);
        }
        vec
    }

    fn is_open(&self) -> bool {
        self.open.iter().any(|&x| x == self.position)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
        Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
        Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
        Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
        Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
        Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
        Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
        Valve HH has flow rate=22; tunnel leads to valve GG\n\
        Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
        Valve JJ has flow rate=21; tunnel leads to valve II\n\
        ";

    const SIMPLE_INPUT: &str = "\
        Valve AA has flow rate=0; tunnels lead to valves BB, DD\n\
        Valve BB has flow rate=3; tunnels lead to valves CC, DD\n\
        Valve CC has flow rate=7; tunnels lead to valves AA\n\
        Valve DD has flow rate=13; tunnels lead to valves CC, AA\n\
        ";

    test!(part_1() == 1651);
    test!(simple, SIMPLE_INPUT, part_1() == 13 * 28 + 7 * 26 + 3 * 23);
    // test!(part_2() == 0);
    bench_parse!(HashMap::len, 57 - 42 + 1);
    // Way too slow to bench
    // bench!(part_1() == 2330);
    // bench!(part_2() == 0);

    #[test]
    fn test_branches_aa() {
        let initial = State {
            position: "AA",
            remaining: 30,
            ..State::default()
        };
        let map = parse_input(TEST_INPUT);
        let branches = initial.branches(&map);
        let expected = vec![
            State {
                position: "JJ",
                remaining: 28,
                rate: 0,
                open: vec![],
            },
            State {
                position: "DD",
                remaining: 29,
                rate: 0,
                open: vec![],
            },
            State {
                position: "BB",
                remaining: 29,
                rate: 0,
                open: vec![],
            },
        ];
        assert_eq!(branches, expected);
    }

    #[test]
    fn test_branches_jj() {
        let initial = State {
            position: "JJ",
            remaining: 30,
            ..State::default()
        };
        let map = parse_input(TEST_INPUT);
        let branches = initial.branches(&map);
        let expected = vec![
            State {
                position: "BB",
                remaining: 27,
                rate: 0,
                open: vec![],
            },
            State {
                position: "DD",
                remaining: 27,
                rate: 0,
                open: vec![],
            },
            State {
                position: "JJ",
                remaining: 29,
                rate: 21,
                open: vec!["JJ"],
            },
        ];
        assert_eq!(branches, expected);
    }
}
