#![feature(let_chains)]
#![feature(test)]

use std::collections::BinaryHeap;

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

pub fn part_1(map: &Parsed) -> usize {
    highest_relief::<1>(map, 30)
}

pub fn part_2(map: &Parsed) -> usize {
    highest_relief::<2>(map, 26)
}

fn highest_relief<const N: usize>(map: &Parsed, remaining: usize) -> usize {
    let initial = State::initial(remaining);

    // Cache the best solutions until this point; `None` represents the end.
    let mut cache: HashMap<_, State<N>> = Default::default();
    let mut queue: BinaryHeap<_> = [initial].into();

    // Basically Dijkstra.
    while let Some(mut current) = queue.pop() {
        // Check if we already have a better solution till this point.
        let is_finished = current.is_finished();
        let key =
            (!is_finished).then_some((current.positions(), current.rate(), current.open.len()));
        if let Some(other) = cache.get(&key) {
            if other.value() >= current.value() {
                continue;
            }
        }
        cache.insert(key, current.clone());
        if is_finished {
            continue;
        }

        let branches = current.branches(map);
        if branches.is_empty() {
            current.finalize();
            queue.push(current);
        } else {
            queue.extend(branches);
        }
    }
    cache.get(&None).expect("no solution found").released()
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Operator<'a> {
    remaining: usize,
    released: usize,
    position: &'a str,
    rate: usize,
}

impl<'a> Operator<'a> {
    fn initial(remaining: usize) -> Self {
        Self {
            remaining,
            released: 0,
            position: START,
            rate: 0,
        }
    }

    fn value(&self) -> usize {
        // Higher is better.
        self.released + self.rate * self.remaining
    }

    fn finalize(&mut self) {
        self.released += self.rate * self.remaining;
        self.remaining = 0;
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct State<'a, const N: usize> {
    operators: [Operator<'a>; N],
    open: Vec<&'a str>,
}

impl<'a, const N: usize> State<'a, N> {
    fn initial(remaining: usize) -> Self {
        Self {
            operators: [Operator::initial(remaining); N],
            open: vec![],
        }
    }

    // The following are lenses into the `operators` field.
    fn finalize(&mut self) {
        self.operators.iter_mut().for_each(Operator::finalize);
    }

    fn positions(&self) -> [&'a str; N] {
        // There must be a better way …
        let mut res = [""; N];
        for (i, op) in self.operators.iter().enumerate() {
            res[i] = op.position;
        }
        res
    }

    fn value(&self) -> usize {
        self.operators.iter().map(Operator::value).sum()
    }

    fn released(&self) -> usize {
        self.operators.iter().map(|o| o.released).sum()
    }

    fn rate(&self) -> usize {
        self.operators.iter().map(|o| o.rate).sum()
    }

    fn is_finished(&self) -> bool {
        self.operators.iter().all(|o| o.remaining == 0)
    }

    fn branches(&self, map: &'a Parsed) -> Vec<Self> {
        // Move either yourself or the elefant in one iteration.
        let mut vec = vec![];
        if self.open.len() == map.len() - 1 {
            return vec;
        }
        for i in 0..N {
            let mut op = self.operators[i];
            let valve = map.get(op.position).unwrap();
            let next_branches = valve
                .connections
                .iter()
                .filter(|(_, cost)| op.remaining > **cost)
                .map(|(next_pos, cost)| {
                    let mut next = self.clone();
                    let mut next_op = op;
                    next_op.position = next_pos;
                    next_op.remaining -= cost;
                    next_op.released += op.rate * cost;
                    next.operators[i] = next_op;
                    next
                });
            vec.extend(next_branches);
            // Check if rate is > 0 because the initial valve has a rate of 0
            // and whether it makes sense to even open the valve.
            let is_open = || self.open.iter().any(|&x| x == op.position);
            if valve.rate > 0 && op.remaining > 1 && !is_open() {
                op.remaining -= 1;
                op.released += op.rate;
                op.rate += valve.rate;
                let mut next = self.clone();
                next.operators[i] = op;
                next.open.push(op.position);
                vec.push(next);
            }
        }
        vec
    }
}

#[cfg(test)]
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

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    test!(part_1() == 1651);
    test!(part_2() == 1707);
    bench_parse!(HashMap::len, 57 - 42 + 1);
    // Kinda too slow to bench
    bench!(part_1() == 2330);
    // Way too slow to even test
    // bench!(part_2() == 2675);

    const SIMPLE_INPUT: &str = "\
        Valve AA has flow rate=0; tunnels lead to valves BB, DD\n\
        Valve BB has flow rate=3; tunnels lead to valves CC, DD\n\
        Valve CC has flow rate=7; tunnels lead to valves AA\n\
        Valve DD has flow rate=13; tunnels lead to valves CC, AA\n\
        ";

    const NO_SKIP_BB_INPUT: &str = "\
        Valve AA has flow rate=0; tunnels lead to valves BB\n\
        Valve BB has flow rate=3; tunnels lead to valves CC\n\
        Valve CC has flow rate=0; tunnels lead to valves DD\n\
        Valve DD has flow rate=7; tunnels lead to valves BB\n\
        ";

    const SKIP_BB_INPUT: &str = "\
        Valve AA has flow rate=0; tunnels lead to valves BB\n\
        Valve BB has flow rate=3; tunnels lead to valves CC\n\
        Valve CC has flow rate=0; tunnels lead to valves DD\n\
        Valve DD has flow rate=17; tunnels lead to valves BB\n\
        ";

    mod part_1 {
        use super::*;

        test!(simple, SIMPLE_INPUT, part_1() == 13 * 28 + 7 * 26 + 3 * 23);
        // If BB was skipped, would result in 7 * 26 + 3 * 24, which is 5 less.
        test!(no_skip_bb, NO_SKIP_BB_INPUT, part_1() == 3 * 28 + 7 * 25);
        // If BB wasn't skipped, would result in 3 * 28 + 17 * 25, which is 5 less.
        test!(skip_bb, SKIP_BB_INPUT, part_1() == 17 * 26 + 3 * 24);

        #[test]
        fn test_branches_aa() {
            let initial = State::initial(30);
            let map = parse_input(TEST_INPUT);
            let branches = initial.branches(&map);
            let expected = vec![
                State {
                    operators: [Operator {
                        position: "JJ",
                        remaining: 28,
                        released: 0,
                        rate: 0,
                    }],
                    open: vec![],
                },
                State {
                    operators: [Operator {
                        position: "DD",
                        remaining: 29,
                        released: 0,
                        rate: 0,
                    }],
                    open: vec![],
                },
                State {
                    operators: [Operator {
                        position: "BB",
                        remaining: 29,
                        released: 0,
                        rate: 0,
                    }],
                    open: vec![],
                },
            ];
            assert_eq!(branches, expected);
        }

        #[test]
        fn test_branches_jj() {
            let mut initial = State::<1>::initial(30);
            initial.operators[0].position = "JJ";
            let map = parse_input(TEST_INPUT);
            let branches = initial.branches(&map);
            let expected = vec![
                State {
                    operators: [Operator {
                        position: "BB",
                        remaining: 27,
                        released: 0,
                        rate: 0,
                    }],
                    open: vec![],
                },
                State {
                    operators: [Operator {
                        position: "DD",
                        remaining: 27,
                        released: 0,
                        rate: 0,
                    }],
                    open: vec![],
                },
                State {
                    operators: [Operator {
                        position: "JJ",
                        remaining: 29,
                        released: 0,
                        rate: 21,
                    }],
                    open: vec!["JJ"],
                },
            ];
            assert_eq!(branches, expected);
        }
    }
}
