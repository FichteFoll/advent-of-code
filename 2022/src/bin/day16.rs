#![feature(test)]

use aoc2022::*;
use aoc2022::collections::*;

const DAY: usize = 16;

type Parsed<'a> = HashMap<&'a str, Valve<'a>>;

#[derive(Debug)]
struct Valve<'a> {
    rate: usize,
    connections: Vec<&'a str>,
}

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let mut tokens = line.split([' ', ',', '=', ';']);
            let label = tokens.nth(1).unwrap();
            let rate = tokens.nth(3).unwrap().parse().unwrap();
            let connections = tokens.skip(5).filter(|s| s.len() > 0).collect();
            (label, Valve { rate, connections })
        })
        .collect()
}


fn part_1(map: &Parsed) -> usize {
    let initial = State { position: "AA", remaining: 30, ..State::default() };
    let mut cache: HashMap<_, usize> = Default::default();
    let mut path: Vec<_> = vec![(initial.clone(), 0)];
    'path: while let Some((current, released)) = path.last() {
        if current.remaining == 0 {
            cache.insert(path.pop().unwrap().0, 0);
            continue;
        }
        let branches = current.branches(map);
        let mut max = 0;
        for branch in branches {
            if let Some(&result) = cache.get(&branch) {
                max = max.max(result);
            } else {
                path.push((branch, released + current.rate));
                continue 'path;
            }
        }
        let rate = current.rate;
        cache.insert(path.pop().unwrap().0, max + rate);
        if cache.len() % 100000 == 0 {
            println!("path len {}; cache size {}", path.len(), cache.len());
        }
    }
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
        let mut vec: Vec<_> = valve.connections.iter()
            .map(|next_pos| State {
                position: next_pos,
                remaining: self.remaining - 1,
                ..self.clone()
            })
            .collect();
        if !self.is_open() && valve.rate > 0 {
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

    test!(part_1() == 1651);
    // test!(part_2() == 0);
    bench_parse!(HashMap::len, 57);
    // way too slow to bench
    // bench!(part_1() == 2330);
    // bench!(part_2() == 0);
}
