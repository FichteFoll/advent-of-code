#![feature(custom_test_frameworks)]
#![feature(iterator_try_collect)]
#![feature(test)]

use std::iter::once;

use aoc2022::*;
use itertools::izip;
use parse::parse_input;

const DAY: usize = 19;

const N_RESOURCE: usize = 4;
const N_ROBOTS: usize = 4;

type Parsed = Vec<Blueprint>;
// Blueprint[Robot][Resource]: usize
type Blueprint = [[usize; N_RESOURCE]; N_ROBOTS];

main!();

const ORE: usize = 0;
const CLAY: usize = 1;
const OBSIDIAN: usize = 2;
const GEODE: usize = 3;

macro_rules! arr {
    [$init:expr; $size:expr] => {
        [$init; $size]
    };
    [$init:expr; $size:expr; $($index:expr => $value:expr),+] => {
        {
            let mut tmp_arr = [$init; $size];
            $(
                tmp_arr[$index] = $value;
            )*
            tmp_arr
        }
    };
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.lines().map(parse_line).try_collect().unwrap()
    }

    fn parse_line(line: &str) -> Option<Blueprint> {
        let mut nums = line
            .split_ascii_whitespace()
            .filter_map(|s| s.parse::<usize>().ok());
        let bp = [
            arr![0; N_RESOURCE; ORE => nums.next()?],
            arr![0; N_RESOURCE; ORE => nums.next()?],
            arr![0; N_RESOURCE; ORE => nums.next()?, CLAY => nums.next()?],
            arr![0; N_RESOURCE; ORE => nums.next()?, OBSIDIAN => nums.next()?],
        ];
        nums.next().is_none().then_some(bp)
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed
        .iter()
        .map(simulate)
        .enumerate()
        .map(|(i, result)| (i + 1) * result)
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct State {
    robots: [usize; N_ROBOTS],
    resources: [usize; N_RESOURCE],
}

impl State {
    fn branches(&self, bp: &Blueprint) -> Vec<Self> {
        // TODO see if RAM can handle this
        // Answer: it does not
        let mut result = vec![];
        // Create a branch for each robot produced,
        // which multiplies for each robot type.
        for rob_i in 0..N_RESOURCE {
            let mut next_results = vec![];
            for source in once(self).chain(result.iter()) {
                let lowest_divisor = izip!(source.resources.iter(), bp[rob_i])
                    .filter(|&(_, needed)| needed > 0)
                    .map(|(available, needed)| available / needed)
                    .min()
                    .unwrap_or(0);
                next_results.extend((1..=lowest_divisor).map(|times| {
                    let mut next = source.clone();
                    for (resource, needed) in izip!(next.resources.iter_mut(), bp[rob_i]) {
                        *resource -= needed * times;
                    }
                    next.robots[rob_i] += times;
                    next
                }));
            }
            result.extend(next_results);
        }
        // Always allow no robots being produced.
        result.push(self.clone());
        // Add resources generated this round.
        for branch in result.iter_mut() {
            for (res, rob) in izip!(branch.resources.iter_mut(), self.robots.iter()) {
                *res += rob;
            }
        }
        result
    }
}

fn simulate(bp: &Blueprint) -> usize {
    const MINUTES: usize = 24;
    let start = State {
        robots: arr![0; N_ROBOTS; ORE => 1],
        resources: [0; N_RESOURCE],
    };
    (0..MINUTES)
        .scan(vec![start], |states, _| {
            *states = states
                .into_iter()
                .flat_map(|state| state.branches(bp))
                .collect();
            let max_geode = states
                .iter()
                .map(|state| state.resources[GEODE])
                .max()
                .unwrap();
            Some(max_geode)
        })
        .last()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    const TEST_INPUT: &str = "\
        Blueprint 1: \
          Each ore robot costs 4 ore. \
          Each clay robot costs 2 ore. \
          Each obsidian robot costs 3 ore and 14 clay. \
          Each geode robot costs 2 ore and 7 obsidian.\n\
        \
        Blueprint 2: \
          Each ore robot costs 2 ore. \
          Each clay robot costs 3 ore. \
          Each obsidian robot costs 3 ore and 8 clay. \
          Each geode robot costs 3 ore and 12 obsidian.\n\
        ";

    test!(part_1() == 33);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 30);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);

    #[test_case(1 => 9)]
    #[test_case(2 => 12)]
    fn simulate_test_input(bp_index: usize) -> usize {
        let bp = parse_input(TEST_INPUT)[bp_index - 1];
        simulate(&bp)
    }

    #[test]
    fn branches_1_ore_robot() {
        let state = State {
            robots: arr![0; N_ROBOTS; ORE => 1],
            resources: arr![0; N_RESOURCE],
        };
        let expected = vec![State {
            robots: arr![0; N_ROBOTS; ORE => 1],
            resources: arr![0; N_RESOURCE; ORE => 1],
        }];
        let bp = parse_input(TEST_INPUT)[0];
        assert_eq!(state.branches(&bp), expected);
    }

    #[test]
    fn branches_1_ore_robot_4_ore() {
        let state = State {
            robots: arr![0; N_ROBOTS; ORE => 1],
            resources: arr![0; N_RESOURCE; ORE => 4],
        };
        let expected = vec![
            State {
                robots: arr![0; N_ROBOTS; ORE => 2],
                resources: arr![0; N_RESOURCE; ORE => 1],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 1],
                resources: arr![0; N_RESOURCE; ORE => 3],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 2],
                resources: arr![0; N_RESOURCE; ORE => 1],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1],
                resources: arr![0; N_RESOURCE; ORE => 5],
            },
        ];
        let bp = parse_input(TEST_INPUT)[0];
        assert_eq!(state.branches(&bp), expected);
    }
}
