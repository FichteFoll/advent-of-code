#![feature(custom_test_frameworks)]
#![feature(int_roundings)]
#![feature(iterator_try_collect)]
#![feature(let_chains)]
#![feature(test)]

use std::iter::{once, zip};

use aoc2022::collections::*;
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
        input
            .lines()
            .map(|line| parse_line(line).unwrap())
            .collect()
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
        .map(|bp| simulate(bp, 24))
        .enumerate()
        .map(|(i, result)| (i + 1) * result)
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter().take(3).map(|bp| simulate(bp, 32)).product()
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct State {
    robots: [usize; N_ROBOTS],
    resources: [usize; N_RESOURCE],
}

const START: State = State {
    robots: arr![0; N_ROBOTS; ORE => 1],
    resources: [0; N_RESOURCE],
};

impl State {
    fn branches(&self, bp: &Blueprint) -> Vec<Self> {
        once(self.step_no_build())
            .chain((0..N_ROBOTS).flat_map(|rob_i| self.try_build(bp, rob_i)))
            // Filter out states with more robots for a resource than any build plan needs,
            // except for GEODE.
            .filter(|next| {
                next.robots[..GEODE]
                    .iter()
                    .enumerate()
                    .all(|(i, &robs)| bp.iter().any(|plan| plan[i] >= robs))
            })
            .collect()
    }

    fn step_no_build(&self) -> Self {
        let mut next = self.clone();
        for (resource, produced) in zip(next.resources.iter_mut(), self.robots.iter()) {
            *resource += produced;
        }
        next
    }

    #[must_use]
    fn try_build(&self, bp: &Blueprint, rob_i: usize) -> Option<Self> {
        let mut next = self.clone();
        for (resource, needed, produced) in
            izip!(next.resources.iter_mut(), bp[rob_i], self.robots.iter())
        {
            *resource = resource.checked_sub(needed)? + produced;
        }
        next.robots[rob_i] += 1;
        Some(next)
    }
}

fn simulate(bp: &Blueprint, minutes: usize) -> usize {
    let mut states = HashMap::default();
    states.insert(START.robots, vec![START.resources]);
    for i in 0..minutes {
        states = {
            let mut new_states = HashMap::default();
            for (robots, resources_vec) in states {
                for resources in resources_vec {
                    let state = State { robots, resources };
                    for next in state.branches(bp) {
                        // Filter out resource states that we have a better solution for already,
                        // i.e. strictly more resources for each.
                        let other_entries: &mut Vec<[usize; N_RESOURCE]> =
                            new_states.entry(next.robots).or_default();
                        let all_better = !other_entries.is_empty()
                            && other_entries
                                .iter()
                                .all(|other| all_ge(other, &next.resources));
                        if !all_better {
                            other_entries.retain(|other| !all_ge(&next.resources, other));
                            other_entries.push(next.resources);
                        }
                    }
                }
            }
            new_states
        };
        // Leaving this in because that way it's more visible that things are happining
        // during the several seconds of runtime for part 2
        let n_res_vecs: usize = states.values().map(|v| v.len()).sum();
        println!(
            "Minute {}: {} robots states & {} ressource states",
            i + 1,
            states.len(),
            n_res_vecs
        );
    }
    states
        .into_values()
        .flat_map(|resources_vec| resources_vec.into_iter())
        .map(|resources| resources[GEODE])
        .max()
        .unwrap()
}

#[inline(always)]
fn all_ge(xs: &[usize], ys: &[usize]) -> bool {
    zip(xs.iter(), ys.iter()).all(|(x, y)| x >= y)
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

    const TEST_BLUEPRINTS: [[[usize; N_RESOURCE]; N_ROBOTS]; 2] = [
        [
            // force newline
            [4, 0, 0, 0],
            [2, 0, 0, 0],
            [3, 14, 0, 0],
            [2, 0, 7, 0],
        ],
        [
            // force newline
            [2, 0, 0, 0],
            [3, 0, 0, 0],
            [3, 8, 0, 0],
            [3, 0, 12, 0],
        ],
    ];

    test!(part_1() == 33);
    bench_parse!(Vec::len, 30);
    bench!(part_1() == 1681);
    // Takes over 20s.
    // bench!(part_2() == 5394);

    #[test]
    fn test_parse_input() {
        assert_eq!(parse_input(TEST_INPUT), TEST_BLUEPRINTS);
    }

    #[test_case(1 => 9)]
    #[test_case(2 => 12)]
    fn simulate_test_input_24(bp_index: usize) -> usize {
        let bp = TEST_BLUEPRINTS[bp_index - 1];
        simulate(&bp, 24)
    }

    // #[test_case(1 => 56)] // This example takes really long to compute
    #[test_case(2 => 62)]
    fn simulate_test_input_32(bp_index: usize) -> usize {
        let bp = TEST_BLUEPRINTS[bp_index - 1];
        simulate(&bp, 32)
    }

    #[test]
    fn branches_1_ore_robot() {
        let expected = vec![State {
            robots: arr![0; N_ROBOTS; ORE => 1],
            resources: arr![0; N_RESOURCE; ORE => 1],
        }];
        let bp = TEST_BLUEPRINTS[0];
        assert_eq!(START.branches(&bp), expected);
    }

    #[test]
    fn branches_1_ore_robot_4_ore() {
        let state = State {
            robots: arr![0; N_ROBOTS; ORE => 1],
            resources: arr![0; N_RESOURCE; ORE => 4],
        };
        let expected = vec![
            State {
                robots: arr![0; N_ROBOTS; ORE => 1],
                resources: arr![0; N_RESOURCE; ORE => 5],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 2],
                resources: arr![0; N_RESOURCE; ORE => 1],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 1],
                resources: arr![0; N_RESOURCE; ORE => 3],
            },
        ];
        let bp = TEST_BLUEPRINTS[0];
        assert_eq!(state.branches(&bp), expected);
    }
}
