#![feature(custom_test_frameworks)]
#![feature(int_roundings)]
#![feature(iterator_try_collect)]
#![feature(let_chains)]
#![feature(test)]

use std::iter::once;

use aoc2022::*;
use itertools::izip;
use itertools::Itertools;
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
    parsed.iter()
        .map(find_most_obsidian)
        .max()
        .unwrap()
    // parsed
    //     .iter()
    //     .map(simulate)
    //     .enumerate()
    //     .map(|(i, result)| (i + 1) * result)
    //     .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn find_most_obsidian(bp: &Blueprint) -> usize {
    let mut state = START.clone();
    for i in 1..=24 {
        println!("\nvvv Minute {} vvv", i);
        state = state.step(bp);
        println!("{state:?}\n");
    }
    state.resources[GEODE]
    // iterate(START.clone(), |state| state.step(bp))
    //     .nth(24)
    //     .unwrap()
    //     .resources[GEODE]
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
        // TODO reduce problem space by prioritizing
        // and filtering known bad combinations, such as
        // - having more robots than the most expensive build plan,
        // -
        // let mut result = vec![self.clone()];
        // result.extend((0..N_ROBOTS).flat_map(|rob_i| self.try_build(bp, rob_i)));
        // result
        once(self.clone())
            .chain((0..N_ROBOTS).flat_map(|rob_i| self.try_build(bp, rob_i)))
            .collect()
    }

    fn step(&self, bp: &Blueprint) -> Self {
        // This was a cool idea, but it falls short in multi-leveled predictions
        // because it only considers one level of prioritization
        // (which is enough for the first test case
        // but unfortunately not for the second).
        // Well, it was fun-ish while it lasted.
        //
        // ------------------------------------
        //
        // Determine based on the current robot counts
        // which robots need to be built next to
        // achieve the "ideal ratio" for our target robot kind:
        // the GEODE robot.
        // The next priorities are determined following that.
        // A robot is built if enough resources are available, unless … TODO this is the tricky part
        // ORE needs special-casing because it only depends on the resource it produces.
        let mut robot_order = vec![GEODE];
        // Track the number of iterations needed with the current robots
        // until this robot tier can be built.
        // This is used to not build a robot if building it would delay
        // building the one of the next higher priority.
        let mut can_build_in: [Option<usize>; N_ROBOTS] = Default::default();

        println!("state: {self:?}");
        for i in 0..N_ROBOTS {
            println!("robot_order: {robot_order:?}");
            let Some(&current_robot) = robot_order.get(i) else { break; };
            println!("iteration: {i}; current_robot: {current_robot}");
            if let Some(next) = self.try_build(bp, current_robot) {
                if let Some(&better_robot) = i.checked_sub(1).and_then(|ii| robot_order.get(ii))
                    && let Some(threshold) = dbg!(can_build_in)[better_robot]
                    && let Some(next_buildable_in) = next.buildable_in(bp, better_robot)
                    && dbg!(next_buildable_in + 1) > dbg!(threshold) {
                        if robot_order.len() > i + 1 {
                            continue;
                        }
                } else {
                    println!("building {current_robot}\n");
                    return next;
                }
            }
            can_build_in[current_robot] = self.buildable_in(bp, current_robot);
            println!("can build in: {:?}", can_build_in[current_robot]);

            // Determine the next robots that should be built, prioritized.
            // TODO do we need to add/consider ore requirements of previously required robot plans (robot_order)?
            let plan = bp[current_robot];
            let ratios: Vec<_> = izip!(self.robots.iter(), plan.iter())
                .map(|(&has, &wanted)| match (has, wanted) {
                    (_, 0) => 0f32, // => never
                    (0, _) => wanted as f32,
                    _ => (wanted - has) as f32,
                })
                .collect();
            let priorities: Vec<_> = ratios
                .iter()
                .enumerate()
                .sorted_unstable_by(|t1, t2| t1.1.total_cmp(t2.1).then(t1.0.cmp(&t2.0)).reverse())
                .collect();
            println!("prios: {priorities:?}");

            let next_robots: Vec<_> = priorities
                .iter()
                .map_while(|p| (*p.1 != 0f32).then_some(p.0))
                .filter(|rob_i| !robot_order.contains(rob_i))
                .collect();
            for rob_i in next_robots.into_iter().rev() {
                // insert at the same index in reverse order
                robot_order.insert(i + 1, rob_i);
            }
        }
        println!("building nothing\n");
        return self.step_no_build();
    }

    fn buildable_in(&self, bp: &Blueprint, rob_i: usize) -> Option<usize> {
        let mut max = 0;
        for (&has, needs, &rate) in izip!(self.resources.iter(), bp[rob_i], self.robots.iter()) {
            let rounds = match (needs, rate) {
                (0, _) => 0,
                (_, 0) => return None,
                _ if has > needs => 0,
                _ => (needs - has).div_ceil(rate),
            };
            max = max.max(rounds);
        }
        Some(max)
    }

    fn can_build(&self, bp: &Blueprint, rob_i: usize) -> bool {
        izip!(self.resources.iter(), bp[rob_i]).all(|(resource, needed)| resource >= &needed)
    }

    fn build(&mut self, bp: &Blueprint, rob_i: usize) {
        for (resource, needed) in izip!(self.resources.iter_mut(), bp[rob_i]) {
            *resource -= needed;
        }
        self.robots[rob_i] += 1;
    }

    fn step_no_build(&self) -> Self {
        let mut next = self.clone();
        for (resource, produced) in izip!(next.resources.iter_mut(), self.robots.iter()) {
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

fn simulate(bp: &Blueprint) -> usize {
    const MINUTES: usize = 24;

    (0..MINUTES)
        .scan(vec![START.clone()], |states, _| {
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
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 30);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);

    #[test]
    fn test_parse_input() {
        assert_eq!(parse_input(TEST_INPUT), TEST_BLUEPRINTS);
    }

    #[test_case(1 => 9)]
    #[test_case(2 => 12)]
    fn most_obsidian_test_input(bp_index: usize) -> usize {
        let bp = TEST_BLUEPRINTS[bp_index - 1];
        find_most_obsidian(&bp)
    }

    #[test]
    fn step() {
        let bp = TEST_BLUEPRINTS[0];
        let expected_states = [
            State {
                robots: arr![0; N_ROBOTS; ORE => 1],
                resources: arr![0; N_RESOURCE; ORE => 1],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1],
                resources: arr![0; N_RESOURCE; ORE => 2],
            },
            // minute 3, builds clay robot
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 1],
                resources: arr![0; N_RESOURCE; ORE => 1],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 1],
                resources: arr![0; N_RESOURCE; ORE => 2, CLAY => 1],
            },
            // minute 5, builds clay robot
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 2],
                resources: arr![0; N_RESOURCE; ORE => 1, CLAY => 2],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 2],
                resources: arr![0; N_RESOURCE; ORE => 2, CLAY => 4],
            },
            // minute 7, builds clay robot
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 3],
                resources: arr![0; N_RESOURCE; ORE => 1, CLAY => 6],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 3],
                resources: arr![0; N_RESOURCE; ORE => 2, CLAY => 9],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 3],
                resources: arr![0; N_RESOURCE; ORE => 3, CLAY => 12],
            },
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 3],
                resources: arr![0; N_RESOURCE; ORE => 4, CLAY => 15],
            },
            // minute 11, builds obsidian robot
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 3, OBSIDIAN => 1],
                resources: arr![0; N_RESOURCE; ORE => 2, CLAY => 4],
            },
            // minute 12, builds clay robot
            State {
                robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 4, OBSIDIAN => 1],
                resources: arr![0; N_RESOURCE; ORE => 1, CLAY => 7, OBSIDIAN => 1],
            },
        ];
        println!("Minute 1");
        let mut next = START.step(&bp);
        for (expected, i) in expected_states.into_iter().zip(2..) {
            assert_eq!(next, expected);
            println!("Minute {i}");
            next = next.step(&bp);
        }
    }

    // #[test]
    // fn branches_1_ore_robot() {
    //     let expected = vec![State {
    //         robots: arr![0; N_ROBOTS; ORE => 1],
    //         resources: arr![0; N_RESOURCE; ORE => 1],
    //     }];
    //     let bp = TEST_BLUEPRINTS[0];
    //     assert_eq!(START.branches(&bp), expected);
    // }

    // #[test]
    // fn branches_1_ore_robot_4_ore() {
    //     let state = State {
    //         robots: arr![0; N_ROBOTS; ORE => 1],
    //         resources: arr![0; N_RESOURCE; ORE => 4],
    //     };
    //     let expected = vec![
    //         State {
    //             robots: arr![0; N_ROBOTS; ORE => 1],
    //             resources: arr![0; N_RESOURCE; ORE => 5],
    //         },
    //         State {
    //             robots: arr![0; N_ROBOTS; ORE => 2],
    //             resources: arr![0; N_RESOURCE; ORE => 1],
    //         },
    //         State {
    //             robots: arr![0; N_ROBOTS; ORE => 1, CLAY => 1],
    //             resources: arr![0; N_RESOURCE; ORE => 3],
    //         },
    //     ];
    //     let bp = TEST_BLUEPRINTS[0];
    //     assert_eq!(state.branches(&bp), expected);
    // }
}
