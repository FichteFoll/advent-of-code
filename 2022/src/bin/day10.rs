#![feature(custom_test_frameworks)]
#![feature(test)]

use aoc2022::*;

const DAY: usize = 10;

type Parsed = Vec<Instr>;

enum Instr {
    Noop,
    AddX(i32),
}

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let words = line.split_ascii_whitespace().collect::<Vec<_>>();
            match &words[..] {
                ["noop"] => Instr::Noop,
                ["addx", operand] => Instr::AddX(operand.parse().unwrap()),
                _ => panic!("bad instruction {:?}", line)
            }
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> i32 {
    let mut state_iter = State::new(parsed).into_iter();
    let nums = std::iter::once(20).chain(std::iter::repeat(40));
    nums.map_while(|n| state_iter.nth(n - 1)).sum()
}
fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

struct State<'a> {
    x: i32,
    processing: Option<(&'a Instr, usize)>,
    instructions: &'a [Instr],
}

impl<'a> State<'a> {
    fn new(instructions: &'a [Instr]) -> Self {
        State { x: 1, processing: None, instructions }
    }

    fn into_iter(self) -> Box<dyn Iterator<Item=i32> + 'a> {
        Box::new((1..).scan(self, |state, i| state.run_one_cycle().map(|x| x * i)))
    }

    // returns value of `x` *during* the execution of the cycle
    fn run_one_cycle(&mut self) -> Option<i32> {
        // TODO make this more readable
        let x = self.x;
        match self.processing {
            Some((Instr::AddX(add), remaining)) if remaining == 1 => {
                self.processing = None;
                self.x += add;
            }
            Some((instr, remaining)) => {
                self.processing = Some((instr, remaining - 1));
            }
            None => {
                if let Some((instr, rest)) = self.instructions.split_first() {
                    self.instructions = rest;
                    match instr {
                        instr @ Instr::AddX(_) => {
                            self.processing = Some((instr, 1));
                        }
                        _ => (),
                    };
                } else {
                    return None;
                }
            }
        };
        Some(x)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    const TEST_INPUT: &str = include_str!("../../input/day10_test.txt");

    test!(part_1() == 13140);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 140);
    bench!(part_1() == 12880);
    // bench!(part_2() == 0);

    #[test_case(20 => Some(420))]
    #[test_case(60 => Some(1140))]
    #[test_case(100 => Some(1800))]
    #[test_case(140 => Some(2940))]
    #[test_case(180 => Some(2880))]
    #[test_case(220 => Some(3960))]
    #[test_case(260 => None)]
    fn test_signal_strength_after_cycles(n: usize) -> Option<i32> {
        let instructions = parse_input(TEST_INPUT);
        let state = State::new(&instructions);
        // collecting this so that `instructions` can be dropped in the right order
        let res = state.into_iter().nth(n - 1);
        res
    }
}
