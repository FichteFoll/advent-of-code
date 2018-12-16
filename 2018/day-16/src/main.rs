#![feature(test)]

extern crate test;

use core::str::FromStr;
use std::error::Error;
use std::collections::{HashMap, HashSet};

// TOCHECK can these be negative?
type Value = usize;

// registers A->D; 0->3
type State = [Value; 4];

// Would like to impl FromStr here,
// but can't since we don't define the trait nor [u8; 4].
fn state_from_str(s: &str) -> State {
    // "Before: [3, 1, 1, 3]"
    let nums_str: String = s.chars()
        .skip_while(|&c| c != '[')
        .skip(1)
        .take_while(|&c| c != ']')
        .collect();
    let nums: Vec<Value> = nums_str.split(", ")
        .map(|x| x.parse().expect("Not a Number"))
        .collect();
    [nums[0], nums[1], nums[2], nums[3]]
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Opcode {
    Addr,
    Addi,
    Mulr,
    Muli,
    Banr,
    Bani,
    Borr,
    Bori,
    Setr,
    Seti,
    Gtir,
    Gtri,
    Gtrr,
    Eqir,
    Eqri,
    Eqrr,
}

impl Opcode {
    fn compute(&self, state: &State, a: Value, b: Value) -> Value {
        use self::Opcode::*;
        match self {
            Addr => state[a as usize] + state[b as usize],
            Addi => state[a as usize] + b,
            Mulr => state[a as usize] * state[b as usize],
            Muli => state[a as usize] * b,
            Banr => state[a as usize] & state[b as usize],
            Bani => state[a as usize] & b,
            Borr => state[a as usize] | state[b as usize],
            Bori => state[a as usize] | b,
            Setr => state[a as usize],
            Seti => a,
            Gtir => (a > state[b as usize]) as Value,
            Gtri => (state[a as usize] > b) as Value,
            Gtrr => (state[a as usize] > state[b as usize]) as Value,
            Eqir => (a == state[b as usize]) as Value,
            Eqri => (state[a as usize] == b) as Value,
            Eqrr => (state[a as usize] == state[b as usize]) as Value,
        }
    }

    const ALL: [Opcode; 16] = [
        Opcode::Addr,
        Opcode::Addi,
        Opcode::Mulr,
        Opcode::Muli,
        Opcode::Banr,
        Opcode::Bani,
        Opcode::Borr,
        Opcode::Bori,
        Opcode::Setr,
        Opcode::Seti,
        Opcode::Gtir,
        Opcode::Gtri,
        Opcode::Gtrr,
        Opcode::Eqir,
        Opcode::Eqri,
        Opcode::Eqrr,
    ];
}

#[derive(Debug)]
struct Instruction {
    opcode: Value,
    a: Value,
    b: Value,
    c: Value,
}

impl FromStr for Instruction {
    type Err = Box<Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let nums: Vec<Value> = s.split_whitespace()
            .map(|x| x.parse().expect("Not a Number"))
            .collect();
        Ok(Instruction{opcode: nums[0],
                       a: nums[1],
                       b: nums[2],
                       c: nums[3]})
    }
}

impl Instruction {
    fn run(&self, op: &Opcode, state: &mut State) {
        state[self.c as usize] = op.compute(state, self.a, self.b);
    }
}

fn parse_input(input_str: &str) -> Vec<(State, Instruction, State)> {
    input_str.split("\n\n")
        .map(|block| {
            let lines: Vec<&str>    = block.lines().collect();
            let before: State       = state_from_str(lines[0]);
            let instr:  Instruction = lines[1].parse().expect("Malformed instr line");
            let after:  State       = state_from_str(lines[2]);
            (before, instr, after)
        })
        .collect()
}

fn part_1(input_str: &str) -> usize {
    let input = parse_input(input_str);
    input.into_iter()
        .map(|(before, instr, after)| {
            Opcode::ALL.iter()
                .filter(|op| {
                    let mut state = before.clone();
                    instr.run(op, &mut state);
                    state == after
                })
                .count()
        })
        .filter(|&c| c >= 3)
        .count()
}

fn find_opcode_map(input: &[(State, Instruction, State)]) -> HashMap<Value, Opcode> {
    let mut opcode_map = input.iter()
        .map(|(before, instr, after)| {
            (instr.opcode,
             Opcode::ALL.iter()
                .filter(|op| {
                    let mut state = before.clone();
                    instr.run(op, &mut state);
                    state == *after
                }).collect::<HashSet<_>>()
            )
        })
        .fold(HashMap::new(), |mut map: HashMap<Value, HashSet<Opcode>>, (icode, ops)| {
            // println!("{} => {:?}", icode, ops);
            map.entry(icode)
                .and_modify(|other_ops| {
                    other_ops.retain(|op| ops.contains(op));
                })
                .or_insert(ops.into_iter().cloned().collect());
            map
        });
    // reduce the map
    let mut final_map: HashMap<Value, Opcode> = HashMap::new();
    loop {
        let singles: HashMap<_, _> = opcode_map.iter().filter(|(_, ops)| ops.len() == 1)
            .map(|(code, ops)| (*code, ops.iter().next().unwrap().clone()))
            .collect();
        if singles.len() == 0 {
            break;
        }
        for (icode, op) in singles {
            opcode_map.remove(&icode);
            for (other_icode, other_ops) in &mut opcode_map {
                if *other_icode != icode {
                    other_ops.retain(|other_op| *other_op != op);
                }
            }
            final_map.insert(icode, op);
        }
    }
    assert_eq!(final_map.len(), 16);
    final_map
}

fn part_2(input_str: &str, input2_str: &str) -> Value {
    let input = parse_input(input_str);
    let opcode_map = find_opcode_map(&input);
    // println!("{:?}", opcode_map);
    let instructions = input2_str.lines().map(|line| line.parse::<Instruction>().unwrap());
    let mut state: State = [0; 4];
    for instr in instructions {
        instr.run(opcode_map.get(&instr.opcode).unwrap(), &mut state);
    }
    state[0]
}

fn main() {
    let input_str = std::fs::read_to_string("input_1.txt").expect("can’t read file");
    let input2_str = std::fs::read_to_string("input_2.txt").expect("can’t read file");
    println!("Part 1: {:?}", part_1(&input_str));
    println!("Part 2: {:?}", part_2(&input_str, &input2_str));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn part_1_example() {
        let input_str = "\
            Before: [3, 2, 1, 1]\n\
            9 2 1 2\n\
            After:  [3, 2, 2, 1]";

        assert_eq!(part_1(&input_str), 1);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input_1.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(part_1(&input_str), 614);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input_1.txt").expect("can’t read file");
        let input2_str = std::fs::read_to_string("input_2.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(part_2(&input_str, &input2_str), 656);
        });
    }
}
