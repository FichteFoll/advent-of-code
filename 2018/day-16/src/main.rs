#![feature(test)]

extern crate test;

use std::error::Error;

use core::str::FromStr;

// TOCHECK can these be negative?
type Value = u8;

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

    fn all() -> [Opcode; 16] {
        use self::Opcode::*;
        [
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
        ]
    }
}

#[derive(Debug)]
struct Instruction {
    opcode: u8,
    a: u8,
    b: u8,
    c: u8,
}

impl FromStr for Instruction {
    type Err = Box<Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let nums: Vec<u8> = s.split_whitespace()
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
            println!("{:?}", (&before, &instr, &after));
            (before, instr, after)
        })
        .collect()
}

fn part_1(input_str: &str) -> usize {
    let input = parse_input(input_str);
    input.into_iter()
        .map(|(before, instr, after)| {
            Opcode::all().into_iter()
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

fn main() {
    let input_str = std::fs::read_to_string("input_1.txt").expect("can’t read file");
    println!("Part 1: {:?}", part_1(&input_str));
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
}
