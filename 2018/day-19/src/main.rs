#![feature(test)]

#[macro_use] extern crate derive_error;
extern crate test;

use std::str::FromStr;
use std::error::Error;

// TOCHECK can these be negative?
type Value = usize;

// registers 0->5
type State = [Value; 6];

#[derive(Debug, Error)]
enum InstError {
    ParseError
}

#[derive(Debug)]
struct Instruction {
    opcode: String,
    a: Value,
    b: Value,
    c: Value,
}


impl FromStr for Instruction {
    type Err = Box<Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split_whitespace();
        Ok(Instruction{
            opcode: parts.next().ok_or(InstError::ParseError)?.to_string(),
            a: parts.next().ok_or(InstError::ParseError)?.parse::<Value>()?,
            b: parts.next().ok_or(InstError::ParseError)?.parse::<Value>()?,
            c: parts.next().ok_or(InstError::ParseError)?.parse::<Value>()?,
        })
    }
}

impl Instruction {
    fn run(&self, state: &mut State) {
        state[self.c as usize] = match self.opcode.as_str() {
            "addr" => state[self.a as usize] + state[self.b as usize],
            "addi" => state[self.a as usize] + self.b,
            "mulr" => state[self.a as usize] * state[self.b as usize],
            "muli" => state[self.a as usize] * self.b,
            "banr" => state[self.a as usize] & state[self.b as usize],
            "bani" => state[self.a as usize] & self.b,
            "borr" => state[self.a as usize] | state[self.b as usize],
            "bori" => state[self.a as usize] | self.b,
            "setr" => state[self.a as usize],
            "seti" => self.a,
            "gtir" => (self.a > state[self.b as usize]) as Value,
            "gtri" => (state[self.a as usize] > self.b) as Value,
            "gtrr" => (state[self.a as usize] > state[self.b as usize]) as Value,
            "eqir" => (self.a == state[self.b as usize]) as Value,
            "eqri" => (state[self.a as usize] == self.b) as Value,
            "eqrr" => (state[self.a as usize] == state[self.b as usize]) as Value,
            _ => unimplemented!(),
        };
    }
}

struct Program {
    ip_reg: usize,
    instructions: Vec<Instruction>,
    state: State,
}

impl Program {
    fn step(&mut self) -> bool {
        // false if finished
        let ip = self.state[self.ip_reg];
        match self.instructions.get(ip) {
            Some(instr) => {
                instr.run(&mut self.state);
                self.state[self.ip_reg] += 1;
                true
            },
            None => false
        }
    }

    fn run(&mut self) {
        while self.step() {}
    }
}


fn parse_input(input_str: &str) -> Program {
    let mut lines = input_str.lines();
    let ip_reg: usize = lines.next().expect("input empty").chars().last().unwrap() as usize - 0x30;
    let instructions: Vec<Instruction> = lines
        .map(|line| line.parse().expect("Malformed instr line"))
        .collect();
    Program { ip_reg, instructions, state: [0; 6] }
    // Program::new(ip_reg, instructions)
}


fn part_1(input_str: &str) -> usize {
    let mut program = parse_input(input_str);
    program.run();
    program.state[0]
}


fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    println!("Part 1: {:?}", part_1(&input_str));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn part_1_example() {
        let input_str = "\
            #ip 0\n\
            seti 5 0 1\n\
            seti 6 0 2\n\
            addi 0 1 0\n\
            addr 1 2 3\n\
            setr 1 0 0\n\
            seti 8 0 4\n\
            seti 9 0 5";
        assert_eq!(part_1(&input_str), 7);

        let mut program = parse_input(&input_str);
        program.run();
        assert_eq!(program.state, [7, 5, 6, 0, 0, 9]);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        // 55ms
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(part_1(&input_str), 1920);
        });
    }

    // #[bench]
    // fn bench_part_2(b: &mut Bencher) {
    //     let input_str = std::fs::read_to_string("input_1.txt").expect("can’t read file");
    //     let input2_str = std::fs::read_to_string("input_2.txt").expect("can’t read file");
    //     b.iter(|| {
    //         assert_eq!(part_2(&input_str, &input2_str), 656);
    //     });
    // }
}
