#![feature(test, str_split_once)]

use std::collections::HashSet;

fn read_input() -> String {
    std::fs::read_to_string("input/day08.txt").expect("can’t read file")
}

struct Handheld<'a> {
    pub acc: i64,
    pub instructions: &'a [Instruction],
    pub pointer: i64,
    pub seen: HashSet<i64>,
}

struct Instruction {
    pub op: Operation,
    pub arg: i64,
}

enum Operation {
    Acc,
    Jmp,
    Nop,
}

impl<'a> Handheld<'a> {
    fn new(instructions: &'a [Instruction]) -> Self {
        Handheld { acc: 0, instructions, pointer: 0, seen: HashSet::new()}
    }

    fn find_cycle(&mut self) {
        while self.seen.insert(self.pointer) {
            let instr = &self.instructions[self.pointer as usize];
            match instr.op {
                Operation::Acc => {self.acc += instr.arg; self.pointer += 1},
                Operation::Jmp => self.pointer = self.pointer + instr.arg,
                Operation::Nop => self.pointer += 1,
            }
        }
    }
}

type Input = Vec<Instruction>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| {
            let (op_str, arg_str) = line.split_once(" ").unwrap();
            let op = match op_str {
                "acc" => Operation::Acc,
                "nop" => Operation::Nop,
                "jmp" => Operation::Jmp,
                _ => panic!("unknown opcode"),
            };
            Instruction { op, arg: arg_str.parse().unwrap() }
        })
        .collect()
}

fn part_1(input: &Input) -> i64 {
    let mut hh = Handheld::new(input);
    hh.find_cycle();
    hh.acc
}

fn part_2(_input: &Input) -> usize {
    0
}

fn main() {
    let input_str = read_input();
    let input = parse_input(&input_str);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    extern crate test;
    use test::Bencher;

    const EXAMPLE_INPUT_STR: &str = "\
        nop +0\n\
        acc +1\n\
        jmp +4\n\
        acc +3\n\
        jmp -3\n\
        acc -99\n\
        acc +1\n\
        jmp -4\n\
        acc +6\n\
        ";

    #[test]
    fn test_part_1() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_1(&input), 5);
    }

    // #[test]
    // fn test_part_2() {
    //     let input = parse_input(&EXAMPLE_INPUT_STR);
    //     assert_eq!(part_2(&input), 32);
    // }

    #[bench]
    fn bench_parse(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let _ = parse_input(&input_str);
        });
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_1(&input), 1179);
        });
    }

    // #[bench]
    // fn bench_part_2(b: &mut Bencher) {
    //     let input_str = read_input();
    //     let input = parse_input(&input_str);
    //     b.iter(|| {
    //         assert_eq!(part_2(&input), 38426);
    //     });
    // }
}
