#![feature(test)]

fn read_input() -> String {
    std::fs::read_to_string("input/day08.txt").expect("can’t read file")
}

#[derive(Clone)]
struct Handheld<'a> {
    pub acc: i64,
    pub instructions: &'a [Instruction],
    pub pointer: usize,
    pub seen: Vec<bool>,
    pub replaced_instr: Option<usize>,
}

#[derive(Debug)]
struct Instruction {
    pub op: Operation,
    pub arg: i64,
}

#[derive(Debug,PartialEq)]
enum Operation {
    Acc,
    Jmp,
    Nop,
}

impl<'a> Handheld<'a> {
    fn new(instructions: &'a [Instruction]) -> Self {
        Handheld {
            acc: 0,
            instructions,
            pointer: 0,
            seen: vec![false; instructions.len()],
            replaced_instr: None,
        }
    }

    fn execute_one(&mut self) {
        self.seen[self.pointer] = true;
        let instr = &self.instructions[self.pointer];
        let is_changed = self.replaced_instr == Some(self.pointer);
        use Operation::*;
        match (&instr.op, is_changed) {
            (Acc, _) => {self.acc += instr.arg; self.pointer += 1},
            (Jmp, false) | (Nop, true) => self.pointer = ((self.pointer as i64) + instr.arg) as usize,
            (Nop, false) | (Jmp, true) => self.pointer += 1,
        }
    }

    fn is_done(&self) -> bool {
        self.pointer >= self.instructions.len()
    }

    fn execute_until_cycle_or_done(&mut self) -> bool {
        loop {
            self.execute_one();
            if self.is_done() {
                return true;
            } else if self.seen[self.pointer] {
                return false;
            }
        }
    }

    fn execute_modified(&mut self) {
        loop {
            if &self.instructions[self.pointer].op != &Operation::Acc {
                let (acc, pointer) = (self.acc, self.pointer);
                self.replaced_instr = Some(pointer);
                if self.execute_until_cycle_or_done() {
                    return;
                }
                // reset state
                (self.acc, self.pointer, self.replaced_instr) = (acc, pointer, None);
            }
            self.execute_one();
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
    hh.execute_until_cycle_or_done();
    hh.acc
}

fn part_2(input: &Input) -> i64 {
    let mut hh = Handheld::new(input);
    hh.execute_modified();
    hh.acc
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

    #[test]
    fn test_part_2() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_2(&input), 8);
    }

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

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 1089);
        });
    }
}
