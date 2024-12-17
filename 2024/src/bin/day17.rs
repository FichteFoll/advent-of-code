#![feature(iter_intersperse)]
#![feature(test)]

use aoc2024::*;
use parse::parse_input;

const DAY: usize = 17;

type Parsed = Computer;
type I = i32;

#[derive(Clone, PartialEq, Eq, Debug)]
struct Computer {
    reg: [I; 3],
    ip: usize,
    program: Vec<u8>,
}

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let (reg_block, prog_block) = input.trim().split_once("\n\n").unwrap();
        let reg_v: Vec<I> = reg_block
            .lines()
            .map(|line| line[12..].parse())
            .collect::<Result<_, _>>()
            .unwrap();
        let program = prog_block
            .bytes()
            .skip(9)
            .filter(|b| b != &b',')
            .map(|b| b - b'0')
            .collect();
        Computer {
            reg: [reg_v[0], reg_v[1], reg_v[2]],
            ip: 0,
            program,
        }
    }
}

fn part_1(parsed: &Parsed) -> String {
    (0..).scan(parsed.clone(), |com, _| com.step())
        .flatten()
        .map(|n| format!("{n}"))
        .intersperse(",".to_owned())
        .collect()
}

fn part_2(_parsed: &Parsed) -> String {
    todo!()
}

impl Computer {
    const ADV: u8 = 0;
    const BXL: u8 = 1;
    const BST: u8 = 2;
    const JNZ: u8 = 3;
    const BXC: u8 = 4;
    const OUT: u8 = 5;
    const BDV: u8 = 6;
    const CDV: u8 = 7;

    fn step(&mut self) -> Option<Option<I>> {
        let instr = *self.program.get(self.ip)?;
        let operand = *self.program.get(self.ip + 1)?;
        self.ip += 2;

        macro_rules! combo {
            () => {
                match operand {
                    0..=3 => operand as I,
                    4..=6 => self.reg[operand as usize - 4],
                    _ => panic!("Invalid combo operand {operand}"),
                }
            };
        }
        macro_rules! xdv {
            () => {
                self.reg[0] / (2 as I).pow(combo!() as u32)
            };
        }

        match instr {
            Self::ADV => self.reg[0] = xdv!(),
            Self::BDV => self.reg[1] = xdv!(),
            Self::CDV => self.reg[2] = xdv!(),
            Self::BXL => self.reg[1] = self.reg[1] ^ operand as I,
            Self::BXC => self.reg[1] = self.reg[1] ^ self.reg[2],
            Self::BST => self.reg[1] = combo!() % 8,
            Self::JNZ if self.reg[0] == 0 => (),
            Self::JNZ => self.ip = operand as usize,
            Self::OUT => return Some(Some(combo!() % 8)),
            _ => panic!("Invalid instruction {operand}"),
        }
        Some(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Register A: 729\n\
        Register B: 0\n\
        Register C: 0\n\
        \n\
        Program: 0,1,5,4,3,0\n\
        ";

    test!(part_1() == "4,6,3,5,6,3,5,2,1,0");
    // test!(part_2() == 0);
    bench_parse!(
        |x: &Parsed| (x.reg, x.program.len()),
        ([66245665, 0, 0], 16)
    );
    bench!(part_1() == "1,4,6,1,6,4,3,0,3");
    // bench!(part_2() == 0);
}
