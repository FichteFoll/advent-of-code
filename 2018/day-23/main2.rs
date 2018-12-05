#![feature(entry_and_modify)]

use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;


fn get_input() -> String {
    let input_filename = "input_optimized.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}

type Register = char;
type Constant = isize;

#[derive(Debug, Copy, Clone)]
struct Operand {
    register: Option<Register>,
    constant: Option<Constant>,
}

impl Operand {
    fn from_str(from: &str) -> Operand {
        let opt_const = from.parse::<isize>();
        match opt_const {
            Ok(n) => Operand{ register: None, constant: Some(n)},
            Err(_) => {
                assert_eq!(from.len(), 1);
                let register: Option<Register> = from.chars().next();
                Operand { register, constant: None}
            }
        }
    }
}


#[derive(Debug, Clone)]
enum Instruction {
    Set(Register, Operand),
    Sub(Register, Operand),
    Mul(Register, Operand),
    Jnz(Operand, Operand),
}

impl Instruction {
    fn parse(line: &str) -> Instruction {
        let mut tokens = line.split(" ");
        let instr_str = tokens.next().unwrap();
        let reg_str = tokens.next().unwrap();
        let operand_str = tokens.next().unwrap();
        assert!(tokens.next().is_none());

        let register = reg_str.chars().next().unwrap();
        let operand = Operand::from_str(operand_str);

        use Instruction::*;
        match instr_str {
            "set" => Set(register, operand),
            "sub" => Sub(register, operand),
            "mul" => Mul(register, operand),
            "jnz" => Jnz(Operand::from_str(reg_str), operand),
            _ => unimplemented!(),
        }
    }
}


#[derive(Debug)]
struct Registers {
    map: HashMap<Register, Constant>,
}

impl Registers {
    fn new() -> Registers {
        Registers{map: HashMap::new()}
    }

    fn resolve_operand(&self, op: Operand) -> Constant {
        if let Some(op_val) = op.constant {
            op_val
        }
        else if let Some(reg) = op.register {
            self.map.get(&reg).cloned().unwrap_or(0)
        }
        else {
            panic!("Operand is empty: {:?}", op);
        }
    }

    fn run_instr(&mut self, instr: Instruction) -> isize {
        use Instruction::*;
        println!("handling instr {:?}", instr);
        match instr {
            Set(reg, op) => {
                let op_val = self.resolve_operand(op);
                self.map.insert(reg, op_val);
            },
            Sub(reg, op) => {
                *self.map.entry(reg).or_insert(0) -= self.resolve_operand(op);
            },
            Mul(reg, op) => {
                *self.map.entry(reg).or_insert(0) *= self.resolve_operand(op);
            },
            Jnz(op1, op2) => {
                let test_val = self.resolve_operand(op1);
                if test_val != 0 {
                    return self.resolve_operand(op2);
                }
            },
        }
        return 1
    }
}


fn main() {
    let input_str = get_input();
    let input = input_str.trim();

    let instructions: Vec<_> = input.split("\n").map(Instruction::parse).collect();
    let mut registers = Registers::new();
    registers.map.insert('a', 1);
    let mut i: isize = 0;
    while i >= 0 && (i as usize) < instructions.len() {
        let instr = instructions[i as usize].clone();
        i += registers.run_instr(instr);
        println!("registers: {:?}", registers);
    }
    println!("value of register h: {:?}", registers.map[&'h']);
}
