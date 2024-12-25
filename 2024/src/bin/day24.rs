#![feature(test)]
#![feature(let_chains)]

use std::collections::VecDeque;

use aoc2024::*;
use collections::HashMap;
use parse::parse_input;

const DAY: usize = 24;

type Parsed = (Wires, Vec<Connection>);
type Wires = HashMap<String, bool>;
struct Connection {
    in1: String,
    in2: String,
    op: Op,
    out: String,
}

main!();

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Op {
    XOR,
    OR,
    AND,
}

impl Op {
    fn eval(&self, in1: bool, in2: bool) -> bool {
        match self {
            Op::XOR => in1 ^ in2,
            Op::OR => in1 || in2,
            Op::AND => in1 && in2,
        }
    }
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let (wires_block, conn_block) = input.trim().split_once("\n\n").unwrap();
        let wires = wires_block
            .lines()
            .map(|line| {
                let (wire, val) = line.split_once(": ").unwrap();
                (wire.to_owned(), val == "1")
            })
            .collect();
        let connections = conn_block
            .lines()
            .map(|line| parse_connection(line))
            .collect();
        (wires, connections)
    }

    fn parse_connection(line: &str) -> Connection {
        let mut tokens = line.split_ascii_whitespace();
        let in1 = tokens.next().unwrap().to_owned();
        let op = match tokens.next().unwrap() {
            "AND" => Op::AND,
            "OR" => Op::OR,
            "XOR" => Op::XOR,
            t => panic!("Unexpected token: {t}"),
        };
        let in2 = tokens.next().unwrap().to_owned();
        assert_eq!(tokens.next(), Some("->"));
        let out = tokens.next().unwrap().to_owned();
        debug_assert_eq!(tokens.next(), None);
        Connection { in1, in2, op, out }
    }
}

fn part_1((wires, connections): &Parsed) -> u64 {
    let mut wires = wires.clone();
    let mut conn_queue: VecDeque<_> = connections.iter().collect();
    while let Some(conn) = conn_queue.pop_front() {
        if let Some(&in1) = wires.get(&conn.in1)
            && let Some(&in2) = wires.get(&conn.in2)
        {
            wires.insert(conn.out.clone(), conn.op.eval(in1, in2));
        } else {
            conn_queue.push_back(conn);
        }
    }
    output(&wires)
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn output(wires: &Wires) -> u64 {
    let mut out_wires: Vec<_> = wires.iter().filter(|(k, _)| k.starts_with('z')).collect();
    out_wires.sort_unstable();
    out_wires
        .into_iter()
        .rev()
        .fold(0, |acc, (_, &b)| (acc << 1) + b as u64)
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        x00: 1\n\
        x01: 0\n\
        x02: 1\n\
        x03: 1\n\
        x04: 0\n\
        y00: 1\n\
        y01: 1\n\
        y02: 1\n\
        y03: 1\n\
        y04: 1\n\
        \n\
        ntg XOR fgs -> mjb\n\
        y02 OR x01 -> tnw\n\
        kwq OR kpj -> z05\n\
        x00 OR x03 -> fst\n\
        tgd XOR rvg -> z01\n\
        vdt OR tnw -> bfw\n\
        bfw AND frj -> z10\n\
        ffh OR nrd -> bqk\n\
        y00 AND y03 -> djm\n\
        y03 OR y00 -> psh\n\
        bqk OR frj -> z08\n\
        tnw OR fst -> frj\n\
        gnj AND tgd -> z11\n\
        bfw XOR mjb -> z00\n\
        x03 OR x00 -> vdt\n\
        gnj AND wpb -> z02\n\
        x04 AND y00 -> kjc\n\
        djm OR pbm -> qhw\n\
        nrd AND vdt -> hwm\n\
        kjc AND fst -> rvg\n\
        y04 OR y02 -> fgs\n\
        y01 AND x02 -> pbm\n\
        ntg OR kjc -> kwq\n\
        psh XOR fgs -> tgd\n\
        qhw XOR tgd -> z09\n\
        pbm OR djm -> kpj\n\
        x03 XOR y03 -> ffh\n\
        x00 XOR y04 -> ntg\n\
        bfw OR bqk -> z06\n\
        nrd XOR fgs -> wpb\n\
        frj XOR qhw -> z04\n\
        bqk OR frj -> z07\n\
        y03 OR x01 -> nrd\n\
        hwm AND bqk -> z03\n\
        tgd XOR rvg -> z12\n\
        tnw OR pbm -> gnj\n\
        ";

    test!(part_1() == 2024);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| (p.0.len(), p.1.len()), (90, 222));
    bench!(part_1() == 45121475050728);
    // bench!(part_2() == 0);
}
