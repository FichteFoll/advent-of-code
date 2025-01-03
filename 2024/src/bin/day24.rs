#![feature(test)]
#![feature(array_chunks)]
#![feature(iter_intersperse)]
#![feature(let_chains)]

use std::collections::VecDeque;

use aoc2024::*;
use collections::{HashMap, HashSet};
use itertools::Itertools;
use parse::parse_input;

const DAY: usize = 24;

type Parsed = (Wires, Vec<Connection>);
type Wires = HashMap<String, bool>;

#[derive(Clone)]
struct Connection {
    in1: String,
    in2: String,
    op: Op,
    out: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Op {
    XOR,
    OR,
    AND,
}

main!();

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
    let wires = run(wires, connections).unwrap();
    num_in_reg(&wires, 'z')
}

fn part_2((wires, connections): &Parsed) -> String {
    // Brute force results in (222 take 2)^4 = 362127085961941521 combinations.
    // We ain't got time for that.
    //
    // Since the machine describes a bit-wise adder,
    // each wire x_n (and y_n) must be transiently connected
    // with the wires z_n, z_{n+1}, ..., z_m, where m is maximum index of z wires.
    // By isolating all connections where this does not hold true,
    // we can (transiently) reduce the number of potentially incorrect wires.
    // We start with z_n & z_{n+1},
    // since x_{n+1} and y_{n+1} should be able to take care
    // of the next index respectively.
    let expected_z = num_in_reg(wires, 'x') + num_in_reg(wires, 'y');
    let num_bits = wires.keys().filter(|k| k.starts_with('x')).count();
    // Assume we have as many y wires as x and that they are without gaps.
    // Also, since we only need to swap outputs,
    // we assume each x_n is correctly connected to a y_n
    // and can check either.
    let candidates: HashSet<_> = (0..num_bits)
        .flat_map(|i| collect_bad_connections(connections, i, 'x'))
        .collect();
    dbg!(&candidates, candidates.len()); // 15 for my input
    let mut swapped_registers = candidates
        .into_iter()
        .combinations(8) // 4 pairs are swapped
        .filter(|outs| {
            println!("{outs:?}");
            // Iterate over all possible pairs for this combination
            outs.iter()
                .permutations(8)
                .map(|outs| outs.array_chunks::<2>().cloned().collect_vec())
                // deduplicate
                .filter(|pairs| pairs.iter().all(|[a, b]| a < b))
                .filter(|pairs| pairs.iter().is_sorted_by_key(|[a, _]| a))
                .any(|pairs| {
                    let mut fixed_connections = connections.clone();
                    for pair in pairs {
                        swap_outputs(&mut fixed_connections, pair[0], pair[1]);
                    }
                    run(wires, &fixed_connections)
                        .map(|result_wires| num_in_reg(&result_wires, 'z') == expected_z)
                        .unwrap_or(false)
                })
        })
        .next()
        .unwrap();

    swapped_registers.sort_unstable();
    Iterator::intersperse(swapped_registers.into_iter(), ",".to_owned()).collect()
}

fn swap_outputs(conns: &mut Vec<Connection>, out1: &str, out2: &str) {
    for c in conns.iter_mut() {
        match &c.out {
            o if o == out1 => c.out = out2.to_owned(),
            o if o == out2 => c.out = out1.to_owned(),
            _ => (),
        }
    }
}

fn run(wires: &Wires, connections: &[Connection]) -> Option<Wires> {
    let mut wires = wires.clone();
    let mut conn_queue: VecDeque<_> = connections.iter().collect();
    let mut cycle_counter = 0;
    while let Some(conn) = conn_queue.pop_front() {
        if let Some(&in1) = wires.get(&conn.in1)
            && let Some(&in2) = wires.get(&conn.in2)
        {
            wires.insert(conn.out.clone(), conn.op.eval(in1, in2));
            cycle_counter = 0;
        } else {
            conn_queue.push_back(conn);
            cycle_counter += 1;
        }
        if cycle_counter > conn_queue.len() {
            return None;
        }
    }
    Some(wires)
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

fn collect_bad_connections(conns: &[Connection], n: usize, prefix: char) -> Vec<String> {
    let source = format!("{prefix}{n:02}");
    let mut expected_heap: Vec<_> = (0..2).map(|i| format!("z{:02}", n + i)).collect();
    expected_heap.reverse();

    let mut connected = vec![];
    let mut queue: VecDeque<_> = [source].into();
    while !expected_heap.is_empty()
        && let Some(wire) = queue.pop_front()
    {
        let successors: HashSet<_> = conns
            .iter()
            .filter(|conn| conn.in1 == &wire[..] || conn.in2 == &wire[..])
            .map(|conn| conn.out.clone())
            .collect();
        connected.extend(successors.clone());
        let z_wires: Vec<_> = successors
            .iter()
            .filter(|out| out.starts_with('z'))
            .collect();
        if z_wires
            .iter()
            .any(|&out| out != expected_heap.last().unwrap())
        {
            // We reached an unexpected z output
            break;
        }
        if !z_wires.is_empty() {
            expected_heap.pop();
        }
        queue.extend(successors);
    }
    if expected_heap.is_empty() {
        vec![]
    } else {
        connected
    }
}

fn num_in_reg(wires: &Wires, reg: char) -> u64 {
    let mut out_wires: Vec<_> = wires.iter().filter(|(k, _)| k.starts_with(reg)).collect();
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
    // test!(part_2() == 0); // example does not work with my code
    bench_parse!(|p: &Parsed| (p.0.len(), p.1.len()), (90, 222));
    bench!(part_1() == 45121475050728);
    // bench!(part_2() == "");
}
