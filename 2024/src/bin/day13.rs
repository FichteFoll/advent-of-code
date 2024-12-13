#![feature(test)]

use aoc2024::*;
use parse::parse_input;

const DAY: usize = 13;

type Parsed = Vec<Machine>;
type I = i32;
type P = (I, I);
#[derive(Debug)]
struct Machine {
    a: P,
    b: P,
    p: P,
}

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.trim().split("\n\n").map(parse_block).collect()
    }

    fn parse_block(input: &str) -> Machine {
        let [a, b, p] = *input
            .lines()
            .map(parse_line)
            .collect::<Option<Vec<_>>>()
            .unwrap()
        else {
            panic!("Bad line: {input}");
        };
        Machine { a, b, p }
    }

    fn parse_line(line: &str) -> Option<P> {
        let (_, nums) = line.split_once(": ")?;
        let (a, b) = nums.split_once(", ")?;
        Some((a[2..].parse().ok()?, b[2..].parse().ok()?))
    }
}

fn part_1(parsed: &Parsed) -> I {
    parsed
        .iter()
        .flat_map(|m| solve_machine(m, 100))
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn solve_machine(m: &Machine, max_allowed: I) -> Option<I> {
    let a = whole_div(m.p.0 * m.b.1 - m.p.1 * m.b.0, m.a.0 * m.b.1 - m.a.1 * m.b.0)?;
    let b = whole_div(m.p.1 - a * m.a.1, m.b.1)?;
    let range = 0..=max_allowed;
    (range.contains(&a) && range.contains(&b)).then_some(3 * a + b)
}

fn whole_div(a: I, b: I) -> Option<I> {
    let r = a / b;
    (r * b == a).then_some(r)
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Button A: X+94, Y+34\n\
        Button B: X+22, Y+67\n\
        Prize: X=8400, Y=5400\n\
        \n\
        Button A: X+26, Y+66\n\
        Button B: X+67, Y+21\n\
        Prize: X=12748, Y=12176\n\
        \n\
        Button A: X+17, Y+86\n\
        Button B: X+84, Y+37\n\
        Prize: X=7870, Y=6450\n\
        \n\
        Button A: X+69, Y+23\n\
        Button B: X+27, Y+71\n\
        Prize: X=18641, Y=10279\n\
        ";

    test!(part_1() == 480);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 320);
    // bench!(part_1() == 0); <41285
    // bench!(part_2() == 0);
}
