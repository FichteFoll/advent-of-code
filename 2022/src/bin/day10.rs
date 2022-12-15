#![feature(custom_test_frameworks)]
#![feature(test)]

use std::iter::{once, successors};

use aoc2022::*;

const DAY: usize = 10;

type Parsed = Vec<Instr>;

enum Instr {
    Noop,
    AddX(i32),
}

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let words = line.split_ascii_whitespace().collect::<Vec<_>>();
            match &words[..] {
                ["noop"] => Instr::Noop,
                ["addx", operand] => Instr::AddX(operand.parse().unwrap()),
                _ => panic!("bad instruction {line:?}")
            }
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> i32 {
    let xes = calc_xes(parsed);
    successors(Some(20), |n| Some(n + 40))
        .map_while(|n| xes.get(n - 1).map(|x| x * n as i32))
        .sum()
}

fn part_2(parsed: &Parsed) -> String {
    calc_xes(parsed)
        .chunks(40)
        .into_iter()
        .flat_map(|chunk| {
            once('\n').chain(
                chunk.iter()
                    .enumerate()
                    .map(|(i, x)| if (i as i32 - x).abs() <= 1 { '#' } else { '.' })
            )
        })
        .collect()
}

fn calc_xes(instructions: &Parsed) -> Vec<i32> {
    instructions.iter()
        .scan(1, |x, instr| {
            match instr {
                Instr::Noop => Some(vec![*x]),
                Instr::AddX(y) => {
                    let ret = vec![*x; 2];
                    *x += y;
                    Some(ret)
                }
            }
        })
        .flatten()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    const TEST_INPUT: &str = include_str!("../../input/day10_test.txt");
    const PART_2_TEST_RESULT: &str = "\n\
        ##..##..##..##..##..##..##..##..##..##..\n\
        ###...###...###...###...###...###...###.\n\
        ####....####....####....####....####....\n\
        #####.....#####.....#####.....#####.....\n\
        ######......######......######......####\n\
        #######.......#######.......#######.....\
        ";
    const PART_2_RESULT: &str = "\n\
        ####..##....##..##..###....##.###..####.\n\
        #....#..#....#.#..#.#..#....#.#..#.#....\n\
        ###..#.......#.#..#.#..#....#.#..#.###..\n\
        #....#.......#.####.###.....#.###..#....\n\
        #....#..#.#..#.#..#.#....#..#.#.#..#....\n\
        #.....##...##..#..#.#.....##..#..#.####.\
        ";

    test!(part_1() == 13140);
    test!(part_2() == PART_2_TEST_RESULT);
    bench_parse!(Vec::len, 140);
    bench!(part_1() == 12880);
    bench!(part_2() == PART_2_RESULT);

    #[test_case(20 => Some(420))]
    #[test_case(60 => Some(1140))]
    #[test_case(100 => Some(1800))]
    #[test_case(140 => Some(2940))]
    #[test_case(180 => Some(2880))]
    #[test_case(220 => Some(3960))]
    #[test_case(260 => None)]
    fn test_signal_strength_after_cycles(n: usize) -> Option<i32> {
        let instructions = parse_input(TEST_INPUT);
        let xes =  calc_xes(&instructions);
        xes.get(n - 1).map(|&x| x * n as i32)
    }
}
