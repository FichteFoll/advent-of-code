#![feature(test)]
#![feature(slice_concat_trait)]

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 8;

type Parsed = Vec<Line>;
pub struct Line { digits: Vec<Pattern>, output: Vec<Pattern> }
type Pattern = Vec<Wire>;
type Wire = char;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split('\n')
            .map(|line| {
                let (left, right) = line.split_once(" | ").unwrap();
                let digits = left.split_whitespace()
                    .map(|digit| digit.chars().collect())
                    .collect();
                let output = right.split_whitespace()
                    .map(|digit| digit.chars().collect())
                    .collect();
                Line { digits, output }
            })
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let single_option =  [2, 3, 4, 7];
    parsed.iter()
        .map(|line| line.output.iter()
            .filter(|a| single_option.contains(&a.len()))
            .count()
        )
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
        edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
        fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
        fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
        aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
        fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
        dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
        bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
        egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
        gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n\
        ";

    test!(part_1() == 26);
    // test!(part_2() == 0);
    // bench_parse!(Vec::len, 0);
    bench!(part_1() == 390);
    // bench!(part_2() == 0);
}
