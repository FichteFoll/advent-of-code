#![feature(bool_to_option)]
#![feature(map_first_last)]
#![feature(test)]

use std::collections::{BTreeSet, BTreeMap, HashMap};

use itertools::{Itertools, iproduct};
use lazy_static::lazy_static;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 8;

type Parsed = Vec<Line>;
pub struct Line { digits: Vec<Digit>, output: Vec<Digit> }
type Digit = BTreeSet<Wire>;
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
    parsed.iter()
        .map(|line| line.output.iter()
            .filter(|a| NUM_WIRES_WITH_SINGLE_OPTION.contains(&a.len()))
            .count()
        )
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|line| {
            let map = digit_to_int_map(line);
            line.output.iter()
                .map(|digit| map.get(digit).unwrap())
                .fold(0, |a, b| a * 10 + b)
        })
        .sum()
}

//   0:      1:      2:      3:      4:
//  aaaa    ....    aaaa    aaaa    ....
// b    c  .    c  .    c  .    c  b    c
// b    c  .    c  .    c  .    c  b    c
//  ....    ....    dddd    dddd    dddd
// e    f  .    f  e    .  .    f  .    f
// e    f  .    f  e    .  .    f  .    f
//  gggg    ....    gggg    gggg    ....

//   5:      6:      7:      8:      9:
//  aaaa    aaaa    aaaa    aaaa    aaaa
// b    .  b    .  .    c  b    c  b    c
// b    .  b    .  .    c  b    c  b    c
//  dddd    dddd    ....    dddd    dddd
// .    f  e    f  .    f  e    f  .    f
// .    f  e    f  .    f  e    f  .    f
//  gggg    gggg    ....    gggg    gggg
lazy_static! {
    static ref DIGITAL_DISPLAY: Vec<BTreeSet<Wire>> =
        [
            "abcefg",  // 0
            "cf",      // 1
            "acdeg",   // 2
            "acdfg",   // 3
            "bcdf",    // 4
            "abdfg",   // 5
            "abdefg",  // 6
            "acf",     // 7
            "abcdefg", // 8
            "abcdfg",  // 9
        ]
            .into_iter()
            .map(|s| s.chars().collect())
            .collect();

    static ref NUM_WIRES_TO_DIGITS: HashMap<usize, BTreeSet<usize>> =
        DIGITAL_DISPLAY.iter().enumerate()
            .map(|(i, ws)| (ws.len(), i))
            .into_grouping_map()
            .fold(BTreeSet::new(), |mut acc, _, v| { acc.insert(v); acc });

    // const NUM_WIRES_WITH_SINGLE_OPTION: [usize; 4] = [2, 3, 4, 7];
    static ref NUM_WIRES_WITH_SINGLE_OPTION: BTreeSet<usize> =
        NUM_WIRES_TO_DIGITS.iter()
            .filter_map(|(n, wires)| (wires.len() == 1).then_some(n))
            .cloned()
            .collect();
    static ref NUM_COMMON_WIRES: HashMap<(usize, usize), usize> =
        iproduct!(0..DIGITAL_DISPLAY.len(), 0..DIGITAL_DISPLAY.len())
            .filter(|(a, b)| a != b)
            .map(|(a, b)| ((a, b), DIGITAL_DISPLAY[a].intersection(&DIGITAL_DISPLAY[b]).count()))
            .collect();
}

// This function is way too cursed.
// I wanted to generalize my approach with finding common intersections
// but refused to give up when the implementation grew in complexity.
// In the end, I'm just glad that my idea worked, and it's not even that slow.
// Alas, I spent enough time on this already, so it's gonna stay like this.
fn digit_to_int_map(line: &Line) -> BTreeMap<Digit, usize> {
    let all_digits: BTreeSet<_> = line.digits.iter()
        .chain(line.output.iter())
        .cloned()
        .collect();
    let by_num_wires: BTreeMap<usize, BTreeSet<_>> = all_digits.into_iter()
        .into_group_map_by(BTreeSet::len)
        .into_iter()
        .map(|(n, vec)| (n, vec.into_iter().collect()))
        .collect();
    let mut map = BTreeMap::new();
    for n in NUM_WIRES_WITH_SINGLE_OPTION.iter() {
        if let Some(groups) = by_num_wires.get(n) {
            let d = *NUM_WIRES_TO_DIGITS.get(n).unwrap().first().unwrap();
            assert_eq!(groups.len(), 1);
            let group = groups.first().unwrap();
            map.insert(group.clone(), d);
        }
    }

    // to find: 2, 3, 5, 6, 9, 0
    for num_wires in [5, 6] {
        if let Some(groups) = by_num_wires.get(&num_wires) {
            for group in groups {
                let opts: Vec<_> = NUM_WIRES_TO_DIGITS.get(&num_wires)
                    .unwrap()
                    .iter()
                    .filter(|n| !map.values().contains(n))
                    .filter(|&&n|
                        map.iter().all(|(m_wires, &m)| {
                            let common = group.intersection(m_wires).count();
                            let expected = *NUM_COMMON_WIRES.get(&(n, m)).unwrap();
                            expected == common
                        })
                    )
                    .cloned()
                    .collect();
                if opts.len() == 1 {
                    map.insert(group.clone(), opts[0]);
                }
            }
        };
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    #[test]
    fn test_wire_to_digit() {
        let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
        let parsed = parse_input(input);
        let line = parsed.first().unwrap();

        assert_eq!(digit_to_int_map(line).len(), 10);
    }


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

    test!(line_1, "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe", part_2() == 8394);


    test!(part_1() == 26);
    test!(part_2() == 61229);
    bench_parse!(Vec::len, 200);
    bench!(part_1() == 390);
    bench!(part_2() == 1011785);
}
