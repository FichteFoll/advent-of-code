#![feature(test, str_split_once, bool_to_option)]

use std::collections::HashMap;

use aoc2020::*;

const DAY: usize = 19;

#[derive(Clone, Debug)]
enum Rule {
    Terminal(u8),
    Branch(Vec<Vec<usize>>),
}
use Rule::*;

type Rules = HashMap<usize, Rule>;
type Input = (Rules, Vec<String>);

fn parse_input(input_str: &str) -> Input {
    let (rule_block, word_block) = input_str
        .trim()
        .split_once("\n\n")
        .unwrap_or((input_str, ""));
    let rules: HashMap<_, _> = rule_block.split("\n")
        .map(|line| {
            let (i, contents) = line.split_once(": ").unwrap();
            let rule = match contents.as_bytes() {
                [b'"', c, b'"'] => Terminal(*c),
                _ => Branch(
                    contents.split(" | ")
                        .map(|seq|
                            seq.split_whitespace()
                                .map(|n| n.parse().unwrap())
                                .collect()
                        )
                        .collect()
                ),
            };
            (i.parse().unwrap(), rule)
        })
        .collect();
    let words = word_block.split("\n").map(|s| s.to_string()).collect();
    (rules, words)
}

fn matches(rules: &Rules, word: &[u8]) -> bool {
    consume(rules, &0, word).map(|rem| rem.len() == 0).unwrap_or(false)
}

fn consume<'a>(rules: &Rules, rule_i: &usize, word: &'a [u8]) -> Option<&'a [u8]> {
    match &rules[rule_i] {
        Terminal(b) => match word.len() {
            0 => None,
            1 => (word[0] == *b).then_some(&[]),
            _ => (word[0] == *b).then_some(&word[1..]),
        },
        Branch(branches) => branches.iter()
            .find_map(|seq| {
                // TODO scan?
                let mut rem = word;
                for sub_i in seq.iter() {
                    if let Some(new_rem) = consume(rules, sub_i, &rem) {
                        rem = new_rem;
                    } else {
                        return None;
                    }
                }
                Some(rem)
            }),
    }
}

fn part_1(input: &Input) -> usize {
    input.1.iter()
        .filter(|word| matches(&input.0, &word.as_bytes()))
        .count()
}

fn part_2(_input: &Input) -> usize {
    0
}

fn main() {
    let input_str = read_input!();
    let input = parse_input(&input_str);
    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT_STR: &str = "\
        0: 4 1 5\n\
        1: 2 3 | 3 2\n\
        2: 4 4 | 5 5\n\
        3: 4 5 | 5 4\n\
        4: \"a\"\n\
        5: \"b\"\n\
        \n\
        ababbb\n\
        bababa\n\
        abbbab\n\
        aaabbb\n\
        aaaabbb\n\
        ";

    test!(part_1() == 2);
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    bench!(part_1() == 165);
    // bench!(part_2() == 0);

    #[test]
    fn test_matcher() {
        let input_str = "\
            0: 4 1 5\n\
            1: 2 3 | 3 2\n\
            2: 4 4 | 5 5\n\
            3: 4 5 | 5 4\n\
            4: \"a\"\n\
            5: \"b\"\
            ";
        let rules = parse_input(input_str).0;
        for (word, expected) in [
            ("ababbb", true),
            ("abbbab", true),
            ("bababa", false),
            ("aaabbb", false),
            ("aaaabbb", false),
        ].iter() {
            println!("testing {}", word);
            assert_eq!(matches(&rules, word.as_bytes()), *expected);
        }
    }
}
