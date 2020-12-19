#![feature(test, str_split_once)]

use std::collections::{HashMap, HashSet};

use aoc2020::*;

const DAY: usize = 19;

#[derive(Clone, Debug)]
enum Rule {
    Terminal(u8),
    // Sequence(Vec<usize>),
    Branch(Vec<Vec<usize>>),
}
use Rule::*;

type Rules = HashMap<usize, Rule>;
type RulesLen = HashMap<usize, (Rule, usize)>;
type Input = (Rules, Vec<String>);

struct Matcher {
    rules: RulesLen,
}

impl Matcher {
    fn new(rules: &Rules) -> Self {
        let mut rules_length: HashMap<usize, (Rule, usize)> = HashMap::new();
        for rule_i in rules.keys() {
            Matcher::rule_len(&mut rules_length, rules, rule_i);
        }
        Matcher { rules: rules_length }
    }

    fn rule_len(mut rules_length: &mut RulesLen, rules: &Rules, rule_i: &usize) -> usize {
        if let Some((_, len)) = rules_length.get(rule_i) {
            return *len;
        }
        let rule = &rules[rule_i];
        let len = match rule {
            Terminal(_) => 1,
            Branch(branches) => {
                let lengths: HashSet<_> = branches.iter()
                    .map(|seq| seq.iter()
                        .map(|i| Matcher::rule_len(&mut rules_length, rules, i))
                        .sum()
                    )
                    .collect();
                assert_eq!(lengths.len(), 1, "expected static length in branch rule");
                lengths.into_iter().next().unwrap()
            },
        };
        rules_length.insert(*rule_i, (rule.clone(), len));
        len
    }

    fn matches(&self, word: &[u8]) -> bool {
        let (_, rule_len) = &self.rules[&0];
        if word.len() != *rule_len {
            return false;
        }
        self.matches_inner(&0, word)
    }

    fn matches_inner(&self, rule_i: &usize, word: &[u8]) -> bool {
        let result = match &self.rules[rule_i].0 {
            Terminal(b) => &word[0] == b,
            Branch(branches) => branches.iter()
                .any(|seq| {
                    let mut offset = 0;
                    for sub_i in seq.iter() {
                        if !self.matches_inner(sub_i, &word[offset..]) {
                            return false;
                        }
                        offset += self.rules[&sub_i].1;
                    }
                    true
                }),
        };
        result
    }
}

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

fn part_1(input: &Input) -> usize {
    let matcher = Matcher::new(&input.0);
    input.1.iter()
        .filter(|word| matcher.matches(&word.as_bytes()))
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
        let matcher = Matcher::new(&rules);
        for (word, expected) in [
            ("ababbb", true),
            ("abbbab", true),
            ("bababa", false),
            ("aaabbb", false),
            ("aaaabbb", false),
        ].iter() {
            println!("testing {}", word);
            assert_eq!(matcher.matches(word.as_bytes()), *expected);
        }
    }
}
