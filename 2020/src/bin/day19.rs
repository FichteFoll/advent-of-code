#![feature(test, bool_to_option)]

use std::collections::HashMap;

use itertools::fold;

use aoc2020::*;

const DAY: usize = 19;

#[derive(Clone, Debug)]
enum Rule {
    Terminal(u8),
    Branch(Vec<Vec<usize>>),
    Nop,
}
use Rule::*;

type Rules = Vec<Rule>;
type Input = (Rules, Vec<String>);

fn parse_input(input_str: &str) -> Input {
    let (rule_block, word_block) = input_str
        .trim()
        .split_once("\n\n")
        .unwrap_or((input_str, ""));
    let rule_map: HashMap<usize, _> = rule_block.split("\n")
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
    let mut rules = vec![Nop; *rule_map.keys().max().unwrap() + 1];
    for (i, rule) in rule_map {
        rules[i] = rule;
    }
    let words = word_block.split("\n").map(|s| s.to_string()).collect();
    (rules, words)
}

fn matches(rules: &Rules, word: &[u8]) -> bool {
    consume(rules, 0, word).iter().any(|rem| rem.len() == 0)
}

const EMPTY: &[u8] = &[];

fn consume<'a>(rules: &Rules, rule_i: usize, word: &'a [u8]) -> Vec<&'a [u8]> {
    match &rules[rule_i] {
        Terminal(b) =>
            word.get(0)
                .filter(|&x| b == x)
                .map(|_| (word.len() > 1).then_some(&word[1..]).unwrap_or(EMPTY))
                .into_iter()
                .collect(),
        Branch(branches) =>
            branches.iter()
                .flat_map(|seq|
                    fold(seq.iter(), vec![word],
                        |rems, &sub_i| rems.iter()
                            .flat_map(|rem| consume(rules, sub_i, &rem))
                            .collect()
                    )
                )
                .collect(),
        Nop => panic!("no rule for {}", rule_i),
    }
}

fn rules_for_part_2(inp: &Rules) -> Rules {
    let mut rules = inp.clone();
    rules[8] = Branch(vec![vec![42], vec![42, 8]]);
    rules[11] = Branch(vec![vec![42, 31], vec![42, 11, 31]]);
    rules
}

fn part_1(input: &Input) -> usize {
    input.1.iter()
        .filter(|word| matches(&input.0, &word.as_bytes()))
        .count()
}

fn part_2(input: &Input) -> usize {
    let rules = rules_for_part_2(&input.0);
    input.1.iter()
        .filter(|word| matches(&rules, &word.as_bytes()))
        .count()
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

    const TEST_INPUT_2_STR: &str = "\
        42: 9 14 | 10 1\n\
        9: 14 27 | 1 26\n\
        10: 23 14 | 28 1\n\
        1: \"a\"\n\
        11: 42 31\n\
        5: 1 14 | 15 1\n\
        19: 14 1 | 14 14\n\
        12: 24 14 | 19 1\n\
        16: 15 1 | 14 14\n\
        31: 14 17 | 1 13\n\
        6: 14 14 | 1 14\n\
        2: 1 24 | 14 4\n\
        0: 8 11\n\
        13: 14 3 | 1 12\n\
        15: 1 | 14\n\
        17: 14 2 | 1 7\n\
        23: 25 1 | 22 14\n\
        28: 16 1\n\
        4: 1 1\n\
        20: 14 14 | 1 15\n\
        3: 5 14 | 16 1\n\
        27: 1 6 | 14 18\n\
        14: \"b\"\n\
        21: 14 1 | 1 14\n\
        25: 1 1 | 1 14\n\
        22: 14 14\n\
        8: 42\n\
        26: 14 22 | 1 20\n\
        18: 15 15\n\
        7: 14 5 | 1 21\n\
        24: 14 1\n\
        \n\
        abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
        bbabbbbaabaabba\n\
        babbbbaabbbbbabbbbbbaabaaabaaa\n\
        aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
        bbbbbbbaaaabbbbaaabbabaaa\n\
        bbbababbbbaaaaaaaabbababaaababaabab\n\
        ababaaaaaabaaab\n\
        ababaaaaabbbaba\n\
        baabbaaaabbaaaababbaababb\n\
        abbbbabbbbaaaababbbbbbaaaababb\n\
        aaaaabbaabaaaaababaa\n\
        aaaabbaaaabbaaa\n\
        aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
        babaaabbbaaabaababbaabababaaab\n\
        aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba\n\
        ";

    test!(part_1() == 2);
    test!(example_2, TEST_INPUT_2_STR, part_1() == 3);
    test!(TEST_INPUT_2_STR, part_2() == 12);
    bench!(part_1() == 165);
    bench!(part_2() == 274);

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

    #[test]
    fn test_matcher_2() {
        let rules = parse_input(TEST_INPUT_2_STR).0;
        let rules_mod = rules_for_part_2(&rules);
        for (word, expected) in [
            ("bbabbbbaabaabba", true),
            ("babbbbaabbbbbabbbbbbaabaaabaaa", true),
            ("aaabbbbbbaaaabaababaabababbabaaabbababababaaa", true),
            ("bbbbbbbaaaabbbbaaabbabaaa", true),
            ("bbbababbbbaaaaaaaabbababaaababaabab", true),
            ("ababaaaaaabaaab", true),
            ("ababaaaaabbbaba", true),
            ("baabbaaaabbaaaababbaababb", true),
            ("abbbbabbbbaaaababbbbbbaaaababb", true),
            ("aaaaabbaabaaaaababaa", true),
            ("aaaabbaabbaaaaaaabbbabbbaaabbaabaaa", true),
            ("aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba", true),
        ].iter() {
            println!("\ntesting {}", word);
            assert_eq!(matches(&rules_mod, word.as_bytes()), *expected);
        }
    }

    // fn
}
