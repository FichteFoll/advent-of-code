#![feature(test)]

use aoc2024::collections::HashSet;
use aoc2024::*;
use parse::parse_input;

const DAY: usize = 5;

// all numbers have 2 digits
type Rule = (u8, u8);
type Rules = Vec<Rule>;
type Parsed = (Rules, Vec<Vec<u8>>);

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let (rules_block, updates_block) = input.split_once("\n\n").unwrap();
        (parse_rules(rules_block), parse_updates(updates_block))
    }

    fn parse_rules(block: &str) -> Rules {
        block
            .lines()
            .map(|line| {
                let (a, b) = line.split_once('|').unwrap();
                (a.parse().unwrap(), b.parse().unwrap())
            })
            .collect()
    }
    fn parse_updates(block: &str) -> Vec<Vec<u8>> {
        block
            .lines()
            .map(|line| line.split(',').map(|n| n.parse().unwrap()).collect())
            .collect()
    }
}

fn part_1((rules, updates): &Parsed) -> u32 {
    updates
        .iter()
        .filter(|upd| is_ordered(rules, upd))
        .map(|upd| upd[upd.len() / 2] as u32)
        .sum()
}

fn is_ordered(rules: &Rules, upd: &[u8]) -> bool {
    // perf: consider u128 since all nums are <100
    let mut seen = HashSet::default();
    for n in upd.iter() {
        if !seen.insert(*n) { continue; }
        if rules.iter().any(|(req, n2)| n2 == n && upd.contains(req) && !seen.contains(req)) {
            return false
        }
    }
    true
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    const TEST_INPUT: &str = "\
        47|53\n\
        97|13\n\
        97|61\n\
        97|47\n\
        75|29\n\
        61|13\n\
        75|53\n\
        29|13\n\
        97|29\n\
        53|29\n\
        61|53\n\
        97|53\n\
        61|29\n\
        47|13\n\
        75|47\n\
        97|75\n\
        47|61\n\
        75|61\n\
        47|29\n\
        75|13\n\
        53|13\n\
        \n\
        75,47,61,53,29\n\
        97,61,53,29,13\n\
        75,29,13\n\
        75,97,47,61,53\n\
        61,13,29\n\
        97,13,75,29,47\n\
        ";

    test!(part_1() == 143);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| (p.0.len(), p.1.len()), (1176, 182));
    bench!(part_1() == 4872);
    // bench!(part_2() == 0);

    #[test_case(&[75, 47, 61, 53, 29] => true)]
    #[test_case(&[97, 61, 53, 29, 13] => true)]
    #[test_case(&[75, 29, 13] => true)]
    #[test_case(&[75, 97, 47, 61, 53] => false ; "violates 97|75")]
    #[test_case(&[61, 13, 29] => false ; "violates 29|13")]
    #[test_case(&[97, 13, 75, 29, 47] => false ; "violates several rules")]
    fn test_is_ordered(update: &[u8]) -> bool {
        let (rules, _) = parse_input(TEST_INPUT);
        is_ordered(&rules, &update)
    }
}
