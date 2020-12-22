#![feature(test, str_split_once)]

use std::{collections::{HashMap, HashSet}, hash::Hash, ops::RangeInclusive};

use aoc2020::*;

const DAY: usize = 16;
type Ticket = Vec<usize>;
type Rule = Vec<RangeInclusive<usize>>;
type Input = (HashMap<String, Rule>, Ticket, Vec<Ticket>);

fn parse_input(input_str: &str) -> Input {
    let (rule_block, rest) = input_str
        .trim()
        .split_once("\n\n")
        .unwrap();
    let rules = rule_block.split("\n")
        .map(|line| {
            let (field, range_str) = line.split_once(": ").unwrap();
            let ranges: Vec<_> = range_str.split(" or ")
                .map(|range| {
                    let (a, b) = range.split_once('-').unwrap();
                    a.parse::<usize>().unwrap()..=b.parse::<usize>().unwrap()
                })
                .collect();
            (field.to_string(), ranges)
        })
        .collect();
    let (my_block, other_block) = rest.split_once("\n\n").unwrap();
    fn line_to_ticket(line: &str) -> Vec<usize> {
        line.split(',').map(|n| n.parse().unwrap()).collect()
    }
    let my = line_to_ticket(my_block.split("\n").nth(1).unwrap());
    let other = other_block.split("\n").skip(1).map(|line| line_to_ticket(line)).collect();

    (rules, my, other)
}

fn part_1(input: &Input) -> usize {
    input.2.iter()
        .flatten()
        .filter(|num| !input.0.values().flatten().any(|range| range.contains(num)))
        .sum()
}

// copied and generalized from day 21
fn resolve_multimap<K, V>(multimap: &HashMap<K, HashSet<V>>, singlemap: HashMap<K, V>) -> Option<HashMap<K, V>>
where
    K: Clone + Eq + Hash,
    V: Clone + Eq + Hash,
{
    // recursive depth-first search
    let mut sorted_map: Vec<_> = multimap.iter().collect();
    sorted_map.sort_unstable_by_key(|(_, x)| x.len());
    if let Some((k, vs)) = sorted_map.into_iter().next() {
        for v in vs.iter() {
            let mut new_singlemap = singlemap.clone();
            new_singlemap.insert(k.clone(), v.clone());
            let mut new_multimap = multimap.clone();
            for mis in new_multimap.values_mut() {
                mis.retain(|x| x != v)
            }
            new_multimap.remove(&k);
            let result = resolve_multimap(&new_multimap, new_singlemap);
            if result.is_some() {
                return result;
            }
        }
        None
    } else {
        Some(singlemap)
    }
}

fn determine_field_order(rules: &HashMap<String, Rule>, tickets: &Vec<Ticket>) -> HashMap<usize, String> {
    let field_candidates: HashMap<usize, HashSet<String>> = (0..tickets.first().unwrap().len())
        .map(|i| {
            let nums: Vec<_> = tickets.iter().map(|ticket| ticket[i]).collect();
            let fields: HashSet<_> = rules.iter()
                .filter(|(_, ranges)| nums.iter().all(|n| ranges.iter().any(|r| r.contains(n))))
                .map(|(k, _)| k)
                .cloned()
                .collect();
            (i, fields)
        })
        .collect();
    resolve_multimap(&field_candidates, HashMap::new()).unwrap()
}

fn part_2(input: &Input) -> usize {
    let valid_tickets: Vec<_> = input.2.iter()
        .filter(|ticket|
            !ticket.iter().any(|n|
                !input.0.values().flatten().any(|range| range.contains(n))
            )
        )
        .cloned()
        .collect();
    let field_order = determine_field_order(&input.0, &valid_tickets);
    field_order.into_iter()
        .filter(|(_, name)| name.starts_with("departure"))
        .map(|(i, _)| input.1[i])
        .product()
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
        class: 1-3 or 5-7\n\
        row: 6-11 or 33-44\n\
        seat: 13-40 or 45-50\n\
        \n\
        your ticket:\n\
        7,1,14\n\
        \n\
        nearby tickets:\n\
        7,3,47\n\
        40,4,50\n\
        55,2,20\n\
        38,6,12\n\
        ";

    test!(part_1() == 71);
    test!(part_2() == 1);
    bench_parse!();
    bench!(part_1() == 27850);
    bench!(part_2() == 491924517533);

    #[test]
    fn test_determine_field_order() {
        let input_str = "\
            class: 0-1 or 4-19\n\
            row: 0-5 or 8-19\n\
            seat: 0-13 or 16-19\n\
            \n\
            your ticket:\n\
            11,12,13\n\
            \n\
            nearby tickets:\n\
            3,9,18\n\
            15,1,5\n\
            5,14,9\n\
            ";
        let input = parse_input(input_str);
        let field_order = determine_field_order(&input.0, &input.2);
        let mut vec: Vec<_> = field_order.into_iter().collect();
        vec.sort_unstable();
        let expected = vec![
            (0, "row".to_string()),
            (1, "class".to_string()),
            (2, "seat".to_string()),
        ];
        assert_eq!(vec, expected);
    }
}
