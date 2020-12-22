#![feature(test, str_split_once, map_into_keys_values)]

use std::collections::{HashMap, HashSet};

use aoc2020::*;

const DAY: usize = 21;
type Input<'a> = Vec<(Vec<&'a str>, Vec<&'a str>)>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| {
            let (first, second) = line.split_once(" (contains ").unwrap();
            let ingredients = first.split_whitespace().collect();
            let allergens = second.trim_end_matches(')').split(", ").collect();
            (ingredients, allergens)
        })
        .collect()
}

fn build_allergen_map<'a>(input: &Input<'a>) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut reverse_input: HashMap<&str, HashSet<&str>> = HashMap::new();
    for (is, as_) in input {
        for a in as_ {
            reverse_input.entry(a)
                .and_modify(|set| set.retain(|i| is.contains(i)))
                .or_insert(is.iter().cloned().collect());
        }
    }
    reverse_input
}

fn part_1(input: &Input) -> usize {
    let allergen_map = build_allergen_map(input);
    let mut ingreds: HashSet<&str> = input.iter().flat_map(|(xs, _)| xs.iter()).cloned().collect();
    for is in allergen_map.values() {
        ingreds.retain(|i| !is.contains(i));
    }
    input.iter()
        .map(|(dish_ingreds, _)| dish_ingreds.iter().filter(|i| ingreds.contains(*i)).count())
        .sum()
}

fn part_2(input: &Input) -> String {
    let allergen_map = build_allergen_map(input);
    let resolved_map = resolve_multimap(&allergen_map, HashMap::new()).unwrap();
    let mut keys: Vec<_> = resolved_map.keys().cloned().collect();
    keys.sort_unstable();
    keys.into_iter().map(|k| resolved_map[k]).collect::<Vec<_>>().join(",")
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
        mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
        trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
        sqjhc fvjkl (contains soy)\n\
        sqjhc mxmxvkd sbzzf (contains fish)\n\
        ";

    test!(part_1() == 5);
    test!(part_2() == "mxmxvkd,sqjhc,fvjkl");
    bench_parse!(len, 35);
    bench!(part_1() == 1913);
    bench!(part_2() == "gpgrb,tjlz,gtjmd,spbxz,pfdkkzp,xcfpc,txzv,znqbr");
}
