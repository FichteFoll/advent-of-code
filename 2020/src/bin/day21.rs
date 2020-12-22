#![feature(test, str_split_once, map_into_keys_values)]

use std::collections::{HashMap, HashSet};

use aoc2020::*;

use itertools::Itertools;

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

#[derive(Clone, Debug)]
struct Assignments<'a> {
    map: HashMap<&'a str, &'a str>,
    assigned: HashSet<&'a str>,
}

impl<'a> Assignments<'a> {
    fn new() -> Self {
        Self { map: HashMap::new(), assigned: HashSet::new() }
    }
}


fn assign_allergens<'a>(input: &Input<'a>) -> Vec<HashMap<&'a str, &'a str>> {
    // Try all legit combinations of allergen assignments ingredients within our state;
    // bail if there is a conflict
    let mut states = vec![Assignments::new()];

    for (is, as_) in input {
        println!("ingredients: {:?}, valid states: {}", is, states.len());
        states = states.into_iter()
            .flat_map(|in_state| {
                let contained_as: HashSet<&str> = is.iter().filter_map(|i| in_state.map.get(i)).cloned().collect();
                let reduced_as = as_.iter().filter(|a| !contained_as.contains(*a)).collect_vec();
                (0..is.len()).permutations(reduced_as.len())
                    .filter_map(move |indices| {
                        let mut state = in_state.clone();
                        // println!("new state");
                        for (ai, ii) in indices.into_iter().enumerate() {
                            let a = reduced_as[ai];
                            let i = is[ii];
                            if state.map.insert(i, a) != None || !state.assigned.insert(a) {
                                return None; // ingredient or allergene have assignment already
                            }
                        }
                        Some(state)
                    })
            })
            .collect();
    }
    states.into_iter().map(|ass| ass.map).collect()
}

fn find_ingreds_without_allergens<'a>(input: &Input<'a>) -> HashSet<&'a str> {
    let mut ingreds: HashSet<&str> = input.iter().flat_map(|(xs, _)| xs.iter()).cloned().collect();
    let mappings = assign_allergens(input);
    for mapping in mappings {
        ingreds.retain(|i| !mapping.contains_key(i));
    }
    ingreds
}

fn part_1(input: &Input) -> usize {
    let ingreds = find_ingreds_without_allergens(input);
    input.iter()
        .map(|(dish_ingreds, _)| dish_ingreds.iter().filter(|i| ingreds.contains(*i)).count())
        .sum()
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
        mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
        trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
        sqjhc fvjkl (contains soy)\n\
        sqjhc mxmxvkd sbzzf (contains fish)\n\
        ";

    test!(part_1() == 5);
    // test!(part_2() == 0);
    // bench_parse!(len, 0);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
