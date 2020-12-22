#![feature(test, bool_to_option)]

use std::collections::{HashSet, VecDeque};

use aoc2020::*;

use itertools::Itertools;

const DAY: usize = 22;
type Input = Vec<VecDeque<usize>>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n\n")
        .map(|deck| deck.split("\n").skip(1).map(|line| line.parse().unwrap()).collect())
        .collect()
}

fn winners_score(decks: Input) -> usize {
    decks.into_iter()
        .find(|d| d.len() != 0)
        .unwrap()
        .into_iter()
        .rev()
        .enumerate()
        .map(|(i, card)| (i + 1) * card)
        .sum()
}

fn part_1(input: &Input) -> usize {
    let mut decks = input.clone();
    while !decks.iter().any(|d| d.len() == 0) {
        let cards: Vec<_> = decks.iter_mut().map(|d| d.pop_front().unwrap()).collect();
        let max = cards.iter().max().unwrap();
        for (card, d) in cards.iter().zip(decks.iter_mut()) {
            if card == max {
                d.push_back(*max);
                d.extend(cards.iter().filter(|&c| c != max));
            }
        }
    }
    winners_score(decks)
}

fn recursive_combat(mut decks: Input, l: usize) -> Input {
    let mut seen: HashSet<Input> = HashSet::new();
    while !decks.iter().any(|d| d.len() == 0) {
        if !seen.insert(decks.clone()) {
            // report winner using fake decks
            return vec![vec![1].into_iter().collect(), VecDeque::new()];
        }
        let cards: Vec<_> = decks.iter_mut().map(|d| d.pop_front().unwrap()).collect();
        let winner = if decks.iter().zip(cards.iter()).all(|(d, &c)| d.len() >= c) {
            let sub_decks = decks.iter()
                .zip(cards.iter())
                .map(|(d, &c)| d.iter().cloned().take(c).collect())
                .collect();
            recursive_combat(sub_decks, l + 1)
                .into_iter()
                .find_position(|d| d.len() != 0)
                .unwrap().0
        } else {
            let max = cards.iter().max().unwrap();
            cards.iter()
                .find_position(|&c| c == max)
                .unwrap().0
        };
        decks[winner].push_back(cards[winner]);
        decks[winner].extend(cards.iter().enumerate().filter_map(|(i, c)| (i != winner).then_some(c)));
    }
    decks
}

fn part_2(input: &Input) -> usize {
    let decks = recursive_combat(input.clone(), 0);
    winners_score(decks)
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
        Player 1:\n\
        9\n\
        2\n\
        6\n\
        3\n\
        1\n\
        \n\
        Player 2:\n\
        5\n\
        8\n\
        4\n\
        7\n\
        10\n\
        ";

    test!(part_1() == 306);
    test!(part_2() == 291);
    bench!(part_1() == 31308);
    // bench!(part_2() == 33647); // takes almost 1s
}
