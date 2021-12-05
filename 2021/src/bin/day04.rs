#![feature(test)]
#![feature(bool_to_option)]

use core::panic;
use std::ops::BitOr;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 4;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn part_1(parsed: &Parsed) -> usize {
    let mut bingo = parsed.to_owned();
    for n in bingo.draws.iter() {
        for card in bingo.cards.iter_mut() {
            card.mark(*n);
        }

        if let Some(winning_card) = bingo.cards.iter().find(|c| c.is_winning()) {
            return winning_card.points() * n;
        }
    }
    panic!("No board won");
}

fn part_2(parsed: &Parsed) -> usize {
    let mut bingo = parsed.to_owned();
    for n in bingo.draws.iter() {
        for card in bingo.cards.iter_mut() {
            card.mark(*n);
        }

        if bingo.cards.len() == 1 && bingo.cards[0].is_winning() {
            return bingo.cards[0].points() * n;
        }
        bingo.cards.retain(|card| !card.is_winning());
    }
    panic!("No board or multiple boards won");
}

#[derive(Clone,Debug)]
pub struct Parsed {
    draws: Vec<usize>,
    cards: Vec<Card>,
}

type Mask = u32; // supports up to 5x5

#[derive(Clone,Debug)]
struct Card {
    fields: Vec<usize>,
    marked: Mask,
    winning_masks: Vec<Mask>, // computed in ::new
}

mod parse {
    use std::num::ParseIntError;
    use std::str::FromStr;

    use custom_error::custom_error;

    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.trim().parse().unwrap()
    }

    custom_error!{pub ParseError
        BadInt {source: ParseIntError} = "Unable to parse integer",
        NoDraws = "Couldn't find line with draws",
    }

    impl FromStr for Parsed {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (draws_line, cards_lines) = s.split_once("\n\n").ok_or(ParseError::NoDraws)?;
            let draws = draws_line
                .split(',')
                .map(|x| x.parse())
                .collect::<Result<_, _>>()?;
            let cards = cards_lines
                .split("\n\n")
                .map(|x| x.parse())
                .collect::<Result<_, _>>()?;
            Ok(Parsed { draws, cards })
        }
    }

    impl FromStr for Card {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let fields: Vec<usize> = s.trim().split('\n')
                .flat_map(|line|
                    line.split_whitespace()
                        .map(|num| num.parse::<usize>())
                ).collect::<Result<_, _>>()?;
            Ok(Card::new(fields))
        }
    }
}

impl Card {
    fn new(fields: Vec<usize>) -> Self {
        let size = (fields.len() as f32).sqrt() as usize;
        assert_eq!(size * size, fields.len(), "Fields must form a square");
        assert!(size * size < Mask::BITS as usize, "Fields do not fit into mask");
        let winning_masks = Card::create_winning_masks(size);
        Card {
            fields,
            marked: 0,
            winning_masks,
        }
    }

    fn create_winning_masks(size: usize) -> Vec<Mask> {
        let mut masks = vec![];
        // horiz →
        for row in 0..size {
            masks.push(((2 as Mask).pow(size as u32) - 1) << (row * size));
        }
        // vert ↓
        for col in 0..size {
            masks.push((0..size).map(|row| 1 << (col + row * size) as Mask).fold(0, Mask::bitor));
        }
        masks
    }

    fn mark(&mut self, n: usize) {
        let i_opt = self.fields.iter()
            .enumerate()
            .find(|(_, &val)| val == n);
        if let Some((i, _)) = i_opt {
            self.marked |= Card::mark_mask(i);
        }
    }

    fn mark_mask(i: usize) -> Mask {
        1 << i as Mask
    }

    fn is_winning(&self) -> bool {
        let winner = self.winning_masks.iter()
            .find(|&&mask| mask & self.marked == mask);

        winner.is_some()
    }

    fn points(&self) -> usize {
        self.fields.iter()
            .enumerate()
            .filter_map(|(i, &val)| (self.marked & Card::mark_mask(i) == 0).then_some(val))
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
        \n\
        22 13 17 11  0\n\
         8  2 23  4 24\n\
        21  9 14 16  7\n\
         6 10  3 18  5\n\
         1 12 20 15 19\n\
        \n\
         3 15  0  2 22\n\
         9 18 13 17  5\n\
        19  8  7 25 23\n\
        20 11 10 24  4\n\
        14 21 16 12  6\n\
        \n\
        14 21 17 24  4\n\
        10 16 15  9 19\n\
        18  8 23 26 20\n\
        22 11 13  6  5\n\
         2  0 12  3  7\n\
        ";

    test!(part_1() == 4512);
    test!(part_2() == 1924);
    bench_parse!(|p: &Parsed| (p.cards.len(), p.draws.len()), (100, 100));
    bench!(part_1() == 58374);
    bench!(part_2() == 11377);
}
