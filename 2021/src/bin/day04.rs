#![feature(test)]
#![feature(bool_to_option)]

use core::panic;
use std::ops::BitOr;

use aoc2021::*;
use aoc2021::grid2d::*;
use parse::parse_input;

const DAY: usize = 04;


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
    grid: Grid2D<usize>,
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
            Ok(Parsed { draws: draws, cards: cards })
        }
    }

    impl FromStr for Card {
        type Err = ParseError;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let grid: Grid2D<usize> = s.trim().split('\n')
                .map(|line|
                    line.split(' ')
                        .filter(|s| s.len() > 0)
                        .map(|num| num.parse::<usize>())
                        .collect::<Result<Vec<_>, _>>()
                ).collect::<Result<_, _>>()?;
            Ok(Card::new(grid))
        }
    }
}

impl Card {
    fn new(grid: Grid2D<usize>) -> Self {
        let winning_masks = Card::create_winning_masks(&grid.size);
        Card {
            grid,
            marked: 0,
            winning_masks,
        }
    }

    fn create_winning_masks(size: &Size) -> Vec<Mask> {
        if size.0 != size.1 {
            panic!("grid must be a square");
        }
        let dim = size.0;
        let mut masks = vec![];
        // horiz →
        for row in 0..dim {
            masks.push(((2 as Mask).pow(dim as u32) - 1) << row * dim);
        }
        // vert ↓
        for col in 0..dim {
            masks.push(
                (0..dim).map(|row| 1 << (col + row * dim) as Mask).fold(0, Mask::bitor)
            );
        }
        // Diagonals don't actually count, but I implemented them anyway because I can't read.
        // The example would pass this after the number 2 in the third card.
        // // diag →↓
        // masks.push(
        //     (0..dim).map(|i| 1 << (i + i * dim) as Mask).fold(0, Mask::bitor)
        // );
        // // diag ←↓
        // masks.push(
        //     (0..dim).map(|i| 1 << ((dim - i - 1) + i * dim) as Mask).fold(0, Mask::bitor)
        // );
        masks
    }

    fn mark(&mut self, n: usize) {
        let point_opt = self.grid
            .iter_enumerate()
            .find(|(_, &val)| val == n);
        if let Some((pt, _)) = point_opt {
            self.marked |= self.mark_mask(&pt);
        }
    }

    fn mark_mask(&self, pt: &Point) -> Mask {
        1 << (self.grid.size.0 as Mask * pt.y as Mask + pt.x as Mask)
    }

    fn is_winning(&self) -> bool {
        let winner = self.winning_masks.iter()
            .find(|&&mask| mask & self.marked == mask);

        winner.is_some()
    }

    fn points(&self) -> usize {
        self.grid
            .iter_enumerate()
            .filter_map(|(pt, &val)| (self.marked & self.mark_mask(&pt) == 0).then_some(val))
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
