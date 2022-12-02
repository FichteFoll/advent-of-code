#![feature(test)]


use aoc2022::*;
use parse::parse_input;

const DAY: usize = 2;

type Parsed = Vec<(Rps, Rps)>;

pub enum Rps {
    Rock,
    Paper,
    Scissors,
}

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;
    use std::str::FromStr;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .lines()
            .map(|line| {
                let (a, b) = line.split_once(' ').unwrap();
                (a.parse().unwrap(), b.parse().unwrap())
            })
            .collect()
    }

    impl FromStr for Rps {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "A" | "X" => Ok(Rps::Rock),
                "B" | "Y" => Ok(Rps::Paper),
                "C" | "Z" => Ok(Rps::Scissors),
                _ => Err("unknown character".to_owned())
            }
        }
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|(other, mine)| mine.score_against(other))
        .sum()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

impl Rps {
    fn score(&self) -> usize {
        match self {
            Rps::Rock => 1,
            Rps::Paper => 2,
            Rps::Scissors => 3,
        }
    }

    fn score_against(&self, other: &Rps) -> usize {
        // Add 3 first to prevent underflow
        //   win:  rock - scissors % 3 + 1 = 2
        //   draw: rock - rock     % 3 + 1 = 1
        //   loss: rock - paper    % 3 + 1 = 0
        self.score() + (4 + self.score() - other.score()) % 3 * 3
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        A Y\n\
        B X\n\
        C Z\n\
        ";

    test!(part_1() == 15);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 2500);
    bench!(part_1() == 11063);
    // bench!(part_2() == 0);

    #[test]
    fn test_score_against_draw() {
        assert_eq!(Rps::Rock.score_against(&Rps::Rock), 1 + 3);
        assert_eq!(Rps::Paper.score_against(&Rps::Paper), 2 + 3);
        assert_eq!(Rps::Scissors.score_against(&Rps::Scissors), 3 + 3);
    }

    #[test]
    fn test_score_against_loss() {
        assert_eq!(Rps::Rock.score_against(&Rps::Paper), 1);
        assert_eq!(Rps::Paper.score_against(&Rps::Scissors), 2);
        assert_eq!(Rps::Scissors.score_against(&Rps::Rock), 3);
    }

    #[test]
    fn test_score_against_win() {
        assert_eq!(Rps::Rock.score_against(&Rps::Scissors), 1 + 6);
        assert_eq!(Rps::Paper.score_against(&Rps::Rock), 2 + 6);
        assert_eq!(Rps::Scissors.score_against(&Rps::Paper), 3 + 6);
    }
}
