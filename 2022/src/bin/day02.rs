#![feature(test)]
#![feature(array_chunks)]

use aoc2022::*;

const DAY: usize = 2;

type Parsed = Vec<(usize, usize)>;

main!();


fn parse_input(input: &str) -> Parsed {
    // requires trailing newline
    input.as_bytes().array_chunks::<4>()
        .map(|line| {
            ((line[0] - b'A') as usize, (line[2] as u8 - b'X') as usize)
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|&(other, mine)| score_against(mine, other))
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|&(other, outcome)| {
            let mine = second_for(other, outcome);
            outcome * 3 + mine + 1
        })
        .sum()
}

fn score_against(this: usize, other: usize) -> usize {
    // Add 3 first to prevent underflow and 1 for score
    //   win:  rock - scissors % 3 + 1 = 2
    //   draw: rock - rock     % 3 + 1 = 1
    //   loss: rock - paper    % 3 + 1 = 0
    this + 1 + (4 + this - other) % 3 * 3
}

fn second_for(other: usize, outcome: usize) -> usize {
    // rock & x => win: 0 + 2 + 2 % 3 = 1 => paper
    // scissors & x => draw: 2 + 1 + 2 % 3 = 2 => scissors
    // paper & x => loss: 1 + 0 + 2 % 3 = 0 => rock
    (other + outcome + 2) % 3
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
    test!(part_2() == 12);
    bench_parse!(Vec::len, 2500);
    bench!(part_1() == 11063);
    bench!(part_2() == 10349);

    const ROCK: usize = 0;
    const PAPER: usize = 1;
    const SCISSORS: usize = 2;

    const LOSS: usize = 0;
    const DRAW: usize = 1;
    const WIN: usize = 2;

    // very elaborative test suite following

    #[test]
    fn test_score_against_draw() {
        assert_eq!(score_against(ROCK, ROCK), 1 + 3);
        assert_eq!(score_against(PAPER, PAPER), 2 + 3);
        assert_eq!(score_against(SCISSORS, SCISSORS), 3 + 3);
    }

    #[test]
    fn test_score_against_loss() {
        assert_eq!(score_against(ROCK, PAPER), 1);
        assert_eq!(score_against(PAPER, SCISSORS), 2);
        assert_eq!(score_against(SCISSORS, ROCK), 3);
    }

    #[test]
    fn test_score_against_win() {
        assert_eq!(score_against(ROCK, SCISSORS), 1 + 6);
        assert_eq!(score_against(PAPER, ROCK), 2 + 6);
        assert_eq!(score_against(SCISSORS, PAPER), 3 + 6);
    }

    #[test]
    fn test_second_for_draw() {
        assert_eq!(second_for(ROCK, DRAW), ROCK);
        assert_eq!(second_for(PAPER, DRAW), PAPER);
        assert_eq!(second_for(SCISSORS, DRAW), SCISSORS);
    }

    #[test]
    fn test_second_for_loss() {
        assert_eq!(second_for(ROCK, LOSS), SCISSORS);
        assert_eq!(second_for(PAPER, LOSS), ROCK);
        assert_eq!(second_for(SCISSORS, LOSS), PAPER);
    }

    #[test]
    fn test_second_for_win() {
        assert_eq!(second_for(ROCK, WIN), PAPER);
        assert_eq!(second_for(PAPER, WIN), SCISSORS);
        assert_eq!(second_for(SCISSORS, WIN), ROCK);
    }
}
