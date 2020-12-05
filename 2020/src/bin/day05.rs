#![feature(test, str_split_once, bool_to_option)]

use std::{io::Error, str::FromStr};

use itertools::Itertools;

fn read_input() -> String {
    std::fs::read_to_string("input/day05.txt").expect("can’t read file")
}

type Input<'a> = Vec<Seat>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| line.parse().unwrap())
        .collect()
}

#[derive(Debug)]
struct Seat {
    pub row: usize,
    pub column: usize,
}

impl Seat {
    fn id(&self) -> usize {
        self.row * 8 + self.column
    }
}

fn binary_partition(s: &str, one: u8) -> usize {
    s.bytes()
        .fold(0usize, |n, b| n << 1 | (b == one).then_some(1).unwrap_or(0))
}


impl FromStr for Seat {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rows, cols) = s.split_at(7);
        Ok(Self { row: binary_partition(rows, b'B'), column: binary_partition(cols, b'R') })
    }
}


fn part_1(input: &Input) -> usize {
    input.iter().map(Seat::id).max().unwrap()
}

fn part_2(input: &Input) -> usize {
    input.iter()
        .map(Seat::id)
        .sorted()
        .tuple_windows()
        .find_map(|(a, b)| (a + 1 != b).then_some(a + 1))
        .expect("no seat found")
}

fn main() {
    let input_str = read_input();
    let input = parse_input(&input_str);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    extern crate test;
    use test::Bencher;

    #[test]
    fn test_binary_partition() {
        assert_eq!(binary_partition("RRR", b'R'), 7);
        assert_eq!(binary_partition("BFFFBBF", b'B'), 70);
    }

    #[test]
    fn test_parse() {
        let tests: &[(&str, usize)] = &[
            ("FBFBBFFRLR", 357),
            ("BFFFBBFRRR", 567),
            ("FFFBBBFRRR", 119),
            ("BBFFBBFRLL", 820),
        ];
        tests.iter()
            .for_each(|(s, id)|
                assert_eq!(s.parse::<Seat>().unwrap().id(), *id)
            )
    }

    #[test]
    fn test_part_1() {
        let input_str = "\
            BFFFBBFRRR\n\
            FFFBBBFRRR\n\
            BBFFBBFRLL\n\
            ";
        let input = parse_input(input_str);
        assert_eq!(part_1(&input), 820);
    }

    #[bench]
    fn bench_parse(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let _ = parse_input(&input_str);
        });
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_1(&input), 858);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 557);
        });
    }
}
