#![feature(test, iterator_fold_self)]

use std::ops::{BitAnd, BitOr};

fn read_input() -> String {
    std::fs::read_to_string("input/day06.txt").expect("can’t read file")
}

type Input = Vec<Vec<u32>>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n\n")
        .map(|group|
            group.split("\n")
                .map(|line| line.bytes()
                    .map(|b| 1 << (b - b'a'))
                    .fold(0, u32::bitor)
                )
                .collect()
        )
        .collect()
}

fn part_1(input: &Input) -> usize {
    input.iter()
        .map(|group| group.iter()
            .fold(0, u32::bitor)
            .count_ones()
            as usize
        )
        .sum()
}

fn part_2(input: &Input) -> usize {
    input.iter()
        .map(|group| group.iter()
            .fold(u32::MAX, u32::bitand)
            .count_ones()
            as usize
        )
        .sum()
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

    const EXAMPLE_INPUT_STR: &str = "\
        abc\n\
        \n\
        a\n\
        b\n\
        c\n\
        \n\
        ab\n\
        ac\n\
        \n\
        a\n\
        a\n\
        a\n\
        a\n\
        \n\
        b\n\
        ";

    #[test]
    fn test_part_1() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_1(&input), 11);
    }

    #[test]
    fn test_part_2() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_2(&input), 6);
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
            assert_eq!(part_1(&input), 6583);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 3290);
        });
    }
}
