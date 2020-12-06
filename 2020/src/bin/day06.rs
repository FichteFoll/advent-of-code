#![feature(test, iterator_fold_self)]

use std::collections::HashSet;

#[macro_use]
extern crate lazy_static;

fn read_input() -> String {
    std::fs::read_to_string("input/day06.txt").expect("can’t read file")
}

type Input = Vec<Vec<HashSet<u8>>>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n\n")
        .map(|group|
            group.split("\n")
                .map(|line| line.bytes().collect())
                .collect()
        )
        .collect()
}

fn part_1(input: &Input) -> usize {
    input.iter()
        .map(|group| group.iter()
            .fold(HashSet::new(), |acc, p| acc.union(p).cloned().collect())
            .len()
        )
        .sum()
}

fn part_2(input: &Input) -> usize {
    input.iter()
        .map(|group| group.iter()
            .cloned()
            .fold_first(|acc, p| acc.intersection(&p).cloned().collect())
            .map(|s| s.len())
            .unwrap_or(0)
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

    lazy_static! {
        static ref EXAMPLE_INPUT_STR: &'static str = "\
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
    }

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
