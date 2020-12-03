#![feature(test, str_split_once)]

use std::str::FromStr;

use text_io::scan;

fn read_input() -> String {
    std::fs::read_to_string("input/day02.txt").expect("can’t read file")
}

type Input = Password;

fn parse_input(input_str: &str) -> Vec<Input> {
    input_str
        .trim()
        .split("\n")
        .map(|x| x.parse().unwrap())
        .collect()
}

#[derive(Debug)]
struct Password {
    pub start: usize,
    pub end: usize,
    pub c: char,
    pub password: String,
}

impl FromStr for Password {
    type Err = std::io::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end, c, password): (usize, usize, char, String);
        scan!(s.bytes() => "{}-{} {}: {}", start, end, c, password);
        Ok(Self { start, end, c, password })
    }
}

fn part_1(input: &[Input]) -> usize {
    input
        .iter()
        .filter(|p| {
            let count = p.password.chars().filter(|&c| c == p.c).count();
            p.start <= count && count <= p.end
        })
        .count()
}

fn part_2(input: &[Input]) -> usize {
    let char_at = |p: &Password, i: usize| p.password.chars().nth(i.saturating_sub(1)).unwrap();
    input
        .iter()
        .filter(|p| {
            (char_at(p, p.start) == p.c) ^ (char_at(p, p.end) == p.c)
        })
        .count()
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
    fn test_part_1() {
        let input_str = "\
            1-3 a: abcde\n\
            1-3 b: cdefg\n\
            2-9 c: ccccccccc";
        let input = parse_input(input_str);
        assert_eq!(part_1(&input), 2);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(part_1(&input), 519);
        });
    }

    #[test]
    fn test_part_2() {
        let input_str = "\
            1-3 a: abcde\n\
            1-3 b: cdefg\n\
            2-9 c: ccccccccc";
        let input = parse_input(input_str);
        println!("{:?}", input);
        assert_eq!(part_2(&input), 1);
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(part_2(&input), 708);
        });
    }
}
