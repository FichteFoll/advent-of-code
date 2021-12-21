#![feature(explicit_generic_args_with_impl_trait)]
#![feature(iter_advance_by)]
#![feature(test)]

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 21;

type Parsed = Vec<usize>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .lines()
            .map(|line| line.chars().last().unwrap().to_digit(10).unwrap() as usize)
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let mut die = (1..=100).cycle();
    let mut pos: Vec<_> = parsed.iter()
        .map(|&n| {
            let mut iter = (1..=10).cycle();
            iter.advance_by(n).unwrap();
            iter
        })
        .collect();
    let mut sum = vec![0; parsed.len()];
    for i in 0.. {
        let curr = i % 2;
        let result: usize = next::<usize, 3>(&mut die).unwrap().into_iter().sum();
        pos[curr].advance_by(result - 1).unwrap();
        sum[curr] += pos[curr].next().unwrap();
        if sum[curr] >= 1000 {
            return sum[(i + 1) % 2] * (i + 1) * 3;
        }
    };
    unreachable!();
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn next<T, const N: usize>(iter: &mut impl Iterator<Item=T>) -> Result<[T; N], usize>
where T: Default + Copy {
    let mut res = [T::default(); N];
    for i in 0..N {
        res[i] = iter.next().ok_or(i)?;
    }
    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Player 1 starting position: 4\n\
        Player 2 starting position: 8\n\
        ";

    test!(part_1() == 739785);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 2);
    bench!(part_1() == 598416);
    // bench!(part_2() == 0);
}
