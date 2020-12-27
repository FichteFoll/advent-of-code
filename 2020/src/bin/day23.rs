#![feature(test)]

use std::collections::VecDeque;

use aoc2020::*;

use itertools::Itertools;

const DAY: usize = 23;
type Input = VecDeque<u32>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .bytes()
        .map(|b| (b - b'0') as u32)
        .collect()
}

fn play_rounds(cups: &mut Input, rounds: usize) {
    let max = *cups.iter().max().unwrap();
    for r in 0..rounds {
        if r % 10_000 == 0 {
            println!("round {}", r);
        }
        let current = cups.pop_front().unwrap();
        let to_sort: Vec<_> = (0..3).map(|_| cups.pop_front().unwrap()).collect();
        cups.push_front(current);
        let target_i = (1..).find_map(|i| {
            let target = ((current + max) - i - 1) % max + 1; // wrapping in unsigned integer
            if let Some((target_i, _)) = cups.iter().find_position(|&j| *j == target) {
                Some(target_i)
            } else {
                None
            }
        }).unwrap();
        cups.rotate_left(target_i + 1);
        cups.extend(to_sort.into_iter());
        cups.rotate_right(target_i + 3);
    }
}

fn part_1(input: &Input, rounds: usize) -> String {
    let mut cups = input.clone();
    play_rounds(&mut cups, rounds);
    let pos_1 = cups.iter().find_position(|&j| j == &1).unwrap().0;
    cups.rotate_left(pos_1);
    cups.into_iter()
        .skip(1)
        .map(|i| format!("{}", i))
        .collect::<Vec<_>>()
        .join("")
}

fn part_2(input: &Input) -> u64 {
    let mut cups = input.clone();
    let max = *cups.iter().max().unwrap();
    cups.extend(max + 1..=1_000_000);
    play_rounds(&mut cups, 1_000_000);
    let pos_1 = cups.iter().find_position(|&j| j == &1).unwrap().0;
    cups.rotate_left(pos_1);
    cups.into_iter()
        .skip(1)
        .take(2)
        .map(|x| x as u64)
        .product()
}

fn main() {
    let input_str = read_input!();
    let input = parse_input(&input_str);
    println!("Part 1: {}", part_1(&input, 100));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT_STR: &str = "389125467";

    test!(part_1(10) == "92658374");
    test!(part_2() == 149245887792);
    bench!(part_1(100) == "25468379");
    // bench!(part_2() == 0);
}
