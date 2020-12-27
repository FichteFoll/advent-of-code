#![feature(test, linked_list_cursors)]

use std::collections::{LinkedList, linked_list::CursorMut};

use aoc2020::*;

use itertools::Itertools;

const DAY: usize = 23;
type Input = LinkedList<u32>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .bytes()
        .map(|b| (b - b'0') as u32)
        .collect()
}

fn move_curser_to<T: Eq>(cursor: &mut CursorMut<T>, mut target: T) {
    while cursor.current() != Some(&mut target) {
        cursor.move_next();
    }
}

fn rotate_to<T: Eq>(list: &mut LinkedList<T>, target: T) {
    let mut rest = {
        let mut cursor = list.cursor_front_mut();
        move_curser_to(&mut cursor, target);
        cursor.remove_current();
        cursor.split_before()
    };
    list.append(&mut rest);
}


fn play_rounds(cups: &mut Input, rounds: usize) {
    let max = cups.len() as u32;
    for r in 0..rounds {
        if r % 10_000 == 0 {
            println!("round {}", r);
        }
        let current = cups.pop_front().unwrap();
        {
            let mut cursor = cups.cursor_front_mut();
            let to_sort: Vec<_> = (0..).filter_map(|_| cursor.remove_current_as_list()).take(3).collect();
            for i in 1.. {
                let target = ((current + max) - i - 1) % max + 1; // wrapping in unsigned integer
                if to_sort.iter().any(|node| node.contains(&target)) {
                    continue;
                } else {
                    move_curser_to(&mut cursor, target);
                    to_sort.into_iter().rev().for_each(|node| cursor.splice_after(node));
                    break;
                }
            }
        }
        cups.push_back(current);
    }
}

fn part_1(input: &Input, rounds: usize) -> String {
    let mut cups = input.clone();
    play_rounds(&mut cups, rounds);
    rotate_to(&mut cups, 1);
    cups.into_iter()
        .map(|n| format!("{}", n))
        .collect_vec()
        .join("")
}

fn part_2(input: &Input) -> u64 {
    let mut cups = input.clone();
    let max = *cups.iter().max().unwrap();
    cups.extend(max + 1..=1_000_000);
    play_rounds(&mut cups, 10_000);
    // play_rounds(&mut cups, 1_000_000);
    rotate_to(&mut cups, 1);
    cups.into_iter()
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
