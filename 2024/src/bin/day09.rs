#![feature(test)]

use std::collections::VecDeque;

use aoc2024::*;

const DAY: usize = 9;

type Parsed = Vec<Block>;

main!();

#[derive(Clone, Copy, Debug)]
enum Block {
    File { size: u8, id: u64 },
    Empty { size: u8 },
}
use Block::*;

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .bytes()
        .map(|b| b - b'0')
        .enumerate()
        .map(|(i, f)| match i % 2 == 0 {
            true => File {
                size: f,
                id: i as u64 / 2,
            },
            false => Empty { size: f },
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> u64 {
    let mut deq: VecDeque<_> = parsed.iter().cloned().collect();
    let mut hash = 0u64;
    let mut pointer = 0u64;
    while let Some(b) = deq.pop_front() {
        match b {
            File { size, id } => {
                for i in 0..size as u64 {
                    hash += (pointer + i) * id;
                }
                pointer += size as u64;
            }
            Empty { size } => {
                fill_space(&mut deq, size);
            }
        }
    }
    hash
}

fn fill_space(deq: &mut VecDeque<Block>, space: u8) {
    let (size, id) = loop {
        match deq.pop_back() {
            None => return,
            Some(Empty { .. }) => continue,
            Some(File { size, id }) => break (size, id),
        }
    };
    match space.checked_sub(size) {
        None => {
            deq.push_front(File { size: space, id });
            deq.push_back(File {
                size: size - space,
                id,
            });
        }
        Some(0) => {
            deq.push_front(File { size, id });
        }
        _ => {
            deq.push_front(Empty { size: space - size });
            deq.push_front(File { size, id });
        }
    }
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "2333133121414131402";

    test!(part_1() == 1928);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 19999);
    bench!(part_1() == 6360094256423);
    // bench!(part_2() == 0);
}
