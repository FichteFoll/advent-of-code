#![feature(test)]

use std::collections::VecDeque;

use aoc2024::*;
use collections::HashSet;

const DAY: usize = 9;

type Parsed = Vec<Block>;

main!();

// TODO store Empty block size in previous File block?
#[derive(Clone, Copy, Debug)]
enum Block {
    File { size: u8, id: u64 },
    Empty { size: u8 },
}
use Block::*;

impl Block {
    fn hash_step(&self, pointer: &mut u64, hash: &mut u64) {
        match *self {
            File { size, id } => {
                *hash += ((0..size as u64).sum::<u64>() + *pointer * size as u64) * id;
                *pointer += size as u64;
            }
            Empty { size } => {
                *pointer += size as u64;
            }
        };
    }
}

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
        if let Empty { size } = b {
            fill_space(&mut deq, size);
        } else {
            b.hash_step(&mut pointer, &mut hash)
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
        Some(diff) => {
            deq.push_front(Empty { size: diff });
            deq.push_front(File { size, id });
        }
    }
}

fn part_2(parsed: &Parsed) -> u64 {
    let mut deq: VecDeque<_> = parsed.iter().cloned().collect();
    defrag(&mut deq);
    calc_hash(&deq)
}

fn defrag(deq: &mut VecDeque<Block>) {
    let mut seen: HashSet<u64> = Default::default();
    let mut i_r = deq.len();
    while i_r > 1 {
        i_r -= 1;
        let File { size: size_r, id } = deq[i_r] else {
            continue;
        };
        if !seen.insert(id) {
            continue;
        }
        for i_l in 0..i_r {
            let b_l = deq[i_l];
            match b_l {
                Empty { size: size_l } if size_l == size_r => {
                    deq.swap(i_l, i_r);
                    break;
                }
                Empty { size: size_l } if size_l > size_r => {
                    deq[i_l] = Empty {
                        size: size_l - size_r,
                    };
                    deq[i_r] = Empty { size: size_r };
                    deq.insert(i_l, File { size: size_r, id });
                    break;
                }
                _ => (),
            }
        }
    }
}

fn calc_hash(deq: &VecDeque<Block>) -> u64 {
    let mut hash = 0u64;
    let mut pointer = 0u64;
    for b in deq {
        b.hash_step(&mut pointer, &mut hash);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "2333133121414131402";

    test!(part_1() == 1928);
    test!(part_2() == 2858);
    bench_parse!(Vec::len, 19999);
    bench!(part_1() == 6360094256423);
    bench!(part_2() == 6379677752410);

    #[test]
    fn test_part_2_special_case() {
        // 0..111....22222
        // no defragmentation possible
        // 3+4+5+2*(10+11+12+13+14) = 132
        let parsed = parse_input("12345");
        assert_eq!(part_2(&parsed), 132);
    }

    #[test]
    fn test_part_2_special_case_2() {
        // 00.........111.2223
        // =>
        // 003222111..........
        // 3*2+2*(3+4+5)+1*(6+7+8) = 51
        let parsed = parse_input("2931301");
        assert_eq!(part_2(&parsed), 51);
    }
}
