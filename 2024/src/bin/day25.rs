#![feature(test)]

use aoc2024::*;
use itertools::iproduct;

const DAY: usize = 25;

type Parsed = (Vec<Schema>, Vec<Schema>); // (locks, keys)
type Schema = Vec<usize>;

fn main() {
    let input = read_file(DAY);
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    let col_count = input.bytes().position(|b| b == b'\n').unwrap();
    let blocks = input.trim().split("\n\n");
    let mut locks = vec![];
    let mut keys = vec![];
    for block in blocks {
        let bytes = block.as_bytes();
        let item: Vec<_> = (0..col_count)
            .map(|col| {
                (1..)
                    .map_while(|row| bytes.get(row * (col_count + 1) + col))
                    .filter(|&&b| b == bytes[0])
                    .count()
            })
            .collect();
        if bytes[0] == b'#' {
            locks.push(item);
        } else {
            keys.push(item);
        }
    }
    (locks, keys)
}

fn part_1((locks, keys): &Parsed) -> usize {
    iproduct!(locks, keys)
        .filter(|(lock, key)| lock.iter().zip(key.iter()).all(|(l, k)| l <= k))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        #####\n\
        .####\n\
        .####\n\
        .####\n\
        .#.#.\n\
        .#...\n\
        .....\n\
        \n\
        #####\n\
        ##.##\n\
        .#.##\n\
        ...##\n\
        ...#.\n\
        ...#.\n\
        .....\n\
        \n\
        .....\n\
        #....\n\
        #....\n\
        #...#\n\
        #.#.#\n\
        #.###\n\
        #####\n\
        \n\
        .....\n\
        .....\n\
        #.#..\n\
        ###..\n\
        ###.#\n\
        ###.#\n\
        #####\n\
        \n\
        .....\n\
        .....\n\
        .....\n\
        #....\n\
        #.#..\n\
        #.#.#\n\
        #####\n\
        ";

    test!(part_1() == 3);
    // bench_parse!(|p: &Parsed| (), 0);
    bench!(part_1() == 3155);
}
