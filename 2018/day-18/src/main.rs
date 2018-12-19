#![feature(test)]

extern crate test;

use std::cmp::min;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::str::FromStr;


#[derive(Clone, Eq, Hash, PartialEq)]
struct Outskirts {
    fields: Vec<Vec<char>>,
    size: usize
}

impl FromStr for Outskirts {
    type Err = Box<Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fields: Vec<Vec<char>> = s.lines()
            .map(|line| line.chars().collect())
            .collect();
        Ok(Outskirts { size: fields.len(), fields })
    }
}

impl Outskirts {
    fn step(&self) -> Self {
        let mut new = self.clone();
        for y in 0..self.size {
            for x in 0..self.size {
                let adjacent = self.adjacent_acres(x, y);
                new.fields[y][x] = match self.fields[y][x] {
                    '.' => match *adjacent.get(&'|').unwrap_or(&0) >= 3 {
                        true => '|',
                        false => '.',
                    },
                    '|' => match *adjacent.get(&'#').unwrap_or(&0) >= 3 {
                        true => '#',
                        false => '|',
                    },
                    '#' => match *adjacent.get(&'#').unwrap_or(&0) >= 1 && *adjacent.get(&'|').unwrap_or(&0) >= 1 {
                        true => '#',
                        false => '.',
                    },
                    _ => unimplemented!(),
                };
            }
        }
        new
    }

    fn adjacent_acres(&self, x: usize, y: usize) -> HashMap<char, u8> {
        // Determine adjacent acre coordinates
        let mut acres: HashMap<char, u8> = HashMap::new();
        let xrange = (if x == 0 {0} else {x - 1})..=(min(x + 1, self.size - 1));
        let yrange = (if y == 0 {0} else {y - 1})..=(min(y + 1, self.size - 1));
        for y2 in yrange {
            for x2 in xrange.clone() {
                if y == y2 && x == x2 { continue; }
                let c = self.fields[y2][x2];
                *acres.entry(c).or_insert(0) += 1;
            }
        }
        // iproduct!(yrange, xrange)
        //     .filter(|(y2, x2)| y != y2 || x != x2) )
        //     .map(|(y, x)| self.fields[y][x]) )
        //     .for_each(|c| *acres.entry(c).or_insert(0) += 1);
        acres
    }

    fn counts(&self) -> HashMap<char, usize> {
        let mut counts: HashMap<char, usize> = HashMap::new();
        for row in &self.fields {
            for c in row {
                *counts.entry(*c).or_insert(0) += 1;
            }
        }
        counts
    }

    fn to_string(&self) -> String {
        self.fields.iter()
            .map(|line| {
                let mut line_str: String = line.iter().collect();
                line_str.push('\n');
                line_str
            })
            .collect()
    }
}

impl fmt::Debug  for Outskirts {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

fn part_1(input_str: &str) -> usize {
    let mut outskirts: Outskirts = input_str.parse().unwrap();
    for _ in 0..10 {
        outskirts = outskirts.step();
    }
    let counts = outskirts.counts();
    println!("{:?}", counts);
    counts.get(&'|').unwrap_or(&0) * counts.get(&'#').unwrap_or(&0)
}

fn part_2(input_str: &str) -> usize {
    let mut outskirts: Outskirts = input_str.parse().unwrap();
    let mut cycle_map: HashMap<Outskirts, usize> = HashMap::new();
    const MAX_I: usize = 1_000_000_000;
    let mut i: usize = 0;
    while i < MAX_I {
        let new_outskirts = outskirts.step();
        match cycle_map.get(&new_outskirts) {
            Some(last_i) => {
                println!("cycle detected between {} and {}", last_i, i);
                let diff = i - last_i;
                let multiplier = (MAX_I - i) / diff;
                i += multiplier * diff;
                println!("jumping to {}", i);
                cycle_map.clear(); // no use in further cycle detection
            },
            None => {
                cycle_map.insert(new_outskirts.clone(), i);
            }
        }
        outskirts = new_outskirts;
        i += 1;
    }
    let counts = outskirts.counts();
    println!("{:?}", counts);
    counts.get(&'|').unwrap_or(&0) * counts.get(&'#').unwrap_or(&0)
}


fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    println!("Part 1: {:?}", part_1(&input_str));
    println!("Part 2: {:?}", part_2(&input_str));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn step() {
        let before: Outskirts = "\
            .#.#...|#.\n\
            .....#|##|\n\
            .|..|...#.\n\
            ..|#.....#\n\
            #.#|||#|#|\n\
            ...#.||...\n\
            .|....|...\n\
            ||...#|.#|\n\
            |.||||..|.\n\
            ...#.|..|."
            .parse().unwrap();
        let after: Outskirts = "\
            .......##.\n\
            ......|###\n\
            .|..|...#.\n\
            ..|#||...#\n\
            ..##||.|#|\n\
            ...#||||..\n\
            ||...|||..\n\
            |||||.||.|\n\
            ||||||||||\n\
            ....||..|."
            .parse().unwrap();

        let current = before.step();
        assert_eq!(current, after);
    }

    #[test]
    fn part_1_example() {
        let input_str = "\
            .#.#...|#.\n\
            .....#|##|\n\
            .|..|...#.\n\
            ..|#.....#\n\
            #.#|||#|#|\n\
            ...#.||...\n\
            .|....|...\n\
            ||...#|.#|\n\
            |.||||..|.\n\
            ...#.|..|.";

        assert_eq!(part_1(&input_str), 1147);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(part_1(&input_str), 536370);
        });
    }

    #[bench]
    #[ignore]
    fn bench_part_2(b: &mut Bencher) {
        // takes ~31s
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(part_2(&input_str), 190512);
        });
    }
}
