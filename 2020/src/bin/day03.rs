#![feature(test, iter_map_while)]

use std::iter::successors;

#[macro_use]
extern crate lazy_static;

fn read_input() -> String {
    std::fs::read_to_string("input/day03.txt").expect("can’t read file")
}

type Input = Vec<Vec<Square>>;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Square {
    Empty,
    Tree,
}

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '.' => Square::Empty,
                    '#' => Square::Tree,
                    _ => panic!("unexpected input char")
                })
                .collect()
        })
        .collect()
}

fn walk_forest(input: &Input, by_x: usize, by_y: usize) -> usize {
    successors(Some((0, 0)), |(x, y)| Some((x + by_x, y + by_y)))
        .map_while(|(x, y)| input.get(y).map(|line| line[x % line.len()]))
        .filter(|s| s == &Square::Tree)
        .count()
}

fn part_1(input: &Input) -> usize {
    walk_forest(input, 3, 1)
}

fn part_2(input: &Input) -> usize {
    walk_forest(input, 1, 1)
        * walk_forest(input, 3, 1)
        * walk_forest(input, 5, 1)
        * walk_forest(input, 7, 1)
        * walk_forest(input, 1, 2)
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
            ..##.......\n\
            #...#...#..\n\
            .#....#..#.\n\
            ..#.#...#.#\n\
            .#...##..#.\n\
            ..#.##.....\n\
            .#.#.#....#\n\
            .#........#\n\
            #.##...#...\n\
            #...##....#\n\
            .#..#...#.#\n\
            ";
    }

    #[test]
    fn test_part_1() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_1(&input), 7);
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
            assert_eq!(part_1(&input), 167);
        });
    }

    #[test]
    fn test_part_2() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_2(&input), 336);
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 736527114);
        });
    }
}
