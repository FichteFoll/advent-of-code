#![feature(test, iter_map_while)]

use std::iter::successors;

fn read_input() -> String {
    std::fs::read_to_string("input/day03.txt").expect("can’t read file")
}

type Input = Vec<Vec<Tile>>;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tile {
    Empty,
    Tree,
}

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| {
            line.bytes()
                .map(|c| match c {
                    b'.' => Tile::Empty,
                    b'#' => Tile::Tree,
                    _ => panic!("unexpected input char")
                })
                .collect()
        })
        .collect()
}

fn walk_forest(input: &Input, steps: (usize, usize)) -> usize {
    successors(Some(0), |x| Some(x + steps.0))
        .zip((0..).step_by(steps.1))
        .map_while(|(x, y)| input.get(y).map(|line| line[x % line.len()]))
        .filter(|s| s == &Tile::Tree)
        .count()
}

fn part_1(input: &Input) -> usize {
    walk_forest(input, (3, 1))
}

fn part_2(input: &Input) -> usize {
    [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|&steps| walk_forest(input, steps))
        .product()
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

    const EXAMPLE_INPUT_STR: &str = "\
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
