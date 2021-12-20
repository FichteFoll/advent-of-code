#![feature(bool_to_option)]
#![feature(test)]

use std::ops::RangeInclusive;

use aoc2021::*;
use aoc2021::coord::Point;
use aoc2021::collections::HashSet;
use itertools::iproduct;
use parse::parse_input;

const DAY: usize = 20;

type Code = Vec<bool>;
type Image = HashSet<Point<2>>;
type Parsed = (Code, Image);

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let blocks = input.trim().split_once("\n\n").unwrap();
        let code = blocks.0.lines()
            .flat_map(|line| line.bytes().map(|b| b == b'#'))
            .collect();
        let image = blocks.1.lines()
            .enumerate()
            .flat_map(|(y, line)| line.bytes()
                .enumerate()
                .filter_map(move |(x, b)| (b == b'#').then(|| (x, y).into()))
            )
            .collect();
        (code, image)
    }
}

fn part_1((code, image): &Parsed) -> usize {
    let mut image = image.clone();
    println!("{:?}\n", code);
    println!("{}", render(&image));
    for _ in 0..2 {
        image = enhance(code, &image);
        println!("{}", render(&image));
    }
    image.len()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn enhance(code: &Code, image: &Image) -> Image {
    let (x_range, y_range) = min_max(image);
    iproduct!(x_range, y_range)
        .filter_map(|pair| {
            let pt: Point<2> = pair.into();
            let index = convolution_matrix(image, &pt)
                .into_iter()
                .fold(0, |acc, b| (acc << 1) + b as usize);
            code[index].then_some(pt)
        })
        .collect()
}

fn min_max(image: &Image) -> (RangeInclusive<i32>, RangeInclusive<i32>) {
    let (x_min, x_max, y_min, y_max) = image.iter().fold(
        (i32::MAX, i32::MIN, i32::MAX, i32::MIN),
        |(x_min, x_max, y_min, y_max), pt|
            (x_min.min(pt.x()), x_max.max(pt.x()), y_min.min(pt.y()), y_max.max(pt.y()))
    );
    // with a padding of 1
    (x_min - 1..=x_max + 1, y_min - 1..=y_max + 1)
}

fn convolution_matrix(image: &Image, pt: &Point<2>) -> Vec<bool> {
    iproduct!(-1..=1, -1..=1)
        .map(|(y_offset, x_offset)| { // iproduct exhausts the latter iter first
            let mut new_pt = *pt;
            *new_pt.x_mut() += x_offset;
            *new_pt.y_mut() += y_offset;
            image.contains(&new_pt)
        })
        .collect()
}

// #[cfg(test)]
fn render(image: &Image) -> String {
    let (x_range, y_range) = min_max(image);
    let mut buf = Vec::with_capacity(
        ((x_range.end() - x_range.start() + 1) * (y_range.end() - y_range.start())) as usize
    );
    for y in y_range {
        buf.push('\n'); // newline first for formatting reasons
        for x in x_range.clone() {
            buf.push(match image.contains(&Point::new([x, y])) {
                true => '#',
                false => '.',
            });
        }
    }
    buf.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "
        ..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\n\
        #..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\n\
        .######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\n\
        .#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\n\
        .#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\n\
        ...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\n\
        ..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\
        \n\
        #..#.\n\
        #....\n\
        ##..#\n\
        ..#..\n\
        ..###\n\
        ";

    #[test]
    fn test_part_1() {
        let (code, mut image) = parse_input(TEST_INPUT);
        assert_eq!(image.len(), 10);
        image = enhance(&code, &image);
        assert_eq!(image.len(), 24);
        image = enhance(&code, &image);
        assert_eq!(image.len(), 35);
        let string = render(&image);
        println!("{string}\n");
        let expected_render = "\n\
            ...........\n\
            ........#..\n\
            ..#..#.#...\n\
            .#.#...###.\n\
            .#...##.#..\n\
            .#.....#.#.\n\
            ..#.#####..\n\
            ...#.#####.\n\
            ....##.##..\n\
            .....###...\n\
            ...........";
        assert_eq!(string, expected_render);
    }

    // test!(part_2() == 0);
    bench_parse!(|x: &Parsed| (x.0.len(), x.1.len()), (512, 4947));
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);

    #[test]
    fn test_convolution_matrix() {
        let (_, image) = parse_input(TEST_INPUT);
        assert_eq!(
            convolution_matrix(&image, &Point::new([2, 2])),
            vec![false, false, false,
                  true, false, false,
                 false,  true, false]
        );
    }
}
