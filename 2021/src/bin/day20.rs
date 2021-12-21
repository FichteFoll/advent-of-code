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
        let code: Vec<_> = blocks.0.lines()
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

fn part_1(parsed: &Parsed) -> usize {
    solve(parsed, 2)
}

fn part_2(parsed: &Parsed) -> usize {
    solve(parsed, 50)
}

fn solve((code, image): &Parsed, times: usize) -> usize {
    assert!(!code[0] || !code[511], "the output would be infinity");
    let mut image = image.clone();
    let mut carry = false; // state of everything not pictured
    for _ in 0..times {
        (image, carry) = enhance(code, &image, carry);
    }
    assert!(!carry, "output is infinity (impossible, I hope)");
    image.len()
}

fn enhance(code: &Code, image: &Image, carry: bool) -> (Image, bool) {
    let (x_range, y_range) = min_max(image, 1);
    let get_carry = |pt: &Point<2>| {
        let res = match &pt.coord {
            [x, _] if x <= x_range.start() || x >= x_range.end() => Some(carry),
            [_, y] if y <= y_range.start() || y >= y_range.end() => Some(carry),
            _ => None,
        };
        res
    };
    let new_image = iproduct!(x_range.clone(), y_range.clone())
        .filter_map(|pair| {
            let center: Point<2> = pair.into();
            let index = convolution_pt_matrix(&center)
                .map(|pt| get_carry(&pt).unwrap_or_else(|| image.contains(&pt)))
                .fold(0, |acc, b| (acc << 1) + b as usize);
            code[index].then_some(center)
        })
        .collect();
    (new_image, carry ^ code[0])
}

fn min_max(image: &Image, padding: i32) -> (RangeInclusive<i32>, RangeInclusive<i32>) {
    let (x_min, x_max, y_min, y_max) = image.iter().fold(
        (i32::MAX, i32::MIN, i32::MAX, i32::MIN),
        |(x_min, x_max, y_min, y_max), pt|
            (x_min.min(pt.x()), x_max.max(pt.x()), y_min.min(pt.y()), y_max.max(pt.y()))
    );
    (x_min - padding..=x_max + padding, y_min - padding..=y_max + padding)
}

fn convolution_pt_matrix(pt: &Point<2>) -> impl Iterator<Item=Point<2>> {
    let pt_ref = *pt;
    iproduct!(-1..=1, -1..=1)
        .map(move |(y_offset, x_offset)| { // iproduct exhausts the latter iter first
            let mut new_pt = pt_ref;
            *new_pt.x_mut() += x_offset;
            *new_pt.y_mut() += y_offset;
            new_pt
        })
}

#[cfg(test)]
fn render(image: &Image) -> String {
    let (x_range, y_range) = min_max(image, 0);
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

    const TEST_INPUT: &str = "\
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

    test!(part_2() == 3351);
    bench_parse!(|x: &Parsed| (x.0.len(), x.1.len()), (512, 4947));
    bench!(part_1() == 5682);
    bench!(part_2() == 17628);

    #[test]
    fn test_part_1() {
        let (code, mut image) = parse_input(TEST_INPUT);
        assert_eq!(image.len(), 10);
        let mut carry = false;
        (image, carry) = enhance(&code, &image, carry);
        assert_eq!((image.len(), carry), (24, false));
        (image, carry) = enhance(&code, &image, carry);
        assert_eq!((image.len(), carry), (35, false));
        let rendered = render(&image);
        println!("{rendered}\n");
        let expected_render = "\n\
            .......#.\n\
            .#..#.#..\n\
            #.#...###\n\
            #...##.#.\n\
            #.....#.#\n\
            .#.#####.\n\
            ..#.#####\n\
            ...##.##.\n\
            ....###..";
        assert_eq!(rendered, expected_render);
    }

    #[test]
    fn test_part_1_case_2() {
        let (mut code, mut image) = parse_input(TEST_INPUT);
        assert_eq!(code.len(), 512);
        code.swap(0, 511); // test the case where the carry flips
        assert_eq!(image.len(), 10);
        let mut carry = false;
        println!("{}\n{carry}", render(&image));

        (image, carry) = enhance(&code, &image, carry);

        let rendered_1 = render(&image);
        println!("{rendered_1}\n{carry}");
        let expected_1 = "\n\
            .##.###\n\
            #..#.##\n\
            ##.#..#\n\
            ####..#\n\
            .#..##.\n\
            ####..#\n\
            ##.#.#.";
        assert_eq!((image.len(), carry), (30, true));
        assert_eq!(rendered_1, expected_1);

        (image, carry) = enhance(&code, &image, carry);
        // [-1,-1]
        // ### #.# ##. => .
        assert!(!image.contains(&Point::new([-1, -1])));
        // [-2,-2]
        // ### ### ##. => .
        assert!(!image.contains(&Point::new([-2, -2])));

        let rendered_2 = render(&image);
        println!("{rendered_2}\n{carry}");
        let expected_2 = "\n\
            .....#...\n\
            ###......\n\
            .#..###..\n\
            ......#..\n\
            .#..###.#\n\
            #..#####.\n\
            ...#....#\n\
            ..#.....#";
        assert_eq!((image.len(), carry), (24, false));
        assert_eq!(rendered_2, expected_2);
    }

    #[test]
    fn test_convolution_matrix() {
        let (_, image) = parse_input(TEST_INPUT);
        let matrix: Vec<_> = convolution_pt_matrix(&Point::new([2, 2]))
            .map(|pt| image.contains(&pt))
            .collect();
        assert_eq!(
            matrix,
            vec![false, false, false,
                  true, false, false,
                 false,  true, false]
        );
    }
}
