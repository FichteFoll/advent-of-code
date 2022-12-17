#![feature(custom_test_frameworks)]
#![feature(generic_arg_infer)]
#![feature(once_cell)]
#![feature(test)]

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;

const DAY: usize = 17;

type Parsed<'a> = &'a str;
type Grid = HashSet<Point<2>>;

const WIDTH: i32 = 7;

main!();

fn parse_input(input: &str) -> Parsed {
    input.trim()
}

fn part_1(parsed: &Parsed) -> i32 {
    let mut grid = Default::default();
    make_blocks_fall(&mut grid, parsed, 2022)
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn make_blocks_fall(grid: &mut Grid, steam: &Parsed, times: usize) -> i32 {
    let mut steam_iter = steam.bytes().cycle();
    let mut block_iter = Block::all().into_iter().cycle();
    let mut highest_point = 1; // floor starts at y == 1 (and is build into the negatives)
    for _ in 1..=times {
        let mut block = block_iter.next().unwrap().points();
        move_points(&mut block, Point([2, highest_point - 4]));
        loop {
            // Captures `grid`.
            let is_free = |pt: &Point<2>| 0 <= pt.x() && pt.x() < WIDTH && pt.y() <= 0 && !grid.contains(pt);

            // Move horizontally
            let steam_pt = match steam_iter.next().unwrap() {
                b'<' => Point::<2>::W,
                b'>' => Point::<2>::E,
                _ => unreachable!(),
            };
            if let Some(new_block) = move_points_if(&block, steam_pt, is_free) {
                block = new_block;
            }

            // Move vertically
            if let Some(new_block) = move_points_if(&block, Point::<2>::S, is_free) {
                block = new_block;
            } else {
                let block_max = block.iter().map(|pt| pt.y()).min().unwrap();
                highest_point = highest_point.min(block_max);
                grid.extend(block);
                break;
            }
        }
    }
    -highest_point + 1
}

#[derive(Clone, Copy, Debug)]
enum Block {
    Minus,
    Plus,
    L,
    I,
    Square,
}

impl Block {
    const fn all() -> [Self; 5] {
        use Block::*;
        [Minus, Plus, L, I, Square]
    }

    fn points(&self) -> Vec<Point<2>> {
        use Block::*;
        // Bottom left of the block is 0,0; grid expands to bottom right (↓→).
        match self {
            Minus => vec![Point([0, 0]), Point([1, 0]), Point([2, 0]), Point([3, 0])],
            Plus => vec![
                Point([1, 0]),
                Point([0, -1]),
                // Skipping the middle because it will never be relevant.
                // Point([1, -1]),
                Point([2, -1]),
                Point([1, -2]),
            ],
            L => vec![
                Point([0, 0]),
                Point([1, 0]),
                Point([2, 0]),
                Point([2, -1]),
                Point([2, -2]),
            ],
            I => vec![
                Point([0, 0]),
                Point([0, -1]),
                Point([0, -2]),
                Point([0, -3]),
            ],
            Square => vec![Point([0, 0]), Point([1, 0]), Point([0, -1]), Point([1, -1])],
        }
    }
}

fn move_points<const N: usize>(points: &mut [Point<N>], by: Point<N>) {
    for pt in points.iter_mut() {
        *pt += by;
    }
}

#[must_use]
fn move_points_if<const N: usize, F>(
    points: &Vec<Point<N>>,
    by: Point<N>,
    check: F,
) -> Option<Vec<Point<N>>>
where
    F: Fn(&Point<N>) -> bool,
{
    let mut new_points = points.clone();
    for pt in new_points.iter_mut() {
        *pt += by;
        if !check(pt) {
            return None;
        }
    }
    Some(new_points)
}

#[allow(unused)]
fn print_grid(grid: &Grid, falling: Option<&Vec<Point<2>>>) {
    let highest_in_grid = grid.iter().map(|pt| pt.y()).min().unwrap_or(0);
    let highest_in_block = falling
        .iter()
        .flat_map(|v| v.iter().map(|pt| pt.y()))
        .min()
        .unwrap_or(0);

    let start = highest_in_block.min(highest_in_grid);
    println!("");
    for y in start..=0 {
        for x in 0..WIDTH {
            let pt = Point([x, y]);
            if grid.contains(&pt) {
                print!("#");
            } else if falling.map(|b| b.contains(&pt)).unwrap_or(false) {
                print!("@");
            } else {
                print!(".");
            }
        }
        println!("");
    }
    println!("-------");
}

#[cfg(test)]
mod tests {
    use std::cell::LazyCell;

    use super::*;
    extern crate test;
    use test_case::test_case;

    const TEST_INPUT: &str = "\
        >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n\
        ";

    test!(part_1() == 3068);
    // test!(part_2() == 0);
    bench_parse!(str::len, 10091);
    bench!(part_1() == 3181);
    // bench!(part_2() == 0);

    /// 0123456
    // |....SS.| -8
    // |....SS.| -7
    // |....I..| -6
    // |..L.I..| -5
    // |..L.I..| -4
    // |LLL+I..| -3
    // |..+++..| -2
    // |...+...| -1
    // |..----.|  0
    // +-------+
    const BLOCKS: LazyCell<Vec<Vec<Point<2>>>> = LazyCell::new(|| {
        vec![
            block_at(Block::Minus, Point([2, 0])),
            block_at(Block::Plus, Point([2, -1])),
            block_at(Block::L, Point([0, -3])),
            block_at(Block::I, Point([4, -3])),
            block_at(Block::Square, Point([4, -7])),
        ]
    });

    fn block_at(block: Block, at: Point<2>) -> Vec<Point<2>> {
        let mut points = block.points();
        move_points(&mut points, at);
        points
    }

    #[test_case(1 => 1)]
    #[test_case(2 => 4)]
    #[test_case(3 => 6)]
    #[test_case(4 => 7)]
    #[test_case(5 => 9)]
    fn make_block_fall_first_n(n: usize) -> i32 {
        let parsed = parse_input(TEST_INPUT);
        let mut grid = Default::default();
        let expected = BLOCKS.iter().take(n).flatten().cloned().collect();
        let highest = make_blocks_fall(&mut grid, &parsed, n);
        assert_eq!(grid, expected);
        highest
    }
}
