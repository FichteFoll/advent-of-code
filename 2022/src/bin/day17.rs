#![feature(array_windows)]
#![feature(custom_test_frameworks)]
#![feature(once_cell)]
#![feature(test)]

use std::ops::RangeBounds;
use itertools::Itertools;
use itertools::Either::{self, *};

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;

const DAY: usize = 17;

type Parsed<'a> = &'a str;
type Grid = HashSet<Point<2>>;

const WIDTH: i32 = 7;
const PART1_COUNT: u64 = 2022;
const PART2_COUNT: u64 = 1_000_000_000_000;

main!();

fn parse_input(input: &str) -> Parsed {
    input.trim()
}

fn part_1(steam: &Parsed) -> u64 {
    make_blocks_fall_smart(steam, PART1_COUNT)
}

fn part_2(steam: &Parsed) -> u64 {
    make_blocks_fall_smart(steam, PART2_COUNT)
}

fn make_blocks_fall_smart(steam: &Parsed, times: u64) -> u64 {
    // The cave cycles at *some point* and *with some size*,
    // allowing us to triangulate the total height for as many iterations as we want
    // so long that we find these two figures.
    // For the test input, these are 25 for the prefix and 35 for the cycle.
    // The cycle size must be a multiple of 5.
    let (prefix_size, cycle_size) = match make_blocks_fall(&mut Default::default(), steam, times) {
        Left(result) => return result,
        Right(tpl) => tpl,
    };
    // let prefix_size = 46; // 25
    // let cycle_size = 35;
    let to_estimate = times - prefix_size;
    let (mul, suffix_size) = (to_estimate / cycle_size, to_estimate % cycle_size);
    dbg!(prefix_size, cycle_size, to_estimate, mul, suffix_size);

    let prefix_height = make_blocks_fall(&mut Default::default(), steam, prefix_size).unwrap_left();
    let after_cycle_height = make_blocks_fall(&mut Default::default(), steam, prefix_size + cycle_size).unwrap_left();
    let cycle_height = after_cycle_height - prefix_height;

    let prefix_and_suffix_height =
        make_blocks_fall(&mut Default::default(), steam, prefix_size + suffix_size).unwrap_left();
    let suffix_height = prefix_and_suffix_height - prefix_height;

    dbg!(
        prefix_height,
        after_cycle_height,
        cycle_height,
        prefix_and_suffix_height,
        suffix_height
    );
    dbg!(prefix_size + mul * cycle_size + suffix_size);
    dbg!(prefix_height + mul * cycle_height + suffix_height)
}

fn make_blocks_fall(grid: &mut Grid, steam: &Parsed, times: u64) -> Either<u64, (u64, u64)> {
    let mut steam_iter = steam.bytes().cycle();
    let mut block_iter = Block::all().into_iter().cycle();
    let mut highest_point = 1; // floor starts at y == 1 (and is build into the negatives)
    let mut cycle_finder = CycleFinder::default();

    for i in 1..=times {
        let block_kind = block_iter.next().unwrap();
        let mut block = block_kind.points();
        move_points(&mut block, Point([2, highest_point - 4]));
        loop {
            // Captures `grid`.
            let is_free =
                |pt: &Point<2>| 0 <= pt.x() && pt.x() < WIDTH && pt.y() <= 0 && !grid.contains(pt);

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
                let height_diff = block_max - highest_point;
                if height_diff > 0 {
                    println!("block {i} {:?} fell through by {}", block_kind, height_diff);
                    if let Some(result) = cycle_finder.register((block_kind, height_diff), i) {
                        dbg!(&cycle_finder);
                        println!("{result:?}");
                        return Right(result); // take the shortcut!
                    }
                }
                highest_point = highest_point.min(block_max);
                grid.extend(block);
                break;
            }
        }
    }
    Left((-highest_point) as u64 + 1)
}

#[derive(Default, Debug)]
struct CycleFinder {
    map: HashMap<(Block, i32), Vec<u64>>,
}

impl CycleFinder {
    // require this many repetitions of a single fallthrough
    const THRESHOLD: usize = 25;

    #[must_use]
    fn register(&mut self, key: (Block, i32), i: u64) -> Option<(u64, u64)> {
        let vec = self.map.entry(key).or_default();
        vec.push(i);
        (vec.len() == Self::THRESHOLD).then(|| self.find_cycle())
    }

    fn find_cycle(&self) -> (u64, u64) {
        let to_consider: Vec<_> = self
            .map
            .values()
            .collect();

        let mut diff_map: HashMap<_, u8> = HashMap::default();
        let differences = to_consider
            .iter()
            .flat_map(|v| v.array_windows().map(|[a, b]| b - a));
        for diff in differences {
            *diff_map.entry(diff).or_default() += 1
        }
        dbg!(&diff_map);
        let cycle_size = diff_map.into_iter().max_by_key(|(_, c)| *c).unwrap().0;

        let prefix_size = self
            .map
            .values()
            .sorted_by_key(|v| v.len())
            .flat_map(|v| {
                v.iter().permutations(2)
                    .find_map(|p| (p[1] > p[0] && p[1] - p[0] == cycle_size).then_some(p[0] + 1))
            })
            .next()
            .unwrap();
        (prefix_size, cycle_size)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Block {
    Minus,
    Plus,
    L,
    I,
    Square,
}

impl Block {
    #[must_use]
    const fn all() -> [Self; 5] {
        use Block::*;
        [Minus, Plus, L, I, Square]
    }

    #[must_use]
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
fn print_grid(grid: &Grid, falling: Option<&Vec<Point<2>>>, range: impl RangeBounds<i32>) {
    let highest_in_grid = grid.iter().map(|pt| pt.y()).min().unwrap_or(0);
    let highest_in_block = falling
        .iter()
        .flat_map(|v| v.iter().map(|pt| pt.y()))
        .min()
        .unwrap_or(0);

    use std::ops::Bound::*;
    let start = {
        let range_bound = match range.start_bound() {
            Included(&x) => x,
            Excluded(&x) => x + 1,
            Unbounded => i32::MIN,
        };
        highest_in_block.min(highest_in_grid).max(range_bound)
    };
    let end = match range.end_bound() {
        Included(&x) => x,
        Excluded(&x) => x - 1,
        Unbounded => 0,
    };
    println!("");
    for y in start..=end {
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
    test!(part_2() == 1514285714288);
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
    fn make_block_fall_first_n(n: usize) -> u64 {
        let parsed = parse_input(TEST_INPUT);
        let mut grid = Default::default();
        let expected = BLOCKS.iter().take(n).flatten().cloned().collect();
        let highest = make_blocks_fall(&mut grid, &parsed, n as u64).unwrap_left();
        assert_eq!(grid, expected);
        highest
    }

    #[test]
    fn find_cycles() {
        let parsed = parse_input(TEST_INPUT);
        let mut grid = Default::default();
        // The cave cycles after 35 iterations for the test input
        // but it does so only after 25 initial blocks.
        let (prefix_size, cycle_size) = make_blocks_fall(&mut grid, &parsed, PART2_COUNT).unwrap_right();
        assert_eq!(cycle_size, 35);
        assert!(prefix_size >= 25); // any prefix higher than 25 works
    }
}
