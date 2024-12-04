#![feature(test)]

use aoc2024::point::Point;
use aoc2024::*;
use itertools::{iproduct, Itertools};

const DAY: usize = 4;

type Parsed = Vec<Vec<u8>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(|line| line.bytes().collect()).collect()
}

const XMAS: [u8; 4] = ['X' as u8, 'M' as u8, 'A' as u8, 'S' as u8];

fn part_1(grid: &Parsed) -> usize {
    let h = grid.len() as i32;
    let w = grid[0].len() as i32;
    let y_range = 0..h;
    let x_range = 0..w;

    let initial_pts = iproduct!(x_range.clone(), y_range.clone())
        .map(Point::<2>::from)
        .flat_map(|p| {
            Point::<2>::ZERO
                .neighbors()
                .into_iter()
                .map(move |dir| (p, dir))
        })
        .collect_vec();
    XMAS.iter()
        .enumerate()
        .fold(initial_pts, |pts, (i, c)| {
            pts.into_iter()
                .filter(|(origin, dir)| {
                    let p = origin + &(dir * i as i32);
                    x_range.contains(&p.x())
                        && y_range.contains(&p.y())
                        && grid[p.y() as usize][p.x() as usize] == *c
                })
                .collect_vec()
        })
        .len()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        MMMSXXMASM\n\
        MSAMXMSMSA\n\
        AMXSXMAAMM\n\
        MSAMASMSMX\n\
        XMASAMXAMM\n\
        XXAMMXXAMA\n\
        SMSMSASXSS\n\
        SAXAMASAAA\n\
        MAMMMXMMMM\n\
        MXMXAXMASX\n\
        ";

    test!(part_1() == 18);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| (p.len(), p[0].len()), (140, 140));
    bench!(part_1() == 2644);
    // bench!(part_2() == 0);

    #[test]
    fn test_p1_single_word() {
        assert_eq!(part_1(&parse_input("XMAS")), 1);
    }

    #[test]
    fn test_p1_single_word_reverse() {
        assert_eq!(part_1(&parse_input("SAMX")), 1);
    }
}
