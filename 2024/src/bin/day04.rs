#![feature(test)]

use aoc2024::point::Point;
use aoc2024::*;
use itertools::{Itertools, iproduct};

const DAY: usize = 4;

type Parsed = Vec<Vec<u8>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(|line| line.bytes().collect()).collect()
}

const XMAS: [u8; 4] = [b'X', b'M', b'A', b'S'];

fn part_1(grid: &Parsed) -> usize {
    let h = grid.len() as i32;
    let w = grid[0].len() as i32;
    let y_range = 0..h;
    let x_range = 0..w;

    // DFS instead of BFS would be much more efficient.
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

fn part_2(grid: &Parsed) -> usize {
    let h = grid.len();
    let w = grid[0].len();
    let y_range = 0..h;
    let x_range = 0..w;

    iproduct!(x_range, y_range)
        .filter(|(x, y)| grid[*y][*x] == b'A')
        .filter(|p| is_x_mas(p, grid).unwrap_or(false))
        .count()
}

// Since the input only contains the chars X, M, A, S,
// we can just xor the pairs
// to check if both required chars are present.
const LEGAL_HASH: u8 = b'M' ^ b'S';

fn is_x_mas((x, y): &(usize, usize), grid: &Parsed) -> Option<bool> {
    let hashes = [
        *grid.get(y.checked_sub(1)?)?.get(x.checked_sub(1)?)? ^ *grid.get(y + 1)?.get(x + 1)?,
        *grid.get(y + 1)?.get(x.checked_sub(1)?)? ^ *grid.get(y.checked_sub(1)?)?.get(x + 1)?,
    ];
    Some(hashes.into_iter().all(|h| h == LEGAL_HASH))
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
    test!(part_2() == 9);
    bench_parse!(|p: &Parsed| (p.len(), p[0].len()), (140, 140));
    bench!(part_1() == 2644);
    bench!(part_2() == 1952);

    #[test]
    fn test_p1_single_word() {
        assert_eq!(part_1(&parse_input("XMAS")), 1);
    }

    #[test]
    fn test_p1_single_word_reverse() {
        assert_eq!(part_1(&parse_input("SAMX")), 1);
    }
}
