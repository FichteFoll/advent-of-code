#![feature(test)]

use aoc2024::*;
use itertools::iproduct;

const DAY: usize = 4;

type Parsed = Vec<Vec<u8>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(|line| line.bytes().collect()).collect()
}

const XMAS: [u8; 4] = [b'X', b'M', b'A', b'S'];

fn part_1(grid: &Parsed) -> usize {
    iproduct!(0..grid[0].len(), 0..grid.len())
        .filter(|&(x, y)| grid[y][x] == XMAS[0])
        .filter(|pt| is_xmas(pt, grid))
        .count()
}

fn part_2(grid: &Parsed) -> usize {
    iproduct!(0..grid[0].len(), 0..grid.len())
        .filter(|&(x, y)| grid[y][x] == b'A')
        .filter(|pt| is_x_mas(grid, pt).unwrap_or(false))
        .count()
}

#[rustfmt::skip]
enum Dir {
    NW, N, NE,
     W,     E,
    SW, S, SE,
}
use Dir::*;

impl Dir {
    #[rustfmt::skip]
    const ALL: [Dir; 8] = [
        NW, N, NE,
         W,     E,
        SW, S, SE,
    ];
}

fn is_xmas(pt: &(usize, usize), grid: &Parsed) -> bool {
    Dir::ALL.iter().any(|dir| {
        XMAS.iter()
            .enumerate()
            .skip(1)
            .all(|(i, c)| char_in_dir(grid, pt, i, dir) == Some(*c))
    })
}

// Since the input only contains the chars X, M, A, S,
// we can just xor the pairs
// to check if both required chars are present.
const M_S_HASH: u8 = b'M' ^ b'S';

fn is_x_mas(grid: &Parsed, pt: &(usize, usize)) -> Option<bool> {
    let hashes = [
        char_in_dir(grid, pt, 1, &NW)? ^ char_in_dir(grid, pt, 1, &SE)?,
        char_in_dir(grid, pt, 1, &SW)? ^ char_in_dir(grid, pt, 1, &NE)?,
    ];
    Some(hashes.into_iter().all(|h| h == M_S_HASH))
}

#[rustfmt::skip]
fn char_in_dir(grid: &Parsed, &(x, y): &(usize, usize), i: usize, dir: &Dir) -> Option<u8> {
    Some(match dir {
        NW => *grid.get(y.checked_sub(i)?)?.get(x.checked_sub(i)?)?,
        N  => *grid.get(y.checked_sub(i)?)?.get(x)?,
        NE => *grid.get(y.checked_sub(i)?)?.get(x + i)?,
         W => *grid.get(y)?                .get(x.checked_sub(i)?)?,
         E => *grid.get(y)?                .get(x + i)?,
        SW => *grid.get(y + i)?            .get(x.checked_sub(i)?)?,
        S  => *grid.get(y + i)?            .get(x)?,
        SE => *grid.get(y + i)?            .get(x + i)?,
    })
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
