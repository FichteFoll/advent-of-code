#![feature(test)]

use itertools::Itertools;

use aoc2024::*;
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 15;

type Parsed = (Grid2D<char>, Vec<P>);
type P = Point<2>;

main!();

fn parse_input(input: &str) -> Parsed {
    let (grid_s, instr_s) = input.split_once("\n\n").unwrap();
    let grid = grid_s.lines().map(str::chars).collect();
    let instr = instr_s
        .bytes()
        .filter_map(|b| match b {
            b'^' => Some(P::N),
            b'<' => Some(P::W),
            b'>' => Some(P::E),
            b'v' => Some(P::S),
            _ => None,
        })
        .collect();
    (grid, instr)
}

fn part_1((grid, instr): &Parsed) -> i32 {
    let mut grid = grid.clone();
    let mut robot = grid.position(|c| c == &'@').unwrap();
    for dir in instr {
        let Some(free_i) = (1i32..)
            .into_iter()
            .map_while(|i| {
                grid.get(&(robot + dir * i))
                    .filter(|c| c != &&'#')
                    .map(|b| (i, b))
            })
            .find_map(|(i, b)| (b == &'.').then_some(i))
        else {
            continue;
        };
        // Move everything towards the free block.
        // (realistically I only need to do at most 2 swaps,
        // but I cba to consider the edge case of free_i = 1.
        for (i1, i2) in (0..=free_i).rev().tuple_windows() {
            let (pt1, pt2) = (robot + dir * i1, robot + dir * i2);
            grid.swap(&pt1, &pt2);
        }
        robot += dir;
    }
    score(&grid)
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn score(grid: &Grid2D<char>) -> i32 {
    grid.iter_enumerate()
        .filter_map(|(pt, b)| (b == &'O').then_some(pt))
        .map(|pt| pt.x() + 100 * pt.y())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use grid2d::Size;

    const TEST_INPUT: &str = "\
        ##########\n\
        #..O..O.O#\n\
        #......O.#\n\
        #.OO..O.O#\n\
        #..O@..O.#\n\
        #O#..O...#\n\
        #O..O..O.#\n\
        #.OO.O.OO#\n\
        #....O...#\n\
        ##########\n\
        \n\
        <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
        vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
        ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
        <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
        ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
        ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
        >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
        <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
        ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
        v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<\n\
        ";

    const TEST_INPUT_SMALL: &str = "\
        ########\n\
        #..O.O.#\n\
        ##@.O..#\n\
        #...O..#\n\
        #.#.O..#\n\
        #...O..#\n\
        #......#\n\
        ########\n\
        \n\
        <^^>>>vv<v>>v<<\n\
        ";

    test!(part_1() == 10092);
    test!(small, TEST_INPUT_SMALL, part_1() == 2028);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| (p.0.size, p.1.len()), (Size(50, 50), 20000));
    bench!(part_1() == 1471826);
    // bench!(part_2() == 0);
}
