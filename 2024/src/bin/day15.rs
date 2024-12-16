#![feature(test)]

use std::collections::VecDeque;

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
        if move_simple(&mut grid, robot, dir) {
            robot += dir;
        }
    }
    score(&grid)
}

fn part_2((grid, instr): &Parsed) -> i32 {
    part_2_expanded(expand(grid), instr)
}

// separate function for testing
fn part_2_expanded(mut grid: Grid2D<char>, instr: &Vec<Point<2>>) -> i32 {
    let mut robot = grid.position(|c| c == &'@').unwrap();

    for dir in instr {
        let moved = if dir.y() == 0 {
            move_simple(&mut grid, robot, dir)
        } else {
            move_wide(&mut grid, robot, *dir)
        };
        if moved {
            robot += dir;
        }
    }
    score(&grid)
}

fn move_wide(grid: &mut Grid2D<char>, robot: P, dir: P) -> bool {
    let mut moving_points = vec![];
    let mut candidates: VecDeque<_> = [robot].into();
    while let Some(cand) = candidates.pop_front() {
        if moving_points.contains(&cand) {
            // Each of the two sides of a block
            // can cause the upper block to be inserted twice
            // if they are perfectly in line.
            continue;
        }
        let check_pt = cand + dir;
        match grid.get(&check_pt).unwrap() {
            &'#' => return false,
            &'.' => (),
            &'@' => candidates.push_back(check_pt),
            &'[' => {
                candidates.push_back(check_pt);
                candidates.push_back(check_pt + P::E);
            }
            &']' => {
                candidates.push_back(check_pt + P::W);
                candidates.push_back(check_pt);
            }
            c => panic!("Unexpected char {c}"),
        }
        moving_points.push(cand);
    }
    // `moving_points` are effectively ordered right now,
    // so we can just iterate backwards.
    for pt in moving_points.into_iter().rev() {
        grid.swap(&pt, &(pt + dir));
    }
    true
}

fn move_simple(grid: &mut Grid2D<char>, robot: P, dir: &P) -> bool {
    let Some(free_i) = (1i32..)
        .map_while(|i| {
            grid.get(&(robot + dir * i))
                .filter(|c| c != &&'#')
                .map(|b| (i, b))
        })
        .find_map(|(i, b)| (b == &'.').then_some(i))
    else {
        return false;
    };
    for (i1, i2) in (0..=free_i).rev().tuple_windows() {
        let (pt1, pt2) = (robot + dir * i1, robot + dir * i2);
        grid.swap(&pt1, &pt2);
    }
    true
}

fn score(grid: &Grid2D<char>) -> i32 {
    grid.iter_enumerate()
        .filter_map(|(pt, b)| (b == &'O' || b == &'[').then_some(pt))
        .map(|pt| pt.x() + 100 * pt.y())
        .sum()
}

fn expand(grid: &Grid2D<char>) -> Grid2D<char> {
    grid.rows
        .iter()
        .map(|row| {
            row.iter().flat_map(|c| match c {
                &'.' => ['.', '.'],
                &'@' => ['@', '.'],
                &'O' => ['[', ']'],
                &'#' => ['#', '#'],
                c => panic!("Unmappable character {c}"),
            })
        })
        .collect()
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

    const TEST_INPUT_SMALL_2: &str = "\
        #######\n\
        #...#.#\n\
        #.....#\n\
        #..OO@#\n\
        #..O..#\n\
        #.....#\n\
        #######\n\
        \n\
        <vv<<^^<<^^\n\
        ";

    test!(part_1() == 10092);
    test!(part_2() == 9021);
    bench_parse!(|p: &Parsed| (p.0.size, p.1.len()), (Size(50, 50), 20000));
    bench!(part_1() == 1471826);
    bench!(part_2() == 1457703);

    test!(small, TEST_INPUT_SMALL, part_1() == 2028);
    // (100 * 1 + 5) + (100 * 2 + 7) + (100 * 3 + 6)
    test!(small, TEST_INPUT_SMALL_2, part_2() == 618);

    #[test]
    fn part_2_special() {
        let grid_s: &str = "\
            ####################\n\
            ##....[]....[]..[]##\n\
            ##............[]..##\n\
            ##..[][]....[]..[]##\n\
            ##...[].......[]..##\n\
            ##[]##....[]......##\n\
            ##[]......[]..[]..##\n\
            ##..[][]..@[].[][]##\n\
            ##........[]......##\n\
            ####################\n\
            ";
        let grid = grid_s.lines().map(str::chars).collect();
        let instr = vec![P::N];
        assert_eq!(part_2_expanded(grid, &instr), 9604);
    }
}
