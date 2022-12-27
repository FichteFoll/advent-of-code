#![feature(test)]
#![feature(custom_test_frameworks)]

use aoc2022::coord::Point;
use aoc2022::*;
use itertools::Itertools;

const DAY: usize = 22;

type Grid = Vec<Vec<Option<Tile>>>;
type Parsed = (Grid, Vec<Cmd>);

main!();

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Tile {
    Air,
    Wall,
}

#[derive(PartialEq, Eq, Debug)]
enum Cmd {
    RotateLeft,
    RotateRight,
    Move(usize),
}

fn parse_input(input: &str) -> Parsed {
    let (map_str, code_str) = input.split_once("\n\n").unwrap();
    let grid = parse_grid(map_str);
    let cmds = parse_cmds(code_str.trim());
    (grid, cmds)
}

fn parse_grid(map_str: &str) -> Vec<Vec<Option<Tile>>> {
    let grid = map_str
        .lines()
        .map(|line| {
            line.bytes()
                .map(|b| match b {
                    b'.' => Some(Tile::Air),
                    b'#' => Some(Tile::Wall),
                    b' ' => None,
                    _ => panic!("unexpected character {b}"),
                })
                .collect()
        })
        .collect();
    grid
}

fn parse_cmds(code_str: &str) -> Vec<Cmd> {
    let moves = code_str
        .split(['L', 'R'])
        .map(|s| s.parse().ok().map(|n| Cmd::Move(n)));
    let rotations = code_str
        .trim()
        .bytes()
        .filter_map(|b| match b {
            b'L' => Some(Cmd::RotateLeft),
            b'R' => Some(Cmd::RotateRight),
            _ => None,
        })
        .map(|cmd| Some(cmd));
    // Interleaves iterators of `Option`
    // to handle rotation or number coming first.
    moves.interleave(rotations).flatten().collect()
}

fn part_1(parsed: &Parsed) -> i32 {
    let state = walk_map(parsed);
    (state.pos.y() + 1) * 1000
        + (state.pos.x() + 1) * 4
        + match state.dir {
            Point::<2>::E => 0,
            Point::<2>::S => 1,
            Point::<2>::W => 2,
            Point::<2>::N => 3,
            _ => unreachable!(),
        }
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn walk_map((grid, cmds): &Parsed) -> State {
    let start_x = grid[0].iter().position(|t| t == &Some(Tile::Air)).unwrap();
    let mut state = State {
        pos: Point([start_x as i32, 0]),
        dir: Point::<2>::E,
    };

    let get_tile = |pos: Point<2>| {
        grid.get(pos.y() as usize)
            .and_then(|row| row.get(pos.x() as usize))
            .cloned()
            .flatten()
    };
    for cmd in cmds {
        match cmd {
            Cmd::RotateLeft => state.dir.rotate_left(90),
            Cmd::RotateRight => state.dir.rotate_right(90),
            &Cmd::Move(by) => {
                for _ in 0..by {
                    let mut next_pos = state.pos + state.dir;
                    let mut next_tile = get_tile(next_pos);
                    if next_tile.is_none() {
                        // Wrap around by going in the opposite direction
                        // until we reach the end.
                        let mut rev_dir = state.dir.clone();
                        rev_dir.rotate_right(180);
                        loop {
                            let prev_pos = next_pos + rev_dir;
                            let prev_tile = get_tile(prev_pos);
                            if prev_tile.is_none() {
                                break;
                            }
                            next_pos = prev_pos;
                            next_tile = prev_tile;
                        }
                    }
                    if next_tile == Some(Tile::Wall) {
                        break;
                    }
                    state.pos = next_pos;
                }
            }
        }
    }
    state
}

#[derive(Debug)]
struct State {
    pos: Point<2>,
    dir: Point<2>,
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    // Cannot inline test input nicely because it has leading whitespace.
    const TEST_INPUT: &str = include_str!("../../input/day22_test.txt");

    test!(part_1() == 6032);
    // test!(part_2() == 0);
    bench_parse!(
        |p: &Parsed| (p.0.len(), p.1.len()),
        (200, (1046 + 954) * 2 + 1)
    );
    bench!(part_1() == 131052);
    // bench!(part_2() == 0);

    use Cmd::*;

    #[test_case("L10R" => vec![RotateLeft, Move(10), RotateRight])]
    #[test_case("L10R5" => vec![RotateLeft, Move(10), RotateRight, Move(5)])]
    #[test_case("10R1" => vec![Move(10), RotateRight, Move(1)])]
    #[test_case("10R1L" => vec![Move(10), RotateRight, Move(1), RotateLeft])]
    fn parse_cmds(s: &str) -> Vec<Cmd> {
        super::parse_cmds(s)
    }
}
