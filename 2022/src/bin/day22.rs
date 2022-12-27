#![feature(custom_test_frameworks)]
#![feature(int_roundings)]
#![feature(test)]

use aoc2022::coord::Point;
use aoc2022::*;
use itertools::Itertools;

const DAY: usize = 22;

type Grid = Vec<Vec<Option<Tile>>>;
type Parsed = (Grid, Vec<Cmd>);

fn main() {
    let input = read_file(DAY);
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed, 50));
}

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

fn part_1((grid, cmds): &Parsed) -> i32 {
    let start_x = grid[0].iter().position(|t| t == &Some(Tile::Air)).unwrap();
    let mut state = State {
        pos: Point([start_x as i32, 0]),
        dir: Point::<2>::E,
    };

    for cmd in cmds {
        match cmd {
            Cmd::RotateLeft => state.dir.rotate_left(90),
            Cmd::RotateRight => state.dir.rotate_right(90),
            &Cmd::Move(by) => {
                for _ in 0..by {
                    let mut next_pos = state.pos + state.dir;
                    let mut next_tile = get_tile(grid, next_pos);
                    if next_tile.is_none() {
                        // Wrap around by going in the opposite direction
                        // until we reach the end.
                        let mut rev_dir = state.dir.clone();
                        rev_dir.rotate_right(180);
                        loop {
                            let prev_pos = next_pos + rev_dir;
                            let prev_tile = get_tile(grid, prev_pos);
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
    state.password()
}

fn part_2((grid, cmds): &Parsed, cube_size: i32) -> i32 {
    let start_x = grid[0].iter().position(|t| t == &Some(Tile::Air)).unwrap();
    let mut state = State {
        pos: Point([start_x as i32, 0]),
        dir: Point::<2>::E,
    };

    state = walk_cube(grid, cmds, state, cube_size);
    state.password()
}

fn walk_cube(grid: &Grid, cmds: &Vec<Cmd>, mut state: State, cube_size: i32) -> State {
    for cmd in cmds {
        match cmd {
            Cmd::RotateLeft => state.dir.rotate_left(90),
            Cmd::RotateRight => state.dir.rotate_right(90),
            &Cmd::Move(by) => {
                for _ in 0..by {
                    let mut next_pos = state.pos + state.dir;
                    let mut next_dir = state.dir;
                    let next_tile = get_tile(grid, next_pos);
                    if next_tile.is_none() {
                        (next_pos, next_dir) = wrap_around_cube(state.pos, state.dir, cube_size);
                    }
                    let next_tile = get_tile(grid, next_pos).unwrap();
                    if next_tile == Tile::Wall {
                        break;
                    }
                    state.pos = next_pos;
                    state.dir = next_dir;
                }
            }
        }
    }
    state
}

fn get_tile(grid: &Grid, pos: Point<2>) -> Option<Tile> {
    grid.get(pos.y() as usize)
        .and_then(|row| row.get(pos.x() as usize))
        .cloned()
        .flatten()
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct State {
    pos: Point<2>,
    dir: Point<2>,
}

impl State {
    fn password(&self) -> i32 {
        (self.pos.y() + 1) * 1000
            + (self.pos.x() + 1) * 4
            + match self.dir {
                Point::<2>::E => 0,
                Point::<2>::S => 1,
                Point::<2>::W => 2,
                Point::<2>::N => 3,
                _ => unreachable!(),
            }
    }
}

fn wrap_around_cube(source_pos: Point<2>, dir: Point<2>, cs: i32) -> (Point<2>, Point<2>) {
    // All inputs supposedly have the following shape:
    //
    //       |              (1,-1) (2,-1)
    //   12  |        (0,0) (1, 0) (2, 0) (3,0)
    //   3   |        (0,1) (1, 1) (2, 1)
    //  54   | (-1,2) (0,2) (1, 2) (2, 2)
    //  6    | (-1,3) (0,3) (1, 3)
    //       |        (0,4)
    //
    // We need mappings for the following transitions:
    // 1->5, 1->6
    // 2->3, 2->4, 2->6
    // 3->2, 3->5
    // 4->2, 4->6
    // 5->3, 5->1
    // 6->1, 6->2, 6->4
    let source_side = (source_pos.x().div_floor(cs), source_pos.y().div_floor(cs));
    let target_pos = source_pos + dir;
    let target_side = (target_pos.x().div_floor(cs), target_pos.y().div_floor(cs));
    match (source_side, target_side) {
        // 1->5 | (0,0)=>(0,2)
        (_, (0, 0)) => (Point([0, 3 * cs - 1 - source_pos.y() % cs]), Point::<2>::E),
        // 1->6 | (1,-1)=>(0,3)
        (_, (1, -1)) => (Point([0, 3 * cs + source_pos.x() % cs]), Point::<2>::E),
        // 2->3 | (2,1)=>(1,1)
        ((2, 0), (2, 1)) => (Point([2 * cs - 1, cs + source_pos.x() % cs]), Point::<2>::W),
        // 2->4 | (3,0)=>(1,2)
        (_, (3, 0)) => (
            Point([2 * cs - 1, 3 * cs - 1 - source_pos.y() % cs]),
            Point::<2>::W,
        ),
        // 2->6 | (2,-1)=>(0,3)
        (_, (2, -1)) => (Point([source_pos.x() % cs, 4 * cs - 1]), Point::<2>::N),
        // 3->2 | (2,1)=>(2,0)
        ((1, 1), (2, 1)) => (Point([2 * cs + source_pos.y() % cs, cs - 1]), Point::<2>::N),
        // 3->5 | (0,1)=>(1,2)
        ((1, 1), (0, 1)) => (Point([source_pos.y() % cs, 2 * cs]), Point::<2>::S),
        // 4->2 | (2,2)=>(2,0)
        (_, (2, 2)) => (
            Point([3 * cs - 1, cs - 1 - source_pos.y() % cs]),
            Point::<2>::W,
        ),
        // 4->6 | (1,3)=>(0,3)
        ((1, 2), (1, 3)) => (Point([cs - 1, 3 * cs + source_pos.x() % cs]), Point::<2>::W),
        // 5->3 | (0,1)=>(1,1)
        ((0, 2), (0, 1)) => (Point([cs, cs + source_pos.x() % cs]), Point::<2>::E),
        // 5->1 | (-1,2)=>(1,0)
        (_, (-1, 2)) => (Point([cs, cs - 1 - source_pos.y() % cs]), Point::<2>::E),
        // 6->1 | (-1,3)=>(1,0)
        (_, (-1, 3)) => (Point([cs + source_pos.y() % cs, 0]), Point::<2>::S),
        // 6->2 | (0,4)=>(2,0)
        (_, (0, 4)) => (Point([2 * cs + source_pos.x() % cs, 0]), Point::<2>::S),
        // 6->4 | (1,3)=>(1,2)
        ((0, 3), (1, 3)) => (Point([cs + source_pos.y() % cs, 3 * cs - 1]), Point::<2>::N),
        _ => panic!("unexpected wrap: {source_side:?}->{target_side:?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    // Cannot inline test input nicely because it has leading whitespace.
    const TEST_INPUT: &str = include_str!("../../input/day22_test.txt");
    const TEST_INPUT_2: &str = include_str!("../../input/day22_test2.txt");
    const BLANK_INPUT: &str = include_str!("../../input/day22_blank.txt");

    test!(part_1() == 6032);
    bench_parse!(
        |p: &Parsed| (p.0.len(), p.1.len()),
        (200, (1046 + 954) * 2 + 1)
    );
    bench!(part_1() == 131052);
    bench!(part_2(50) == 4578);

    // I didn't implement the wrapping rules for the example layout.
    // Instead we test with a custom test input.
    // test!(part_2(4) == 5031);

    #[test]
    fn test_part_2_custom_test_input() {
        let parsed = parse_input(TEST_INPUT_2);
        let result = part_2(&parsed, 4);
        assert_eq!(result, 2 * 1000 + 5 * 4 + 1);
    }

    #[test]
    fn test_part_2_folds_horizontal() {
        // Walk from the first point in each row 200 steps in one direction.
        // We should end up exactly where we started.
        let (grid, cmds) = parse_input(BLANK_INPUT);
        let vertical_pts: Vec<_> = grid
            .iter()
            .enumerate()
            .flat_map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .find_map(move |(x, tile)| tile.and_then(|_| Some(Point([x as i32, y as i32]))))
            })
            .collect();
        for pos in vertical_pts.iter().cloned() {
            let state = State {
                pos,
                dir: Point::<2>::E,
            };
            let state_after = walk_cube(&grid, &cmds, state.clone(), 50);
            assert_eq!(state, state_after);
            let state = State {
                pos,
                dir: Point::<2>::W,
            };
            let state_after = walk_cube(&grid, &cmds, state.clone(), 50);
            assert_eq!(state, state_after);
        }
    }

    #[test]
    fn test_part_2_folds_vertical() {
        // Same as horizonzal version.
        let (grid, cmds) = parse_input(BLANK_INPUT);
        let horizontal_pts: Vec<_> = (0..50 * 3)
            .flat_map(|x| {
                grid.iter().enumerate().find_map(move |(y, row)| {
                    row.get(x)
                        .cloned()
                        .flatten()
                        .and_then(|_| Some(Point([x as i32, y as i32])))
                })
            })
            .collect();
        for pos in horizontal_pts.iter().cloned() {
            let state = State {
                pos,
                dir: Point::<2>::S,
            };
            let state_after = walk_cube(&grid, &cmds, state.clone(), 50);
            assert_eq!(state, state_after);
            let state = State {
                pos,
                dir: Point::<2>::N,
            };
            let state_after = walk_cube(&grid, &cmds, state.clone(), 50);
            assert_eq!(state, state_after);
        }
    }

    use Cmd::*;

    #[test_case("L10R" => vec![RotateLeft, Move(10), RotateRight])]
    #[test_case("L10R5" => vec![RotateLeft, Move(10), RotateRight, Move(5)])]
    #[test_case("10R1" => vec![Move(10), RotateRight, Move(1)])]
    #[test_case("10R1L" => vec![Move(10), RotateRight, Move(1), RotateLeft])]
    fn parse_cmds(s: &str) -> Vec<Cmd> {
        super::parse_cmds(s)
    }
}
