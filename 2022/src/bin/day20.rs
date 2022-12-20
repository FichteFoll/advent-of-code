#![feature(linked_list_cursors)]
#![feature(test)]

use std::collections::LinkedList;

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 20;

type Parsed = Vec<isize>;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.lines().map(|line| line.parse().unwrap()).collect()
    }
}

#[derive(Debug)]
struct Item(isize, bool);

fn part_1(parsed: &Parsed) -> isize {
    let mut list: LinkedList<_> = parsed.iter().map(|&n| Item(n, false)).collect();
    let size = list.len();
    let isize_ = size as isize;
    let mut moved = 0;
    let mut cursor = list.cursor_front_mut();

    // Cannot use a function because of lifetimes.
    // Note: can exit at the "ghost" element.
    use std::cmp::Ordering::*;
    macro_rules! move_cursor_by {
        ($by:expr) => {
            match ($by).cmp(&0isize) {
                Equal => (),
                Greater => {
                    for _ in 0..($by) {
                        if cursor.current().is_none() {
                            cursor = list.cursor_front_mut();
                        }
                        cursor.move_next();
                    }
                }
                Less => {
                    for _ in 0..-($by) {
                        if cursor.current().is_none() {
                            cursor = list.cursor_back_mut();
                        }
                        cursor.move_prev();
                    }
                }
            };
        };
    }

    while moved < size {
        let Some(maybe_current) = cursor.current()
        else {
            cursor = list.cursor_front_mut();
            continue;
        };
        if maybe_current.1 {
            cursor.move_next();
            continue;
        }
        // read params and track state
        let mut current = cursor.remove_current().unwrap();
        current.1 = true;
        moved += 1;
        // skip needless iterations & offset by the number of wraps we saved this way
        let move_by = current.0 % isize_ + current.0 / isize_;
        // let move_by = current.0;
        move_cursor_by!(move_by);
        cursor.insert_before(current);
        if move_by > 0 {
            move_cursor_by!(-move_by - 1); // we added one item *before*, so move back one more step
        } else {
            move_cursor_by!(-move_by);
        }
    }

    // Get results for part 1 here because the macro isn't portable.
    // TODO move elsewhere if something else is needed in part 2
    cursor = list.cursor_front_mut();
    loop {
        match cursor.current() {
            None => panic!("didn't find 0 element"),
            Some(Item(0, _)) => break,
            _ => cursor.move_next(),
        };
    }
    let mut sum = 0;
    let move_by = 1000 % isize_;
    for _ in 0..3 {
        move_cursor_by!(move_by);
        sum += cursor.current().unwrap().0; // pray this is not a boundary
    }
    sum
}

fn part_2(_parsed: &Parsed) -> isize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        1\n\
        2\n\
        -3\n\
        3\n\
        -2\n\
        0\n\
        4\n\
        ";

    test!(part_1() == 3);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 5000);
    bench!(part_1() == 13522);
    // bench!(part_2() == 0);
}
