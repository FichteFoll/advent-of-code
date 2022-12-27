#![feature(custom_test_frameworks)]
#![feature(int_roundings)]
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
    // Makes macro usages not portable but doesn't matter
    // since I cannot reuse it for part 2 anyway.
    // Screw you, original order.
    // Note: can exit at the "ghost" element.
    use std::cmp::Ordering::*;
    macro_rules! move_cursor_by {
        ($by:expr) => {
            match ($by).cmp(&0) {
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

fn part_2(parsed: &Parsed) -> isize {
    let decrypted: Vec<_> = parsed.iter().map(|&n| n * 811589153).collect();
    move_numbers_slice_copy(&decrypted, 10)
}

fn move_numbers_slice_copy(parsed: &Parsed, iterations: usize) -> isize {
    // Include the numbers' indices to make them unique.
    let original: Vec<_> = parsed.iter().cloned().enumerate().collect();
    let size = original.len();
    let isize_ = original.len() as isize;
    let mut numbers: Vec<_> = original.clone();
    for i in 0..iterations {
        dbg!(i);
        // This is O(n**2).
        for n in original.iter() {
            let pos = numbers.iter().position(|x| x == n).unwrap();
            let new_pos = calc_new_pos(pos, n.1, isize_);
            move_num_slice_copy(&mut numbers, pos, new_pos);
        }
    }
    let start = numbers.iter().position(|&x| x.1 == 0).unwrap();
    [1000, 2000, 3000]
        .into_iter()
        .map(|offset| (start + offset) % size)
        .map(|i| numbers[i].1)
        .sum()
}

fn move_num_slice_copy(numbers: &mut [(usize, isize)], pos: usize, new_pos: usize) {
    use std::cmp::Ordering::*;
    let n = numbers[pos];
    match new_pos.cmp(&pos) {
        Equal => (),
        Greater => {
            let other = numbers[pos + 1..=new_pos].to_vec();
            numbers[pos..new_pos].copy_from_slice(&other);
            numbers[new_pos] = n;
        }
        Less => {
            let other = numbers[new_pos..pos].to_vec();
            numbers[new_pos + 1..=pos].copy_from_slice(&other);
            numbers[new_pos] = n;
        }
    };
}

// Separate function for tests.
fn calc_new_pos(upos: usize, n: isize, size: isize) -> usize {
    // There are a few things happening here.
    // - `n` cannot move over itself, so we reduce redundant loops via `% (size - 1)`
    // - We start at `size` to prevent underflowing `usize`.
    // - For our movement operation, position `0` and `size - 1` are identical,
    //   so we add one additional step whenever we wrap around a list end.
    // - The final `% (size - 1)` is only to make results consistent.
    let move_by = n % (size - 1);
    let raw_new_pos = upos as isize + move_by;
    let new_pos = (size + raw_new_pos + raw_new_pos.div_floor(size)) % size % (size - 1);
    debug_assert!(0 <= new_pos && new_pos < (size - 1));
    new_pos as usize
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

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
    test!(part_2() == 1623178306);
    bench_parse!(Vec::len, 5000);
    bench!(part_1() == 13522);
    bench!(part_2() == 17113168880158);

    #[test]
    fn move_numbers_part_1() {
        // test the new algorithm with part 1 for verification
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(move_numbers_slice_copy(&parsed, 1), 3);
    }

    #[test_case(2, 7, 7 => 3; "2_plus_7_expects_3")]
    #[test_case(2, 6, 7 => 2; "2_plus_6_expects_2")]
    #[test_case(2, 5, 7 => 1; "2_plus_5_expects_1")]
    #[test_case(2, 4, 7 => 0; "2_plus_4_expects_0")]
    #[test_case(2, 3, 7 => 5; "2_plus_3_expects_5")]
    #[test_case(2, 2, 7 => 4; "2_plus_2_expects_4")]
    #[test_case(2, 1, 7 => 3; "2_plus_1_expects_3")]
    #[test_case(2, 0, 7 => 2; "2_plus_0_expects_2")]
    #[test_case(2, -1, 7 => 1; "2_minus_1_expects_1")]
    #[test_case(2, -2, 7 => 0; "2_minus_2_expects_0")]
    #[test_case(2, -3, 7 => 5; "2_minus_3_expects_5")]
    #[test_case(2, -4, 7 => 4; "2_minus_4_expects_4")]
    #[test_case(2, -5, 7 => 3; "2_minus_5_expects_3")]
    #[test_case(2, -6, 7 => 2; "2_minus_6_expects_2")]
    #[test_case(2, -7, 7 => 1; "2_minus_7_expects_1")]
    #[test_case(2, -8, 7 => 0; "2_minus_8_expects_0")]
    #[test_case(2, -9, 7 => 5; "2_minus_9_expects_5")]
    #[test_case(2, -10, 7 => 4; "2_minus_10_expects_4")]
    #[test_case(2, -14, 7 => 0; "2_minus_14_expects_0")]
    #[test_case(2, -15, 7 => 5; "2_minus_15_expects_5")]
    #[test_case(2, 1623178306, 7 => 0; "2_plus_1623178306_expects_5")] // same as 2, 4, 7
    fn calc_new_pos(pos: usize, n: isize, size: isize) -> usize {
        super::calc_new_pos(pos, n, size)
    }

    #[test_case(7, 2, 0 => vec![2, 0, 1, 3, 4, 5, 6])]
    #[test_case(7, 2, 1 => vec![0, 2, 1, 3, 4, 5, 6])]
    #[test_case(7, 2, 2 => vec![0, 1, 2, 3, 4, 5, 6])]
    #[test_case(7, 2, 3 => vec![0, 1, 3, 2, 4, 5, 6])]
    #[test_case(7, 2, 4 => vec![0, 1, 3, 4, 2, 5, 6])]
    #[test_case(7, 2, 5 => vec![0, 1, 3, 4, 5, 2, 6])]
    fn move_num_slice_copy(size: isize, pos: usize, new_pos: usize) -> Vec<isize> {
        let mut nums: Vec<_> = (0..size).enumerate().collect();
        super::move_num_slice_copy(&mut nums, pos, new_pos);
        nums.into_iter().map(|n| n.1).collect()
    }

    bench!(move_numbers_slice_copy(1) == 13522);
}
