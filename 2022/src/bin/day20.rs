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
    move_numbers(&decrypted, 10)
}

fn move_numbers(original: &Parsed, iterations: usize) -> isize {
    use std::cmp::Ordering::*;
    let size = original.len();
    let isize_ = original.len() as isize;
    let mut numbers: Vec<_> = original.clone();
    // dbg!(size);
    println!("{numbers:?}");
    for i in 0..iterations {
        dbg!(i);
        // this is O(n**2)
        for &n in original {
            let pos = numbers.iter().position(|&x| x == n).unwrap();
            let new_pos = calc_new_pos(pos, n, isize_);
            match new_pos.cmp(&pos) {
                Equal => continue,
                Greater => {
                    let mut other = numbers[pos + 1..=new_pos].to_vec();
                    numbers[pos..new_pos].copy_from_slice(&mut other);
                    numbers[new_pos] = n;
                }
                Less => {
                    let mut other = numbers[new_pos..pos].to_vec();
                    numbers[new_pos + 1..=pos].copy_from_slice(&mut other);
                    numbers[new_pos] = n;
                }
            };
            // println!("{numbers:?}");
        }
        println!("{numbers:?}");
    }
    let start = numbers.iter().position(|&x| x == 0).unwrap();
    [1000, 2000, 3000].into_iter()
        .map(|offset| (start + offset) % size)
        .map(|i| numbers[i])
        .sum()
}

#[allow(unused)]
fn calc_new_pos(upos: usize, n: isize, size: isize) -> usize {
    // let ufp = ((isize_ - 1) + pos as isize + move_by) as usize;
    // let new_pos = (ufp + ufp / size) % size;
    let pos = upos as isize;

    // n cannot move over itself
    //
    // the following fails part 1 with real input
    let move_by = n % (size - 1);
    let raw_new_pos = pos + move_by;
    let new_pos1 = if raw_new_pos < 0 {
        size + raw_new_pos - 1
    } else if raw_new_pos >= size {
        (raw_new_pos + 1) % size
    } else {
        raw_new_pos
    };

    // the following is the same as `new_pos` but in a single line
    let new_pos2 = (size + raw_new_pos + raw_new_pos.div_floor(size)) % size % (size - 1);
    // dbg!(size, raw_new_pos, raw_new_pos.div_floor(size), size, (size - 1), new_pos2, new_pos1);
    // assert_eq!(new_pos1, new_pos2);

    // even more single line that before but also fails everything
    #[allow(unused)]
    let new_pos3 = (n % (size - 1) + pos + size) % (size - 1);

    // WIP
    // #[allow(unused)]
    // let new_pos4 =

    // assert_eq!(new_pos, new_pos3);
    let new_pos = new_pos2;
    // println!("moving {n} from {pos} by {move_by} to {new_pos}");
    // panic!();
    assert!(0 <= new_pos && new_pos < (size - 1));
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
    // bench!(part_2() == 0); > 6482162565011

    #[test]
    fn move_numbers_part_1() {
        // test the new algorithm with part 1 for verification
        let parsed = parse_input(TEST_INPUT);
        assert_eq!(move_numbers(&parsed, 1), 3);
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
    #[test_case(2, 1623178306, 7 => 0)] // same as 2, 4, 7
    fn calc_new_pos(pos: usize, n: isize, size: isize) -> usize {
        super::calc_new_pos(pos, n, size)
    }

    bench!(move_numbers(1) == 13522);
}
