#![feature(array_windows)]
#![feature(test)]

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 6;

type Parsed = String;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input.trim().to_string()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    find_unique_sequence::<4>(parsed)
}

fn part_2(parsed: &Parsed) -> usize {
    find_unique_sequence_on::<14>(parsed)
}

fn find_unique_sequence<const SEQUENCE_LEN: usize>(parsed: &String) -> usize {
    parsed
        .as_bytes()
        .array_windows::<SEQUENCE_LEN>()
        .position(|a| {
            let bitset = a.iter().fold(0u32, |acc, b| acc | 1 << (b - b'a'));
            bitset.count_ones() as usize == SEQUENCE_LEN
        })
        .unwrap()
        + SEQUENCE_LEN
}

fn find_unique_sequence_on<const SEQUENCE_LEN: usize>(parsed: &String) -> usize {
    // Achieve O(n) by tracking chars of the current window in an array
    // and no inner loops.
    // Faster for part 2 and about the same speed for part 1 but with many more lines.
    let mut state = [0u8; 26];
    let mut num_duplicates = 0;
    let bytes = parsed.as_bytes();
    for (i, b) in bytes.iter().enumerate() {
        let in_index = (b - b'a') as usize;
        if state[in_index] > 0 {
            num_duplicates += 1;
        }
        state[in_index] += 1;
        if i >= SEQUENCE_LEN {
            let out_b = bytes[i - SEQUENCE_LEN];
            let out_index = (out_b - b'a') as usize;
            state[out_index] -= 1;
            if state[out_index] > 0 {
                num_duplicates -= 1;
            }
        }
        if i + 1 >= SEQUENCE_LEN && num_duplicates == 0 {
            return i + 1;
        }
    }
    panic!("did not terminate");
}

#[allow(unused)]
#[rustfmt::skip]
fn find_unique_sequence_bitmask<const N: usize>(parsed: &String) -> usize {
    // This isn't my idea or implementation, but I liked it enough to include it here anyway.
    // It's also O(n) but uses the fact that XOR with a duplicate bit
    // flips it to zero which makes it not pass the count_ones check.
    let bytes = parsed.as_bytes();
    let mut mask: u32 = bytes[..N].iter().fold(0, |a, &byte| a ^ (1 << (byte - b'a')));
    for (index, window) in bytes.windows(N + 1).enumerate() {
        if mask.count_ones() == N as u32 {
            return index + N;
        }
        mask ^= (1 << (window[N] - b'a')) ^ (1 << (window[0] - b'a'));
    }
    panic!("did not terminate");
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    test!(part_1() == 7);
    bench_parse!(String::len, 4095);
    bench!(part_1() == 1582);
    bench!(part_2() == 3588);

    test!(ex1, "bvwbjplbgvbhsrlpgdmjqwftvncz", part_1() == 5);
    test!(ex2, "nppdvjthqldpwncqszvftbrmjlhg", part_1() == 6);
    test!(ex3, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", part_1() == 10);
    test!(ex4, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", part_1() == 11);

    test!(ex1, "mjqjpqmgbljsphdztnvjfqwrcgsmlb", part_2() == 19);
    test!(ex2, "bvwbjplbgvbhsrlpgdmjqwftvncz", part_2() == 23);
    test!(ex3, "nppdvjthqldpwncqszvftbrmjlhg", part_2() == 23);
    test!(ex4, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", part_2() == 29);
    test!(ex5, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", part_2() == 26);
}
