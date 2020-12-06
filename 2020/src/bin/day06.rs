#![feature(test, bool_to_option)]

use std::collections::BTreeSet;

fn read_input() -> String {
    std::fs::read_to_string("input/day06.txt").expect("can’t read file")
}

type Input = Vec<BTreeSet<u8>>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n\n")
        .map(|group|
            group.bytes()
                .filter(|&b| b != b'\n')
                .collect()
        )
        .collect()
}

fn part_1(input: &Input) -> usize {
    input.iter().map(BTreeSet::len).sum()
}

fn part_2(_input: &Input) -> usize {
    0
}

fn main() {
    let input_str = read_input();
    let input = parse_input(&input_str);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    extern crate test;
    use test::Bencher;

    #[test]
    fn test_part_1() {
        let input_str = "\
            abc\n\
            \n\
            a\n\
            b\n\
            c\n\
            \n\
            ab\n\
            ac\n\
            \n\
            a\n\
            a\n\
            a\n\
            a\n\
            \n\
            b\n\
            ";
        let input = parse_input(input_str);
        assert_eq!(part_1(&input), 11);
    }

    #[bench]
    fn bench_parse(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let _ = parse_input(&input_str);
        });
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_1(&input), 6583);
        });
    }

    // #[bench]
    // fn bench_part_2(b: &mut Bencher) {
    //     let input_str = read_input();
    //     let input = parse_input(&input_str);
    //     b.iter(|| {
    //         assert_eq!(part_2(&input), 557);
    //     });
    // }
}
