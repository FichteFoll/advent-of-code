#![feature(test, str_split_once, bool_to_option)]

fn read_input() -> String {
    std::fs::read_to_string("input/day05.txt").expect("can’t read file")
}

type Input = Vec<usize>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| {
            line.bytes()
                .fold(0usize, |n, b| n << 1 | (b == b'B' || b == b'R').then_some(1).unwrap_or(0))
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    *input.iter().max().unwrap()
}

fn part_2(input: &Input) -> usize {
    let mut sorted_input = input.clone();
    sorted_input.sort_unstable();
    sorted_input.iter()
        .tuple_windows()
        .find_map(|(a, b)| (a + 1 != *b).then_some(a + 1))
        .expect("no seat found")
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
    fn test_parse() {
        let input_str = "\
            FBFBBFFRLR\n\
            BFFFBBFRRR\n\
            FFFBBBFRRR\n\
            BBFFBBFRLL\n\
            ";
        let nums = vec![357, 567, 119, 820];
        assert_eq!(parse_input(input_str), nums)
    }

    #[test]
    fn test_part_1() {
        let input_str = "\
            BFFFBBFRRR\n\
            FFFBBBFRRR\n\
            BBFFBBFRLL\n\
            ";
        let input = parse_input(input_str);
        assert_eq!(part_1(&input), 820);
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
            assert_eq!(part_1(&input), 858);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 557);
        });
    }
}
