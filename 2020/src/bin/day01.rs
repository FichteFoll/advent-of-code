#![feature(test,bool_to_option)]

use itertools::Itertools;


fn read_input() -> String {
    std::fs::read_to_string("input/day01.txt").expect("can’t read file")
}

fn parse_input(input_str: &str) -> Vec<u64> {
    input_str.trim().split("\n").map(|x| x.parse().unwrap()).collect()
}

fn part_1(input: &[u64]) -> u64 {
    input.iter()
        .tuple_combinations()
        .find_map(|(&a, &b)| (a + b == 2020).then_some(a * b))
        .expect("No solution found")
}

fn part_2(input: &[u64]) -> u64 {
    let mut lookup = [None; 2020];
    for (a, b) in input.iter().tuple_combinations() {
        let i = (a + b) as usize;
        if i < 2020 {
            lookup[i] = Some(a * b);
        }
    }
    input.iter()
        .find_map(|x| lookup[(2020 - x) as usize].map(|ab| ab * x))
        .expect("No solution found")
}

fn main() {
    let input_str = read_input();
    let input = parse_input(&input_str);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}


#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use test::Bencher;

    #[test]
    fn test_part_1() {
        let input = vec![
            1721,
            979,
            366,
            299,
            675,
            1456,
        ];
        assert_eq!(part_1(&input), 514579);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(part_1(&input), 719796);
        });
    }

    #[test]
    fn test_part_2() {
        let input = vec![
            1721,
            979,
            366,
            299,
            675,
            1456,
        ];
        assert_eq!(part_2(&input), 241861950);
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(part_2(&input), 144554112);
        });
    }
}
