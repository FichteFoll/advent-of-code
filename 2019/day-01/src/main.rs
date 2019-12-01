#![feature(test)]

extern crate test;


fn read_input() -> String {
    std::fs::read_to_string("input.txt").expect("can’t read file")
}

fn parse_input(input_str: &str) -> Vec<u64> {
    input_str.trim().split("\n").map(|x| x.parse().unwrap()).collect()
}

fn part_1(input: &[u64]) -> u64 {
    input.into_iter().map(|m| m / 3 - 2).sum()
}

fn fuel_recursive(m: u64) -> u64 {
    match m / 3 {
        n if n > 2 => n - 2 + fuel_recursive(n - 2),
        _ => 0,
    }
}

fn part_2(input: &[u64]) -> u64 {
    input.into_iter().map(|m| fuel_recursive(*m)).sum()
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
    use test::Bencher;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&[12]), 2);
        assert_eq!(part_1(&[14]), 2);
        assert_eq!(part_1(&[1969]), 654);
        assert_eq!(part_1(&[100756]), 33583);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(part_1(&input), 3394689);
        });
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&[14]), 2);
        assert_eq!(part_2(&[1969]), 966);
        assert_eq!(part_2(&[100756]), 50346);
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(part_2(&input), 5089160);
        });
    }
}
