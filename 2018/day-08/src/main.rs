#![feature(test)]

extern crate test;

fn parse_input(input_str: &str) -> Vec<usize> {
    input_str.trim().split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn meta_sum(iter: &mut Iterator<Item=&usize>) -> usize {
    match iter.next() {
        Some(&num_childs) => {
            let &num_meta = iter.next().unwrap();
            (0..num_childs).map(|_| meta_sum(iter)).sum::<usize>()
                + (0..num_meta).map(|_| iter.next().unwrap()).sum::<usize>()
        },
        None => 0,
    }
}

fn part_1(nums: &[usize]) -> usize {
    meta_sum(&mut nums.iter())
}

fn value_sum(iter: &mut Iterator<Item=&usize>) -> usize {
    match iter.next() {
        Some(&num_childs) => {
            let &num_meta = iter.next().unwrap();
            let child_values: Vec<_> = (0..num_childs).map(|_| value_sum(iter)).collect();
            let meta: Vec<_> = (0..num_meta).map(|_| iter.next().unwrap()).collect();
            match num_childs {
                0 => meta.into_iter().sum(),
                _ => meta.into_iter().map(|i| child_values.get(*i - 1).unwrap_or(&0)).sum(),
            }
        },
        None => 0,
    }
}

fn part_2(nums: &[usize]) -> usize {
    value_sum(&mut nums.iter())
}

fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    let nums = parse_input(&input_str);
    println!("Part 1: {:?}", part_1(&nums));
    println!("Part 2: {:?}", part_2(&nums));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn part_1_trivial() {
        let input_str = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";
        let nums = parse_input(input_str);
        assert_eq!(part_1(&nums), 138);
    }

    #[test]
    fn part_2_trivial() {
        let input_str = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2";
        let nums = parse_input(input_str);
        assert_eq!(part_2(&nums), 66);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            let nums = parse_input(&input_str);
            assert_eq!(part_1(&nums), 41926);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            let nums = parse_input(&input_str);
            assert_eq!(part_2(&nums), 24262);
        });
    }
}
