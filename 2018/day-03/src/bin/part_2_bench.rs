#![feature(test)]
extern crate test;

use std::fs::File;
use std::io::prelude::*;

#[macro_use]
extern crate itertools;
extern crate regex;
use regex::Regex;

#[derive(Debug)]
struct Pt(usize, usize);
// type Pt = (usize, usize);

fn read_input(name: Option<&str>) -> String {
    let input_filename = match name {
        Some(string) => string,
        None => "input.txt"
    };
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


type Input = Vec<(usize, Pt, Pt)>;
type Output = Vec<usize>;

fn parse_input(input: &str) -> Input {
    let re = Regex::new(r"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$").unwrap();
    input.trim().lines().map(|line| {
        let caps = re.captures(line).unwrap();
        assert_eq!(caps.len(), 6);
        let values: Vec<usize> = caps.iter()
            .skip(1)
            .map(|x| x.unwrap().as_str().parse::<usize>().unwrap())
            .collect();
        (values[0],
         Pt(values[1], values[2]),
         Pt(values[3], values[4]))
    }).collect()
}


fn find_max_size(input: &Input) -> usize {
    input.iter().map(|(_, pos, size)| {
        std::cmp::max(pos.0 + size.0, pos.1 + size.1)
    }).max().unwrap()
}


fn process(input: &Input) -> Output {
    let mut out: Vec<usize> = Vec::new();
    let fsize = find_max_size(input);
    println!("fabric size: {}", fsize);
    let mut fabric = vec![0u8; fsize * fsize];
    for (_, pos, size) in input.iter() {
        let rows = pos.1..(pos.1 + size.1);
        let cols = pos.0..(pos.0 + size.0);
        for (row, col) in iproduct!(rows, cols) {
            fabric[row * fsize + col] += 1;
        }
    }
    'claimloop:
    for (i, pos, size) in input.iter() {
        let rows = pos.1..(pos.1 + size.1);
        let cols = pos.0..(pos.0 + size.0);
        for (row, col) in iproduct!(rows, cols) {
            if fabric[row * fsize + col] > 1 {
                continue 'claimloop;
            }
        }
        println!("{}", i);
        out.push(*i);
    }
    out
}


fn main() {
    let input_str = read_input(None);
    let input = parse_input(&input_str);

    let result = process(&input);
    println!("The result is {:?}", result);
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    fn test_input() -> String {
        "#1 @ 1,3: 4x4\n\
            #2 @ 3,1: 4x4\n\
            #3 @ 5,5: 2x2".to_string()
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(&input), vec![3]);
    }

    #[test]
    fn with_real_input() {
        let input_str = read_input(None);
        let input = parse_input(&input_str);
        assert_eq!(process(&input), vec![1097]);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = read_input(None);
        let input = parse_input(&input_str);
        b.iter(|| assert_eq!(process(&input), vec![1097]));
    }

    #[bench]
    #[ignore]
    fn bench_big_input(b: &mut Bencher) {
        let input_str = read_input(Some("big_input.txt"));
        let input = parse_input(&input_str);
        b.iter(|| assert_eq!(process(&input), vec![10642, 14038, 16150, 42813, 56253, 59351, 73875, 89877]));
    }
}
