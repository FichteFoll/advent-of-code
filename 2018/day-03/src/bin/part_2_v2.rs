#![feature(test)]
extern crate test;

use std::fs::File;
use std::io::prelude::*;
use std::cmp;

extern crate regex;
use regex::Regex;

#[derive(Debug,PartialEq)]
struct Rect {
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    x2: usize,
    y2: usize,
}

impl Rect {
    fn new(x: usize, y: usize, width: usize, height: usize) -> Rect {
        Rect {x, y, width, height, x2: x + width, y2: y + height}
    }

    fn intersects(&self, other: &Rect) -> bool {
        cmp::min(self.x2, other.x2) > cmp::max(self.x, other.x)
            && cmp::min(self.y2, other.y2) > cmp::max(self.y, other.y)
    }
}


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


type Input = Vec<(usize, Rect)>;
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
         Rect::new(values[1], values[2], values[3], values[4]))
    }).collect()
}


fn process(input: &Input) -> Output {
    let mut out: Vec<usize> = Vec::new();
    'outer:
    for (i, rect1) in input.iter() {
        for (j, rect2) in input.iter() {
            if i != j && rect1.intersects(rect2) {
                continue 'outer;
            }
        }
        out.push(*i)
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
