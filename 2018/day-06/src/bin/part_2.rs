#![feature(test)]

#[macro_use] extern crate itertools;
extern crate test;

use rayon::prelude::*;
use std::fmt::Debug;
use std::cmp::{min, max};
use std::iter::Enumerate;
use std::iter::Iterator;

const DISTANCE: usize = 10000;

struct Pt(i32, i32);

impl Pt {
    fn diff(&self, other: &Pt) -> i32 {
        (self.0 - other.0).abs() + (self.1 - other.1).abs()
    }
}

fn parse_input(input: &str) -> Vec<Pt> {
    input.trim().lines().map(|line| {
        let mut elems = line.split(" ");
        let x = elems.next()
            .expect("no first number")
            .trim_end_matches(',')
            .parse().expect("couldn't parse first namber");
        let y = elems.next()
            .expect("no second number")
            .parse().expect("couldn't parse second number");
        Pt(x, y)
    }).collect()
}


fn get_dimensions(points: &[Pt]) -> (Pt, Pt) {
    let (mut start, mut end) = (Pt(0, 0), Pt(0, 0));
    for pt in points {
        start.0 = min(start.0, pt.0);
        start.1 = min(start.1, pt.1);
        end.0 = max(end.0, pt.0);
        end.1 = max(end.1, pt.1);
    }
    (start, end)
}


fn process(points: &[Pt], max_total: usize) -> usize {
    let (start, end) = get_dimensions(points);
    let test_points: Vec<_> = iproduct!(start.0..=end.0, start.1..=end.1).map(|(x, y)| Pt(x, y)).collect();
    test_points.par_iter()
        .filter_map(|pt| {
            let sum: i32 = points.iter().map(|pt_| pt.diff(pt_)).sum();
            let sum = sum as usize; // idk why I need this
            match sum < max_total {
                true => Some(sum),
                false => None,
            }
        }).count()
}

fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    let input = parse_input(&input_str);
    println!("The result is {:?}", process(&input, DISTANCE));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn simple_input() {
        let input_str = "\
            1, 1\n\
            1, 6\n\
            8, 3\n\
            3, 4\n\
            5, 5\n\
            8, 9";
        let input = parse_input(&input_str);
        assert_eq!(process(&input, 32), 16);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(process(&input, DISTANCE), 45046);
        });
    }
}
