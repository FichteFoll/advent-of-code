#![feature(test)]

#[macro_use] extern crate itertools;
extern crate test;

use rayon::prelude::*;
use std::cmp::{min, max};
use std::iter::Enumerate;

trait SoleMin: Iterator {
    fn sole_min_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where
        B: Ord,
        F: FnMut(&Self::Item) -> B;
}

impl<I> SoleMin for Enumerate<I>
    where I: ExactSizeIterator + DoubleEndedIterator,

{
    fn sole_min_by_key<B, F>(self, mut f: F) -> Option<Self::Item>
    where
        B: Ord,
        F: FnMut(&Self::Item) -> B,
    {
        let mut vec: Vec<Self::Item> = self.collect();
        vec.sort_unstable_by_key(|x| f(x));
        let mut vec: Vec<_> = vec.drain(..2).collect();
        match (vec.pop(), vec.pop()) {
            (Some(second), Some(first)) => {
                if f(&first) == f(&second) {
                    None
                } else {
                    Some(first)
                }
            },
            (Some(first), None) => Some(first),
            _ => None,
        }
    }
}

struct Pt(i32, i32);

impl Pt {
    fn distance(&self, other: &Pt) -> i32 {
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


fn process(points: &[Pt]) -> usize {
    let mut counts = vec![0; points.len()];
    let mut exclude = vec![false; points.len()];

    let (start, end) = get_dimensions(points);
    let test_points: Vec<_> = iproduct!(start.0..=end.0, start.1..=end.1).map(|(x, y)| Pt(x, y)).collect();
    test_points.par_iter()
        .filter_map(|pt| {
            let result = points.iter()
                .map(|pt_| pt.distance(pt_))
                .enumerate().sole_min_by_key(|&(_, item)| item);
            if let Some((index, _)) = result {
                let is_outer = pt.0 == start.0 || pt.0 == end.0 || pt.1 == start.1 || pt.1 == end.1;
                Some((index, is_outer))
            } else {
                None
            }
        })
        .collect::<Vec<_>>().into_iter() // join the parallelized vector
        .for_each(|(index, is_outer)| {
            counts[index] += 1;
            if is_outer {
                exclude[index] = true;
            }
        });

    let (&max_point, _) = counts.iter().zip(exclude)
        .max_by_key(|(&c, exclude)| {
            match exclude {
                true => 0,
                false => c,
            }
        }).unwrap();
    max_point
}

fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    let input = parse_input(&input_str);
    println!("The result is {:?}", process(&input));
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
        assert_eq!(process(&input), 17);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(process(&input), 3238);
        });
    }
}
