#![feature(test)]

#[macro_use] extern crate derive_error;
#[macro_use] extern crate itertools;
extern crate test;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::str::FromStr;

#[derive(Debug, Error)]
enum PtError {
    ParseError
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Pt(isize, isize, isize, isize);

impl FromStr for Pt {
    type Err = Box<Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(',');
        Ok(Pt(
            parts.next().ok_or(PtError::ParseError)?.parse()?,
            parts.next().ok_or(PtError::ParseError)?.parse()?,
            parts.next().ok_or(PtError::ParseError)?.parse()?,
            parts.next().ok_or(PtError::ParseError)?.parse()?,
        ))
    }
}


impl Pt {
    fn distance(&self, other: &Pt) -> isize {
        (self.0 - other.0).abs()
            + (self.1 - other.1).abs()
            + (self.2 - other.2).abs()
            + (self.3 - other.3).abs()
    }
}


fn part_1(input_str: &str) -> usize {
    let points: Vec<Pt> = input_str.lines()
        .map(|line| line.parse().expect("bad line"))
        .collect();

    // Something like https://en.wikipedia.org/wiki/DBSCAN
    let mut neighborhood: HashMap<&Pt, Vec<&Pt>> = HashMap::new();
    for i in 0..points.len() {
        for j in (i + 1)..points.len() {
            let (pt1, pt2) = (&points[i], &points[j]);
            let d = pt1.distance(pt2);
            if d <= 3 {
                neighborhood.entry(pt1).or_insert_with(Vec::new).push(pt2);
                neighborhood.entry(pt2).or_insert_with(Vec::new).push(pt1);
            }
        }
    }

    let mut clusters: Vec<HashSet<&Pt>> = Vec::new();
    let mut pt_remaining: HashSet<_> = points.iter().collect();
    while !pt_remaining.is_empty() {
        let mut active: HashSet<&Pt> = HashSet::new();
        // Pick any remaining random point
        active.insert(pt_remaining.iter().next().unwrap());

        let mut cluster = active.clone();
        while !active.is_empty() {
            pt_remaining = &pt_remaining - &active;
            let next_active: HashSet<_> = active.iter()
                .filter_map(|pt| neighborhood.get(pt))
                .flatten() // Can't map, filter *and* flatten at the same time
                .cloned() // flatten yields references?
                .collect();
            active = &next_active & &pt_remaining;
            cluster = &cluster | &active;
        }
        clusters.push(cluster);
    }
    clusters.len()
}


fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    println!("Part 1: {:?}", part_1(&input_str));
    // println!("Part 2: {:?}", part_2());
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn part_1_trivial() {
        let input_str = "\
             0,0,0,0\n\
             3,0,0,0\n\
             0,3,0,0\n\
             0,0,3,0\n\
             0,0,0,3\n\
             0,0,0,6\n\
             9,0,0,0\n\
            12,0,0,0";
        assert_eq!(part_1(input_str), 2);
    }

    #[test]
    fn part_1_example_1() {
        let input_str = "\
            -1,2,2,0\n\
            0,0,2,-2\n\
            0,0,0,-2\n\
            -1,2,0,0\n\
            -2,-2,-2,2\n\
            3,0,2,-1\n\
            -1,3,2,2\n\
            -1,0,-1,0\n\
            0,2,1,-2\n\
            3,0,0,0";
        assert_eq!(part_1(input_str), 4);
    }

    #[test]
    fn part_1_example_2() {
        let input_str = "\
            1,-1,0,1\n\
            2,0,-1,0\n\
            3,2,-1,0\n\
            0,0,3,1\n\
            0,0,-1,-1\n\
            2,3,-2,0\n\
            -2,2,0,0\n\
            2,-2,0,-1\n\
            1,-1,0,-1\n\
            3,2,0,2";
        assert_eq!(part_1(input_str), 3);
    }

    #[test]
    fn part_1_example_3() {
        let input_str = "\
            1,-1,-1,-2\n\
            -2,-2,0,1\n\
            0,2,1,3\n\
            -2,3,-2,1\n\
            0,2,3,-2\n\
            -1,-1,1,-2\n\
            0,-2,-1,0\n\
            -2,2,3,-1\n\
            1,2,2,0\n\
            -1,-2,0,-2";
        assert_eq!(part_1(input_str), 8);
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(part_1(&input_str), 388);
        });
    }
}
