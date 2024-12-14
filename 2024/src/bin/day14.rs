#![feature(test)]

use aoc2024::*;
use itertools::Itertools;
use parse::parse_input;
use point::Point;

const DAY: usize = 14;

type Parsed = (Vec<(P, P)>, P);
type P = Point::<2>;

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let pts: Vec<_> = input
            .lines()
            .map(|line| {
                let (p_s, v_s) = line.split_once(' ').unwrap();
                (parse_pt(p_s), parse_pt(v_s))
            })
            .collect();
        let max_x = pts.iter().map(|(p, _)| p.x()).max().unwrap() + 1;
        let max_y = pts.iter().map(|(p, _)| p.y()).max().unwrap() + 1;
        (pts, Point([max_x, max_y]))
    }

    fn parse_pt(pt_s: &str) -> P {
        let (x_s, y_s) = pt_s[2..].split_once(',').unwrap();
        Point([x_s.parse().unwrap(), y_s.parse().unwrap()])
    }
}

fn part_1((initial, size): &Parsed) -> usize {
    let mut robots = initial.clone();
    for _ in 0..100 {
        step(&mut robots, size);
    }
    let middle = size / 2;
    robots.into_iter()
        .flat_map(|(p, _)| (p.x() != middle.x() && p.y() != middle.y()).then_some(p))
        .map(|p| (p.x() < middle.x(), p.y() < middle.y()))
        .counts()
        .into_values()
        .product()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn step(robots: &mut [(P, P)], size: &P) {
    for (p, v) in robots.iter_mut() {
        *p.x_mut() = (p.x() + v.x()).rem_euclid(size.x());
        *p.y_mut() = (p.y() + v.y()).rem_euclid(size.y());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        p=0,4 v=3,-3\n\
        p=6,3 v=-1,-3\n\
        p=10,3 v=-1,2\n\
        p=2,0 v=2,-1\n\
        p=0,0 v=1,3\n\
        p=3,0 v=-2,-2\n\
        p=7,6 v=-1,-3\n\
        p=3,0 v=-1,-2\n\
        p=9,3 v=2,3\n\
        p=7,3 v=-1,2\n\
        p=2,4 v=2,-3\n\
        p=9,5 v=-3,-3\n\
        ";

    test!(part_1() == 12);
    // test!(part_2() == 0);
    bench_parse!(|p: &Parsed| (p.0.len(), p.1), (500, Point([101, 103])));
    bench!(part_1() == 211773366);
    // bench!(part_2() == 0);
}
