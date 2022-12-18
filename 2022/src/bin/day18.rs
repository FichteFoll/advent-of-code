#![feature(test)]

use std::collections::VecDeque;

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;
use itertools::Itertools;

const DAY: usize = 18;

type Parsed = HashSet<Point<3>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            line.split(',')
                .map(|s| s.parse().unwrap())
                .collect::<Vec<_>>()
                .try_into()
                .unwrap()
        })
        .collect()
}

fn part_1(points: &Parsed) -> usize {
    points
        .iter()
        .map(|pt| {
            pt.direct_neighbors()
                .into_iter()
                .filter(|nb| !points.contains(nb))
                .count()
        })
        .sum()
}

fn part_2(points: &Parsed) -> u32 {
    let (xs, (ys, zs)): (Vec<_>, (Vec<_>, Vec<_>)) =
        points.iter().map(|pt| (pt.x(), (pt.y(), pt.z()))).unzip();
    let ranges = [xs, ys, zs]
        .map(|nums| nums.into_iter().minmax().into_option().unwrap())
        .map(|(min, max)| min - 1..=max + 1);

    let start: Point<3> = ranges.clone().map(|range| *range.start()).into();
    let mut seen: HashSet<_> = Default::default();
    seen.insert(start);
    let mut queue: VecDeque<_> = [start].into();
    let mut found: u32 = 0;
    while let Some(current) = queue.pop_front() {
        let next_pts = current
            .direct_neighbors()
            .into_iter()
            .filter(|next| (0..3).all(|i| ranges[i].contains(&next.0[i])))
            .filter(|next| !seen.contains(next))
            .filter(|next| {
                let is_lava = points.contains(next);
                found += is_lava as u32;
                !is_lava
            })
            .collect::<Vec<_>>();
        queue.extend(next_pts.iter().cloned());
        seen.extend(next_pts);
    }
    found
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        2,2,2\n\
        1,2,2\n\
        3,2,2\n\
        2,1,2\n\
        2,3,2\n\
        2,2,1\n\
        2,2,3\n\
        2,2,4\n\
        2,2,6\n\
        1,2,5\n\
        3,2,5\n\
        2,1,5\n\
        2,3,5\n\
        ";

    test!(part_1() == 64);
    test!(part_2() == 58);
    bench_parse!(HashSet::len, 2741);
    bench!(part_1() == 4628);
    bench!(part_2() == 2582);
}
