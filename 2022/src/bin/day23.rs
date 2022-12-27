#![feature(test)]

use itertools::Itertools;
use std::collections::hash_map::Entry;

use aoc2022::collections::*;
use aoc2022::coord::Point;
use aoc2022::*;

const DAY: usize = 23;

type Parsed = HashSet<Point<2>>;

main!();

fn parse_input(input: &str) -> Parsed {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.bytes()
                .enumerate()
                .filter_map(move |(x, b)| (b == b'#').then_some((x, y).into()))
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let mut field = parsed.clone();
    for i in 0..10 {
        field = round(&field, i).unwrap();
    }
    count_empty(&field)
}

fn part_2(parsed: &Parsed) -> usize {
    let mut field = parsed.clone();
    for i in 0.. {
        match round(&field, i) {
            Some(next_field) => field = next_field,
            _ => return i + 1,
        }
    }
    unreachable!()
}

fn round(field: &Parsed, i: usize) -> Option<Parsed> {
    let mut next_field = HashSet::default();
    let mut next_to_cur = HashMap::default();
    let mut contested = HashSet::default();

    for pt in field {
        let Some(next_pt) = next_point(field, pt, i) else {
            next_field.insert(*pt);
            continue;
        };
        match next_to_cur.entry(next_pt) {
            Entry::Occupied(_) => {
                contested.insert(next_pt);
                next_field.insert(*pt);
            }
            Entry::Vacant(entry) => {
                entry.insert(*pt);
            }
        }
    }
    // Subtract points from total that will not be moved.
    let moved = field.len() - next_field.len() - contested.len();
    if moved == 0 {
        return None;
    }
    for (to, from) in next_to_cur.into_iter() {
        next_field.insert(match contested.contains(&to) {
            true => from,
            false => to,
        });
    }
    Some(next_field)
}

static LOOKUP: [[Point<2>; 3]; 4] = [
    // first item is the direction we want to move in
    [Point::<2>::N, Point::<2>::NE, Point::<2>::NW],
    [Point::<2>::S, Point::<2>::SE, Point::<2>::SW],
    [Point::<2>::W, Point::<2>::NW, Point::<2>::SW],
    [Point::<2>::E, Point::<2>::NE, Point::<2>::SE],
];

fn next_point(map: &Parsed, pt: &Point<2>, i: usize) -> Option<Point<2>> {
    let candidates: Vec<_> = (0..4)
        .filter_map(|j| {
            let lookup = LOOKUP[(i + j) % LOOKUP.len()];
            lookup
                .iter()
                .all(|dir| !map.contains(&(pt + dir)))
                .then_some(pt + &lookup[0])
        })
        .collect();
    if candidates.is_empty() || candidates.len() == 4 {
        None
    } else {
        Some(candidates[0])
    }
}

fn count_empty(field: &Parsed) -> usize {
    let (xs, ys): (Vec<_>, Vec<_>) = field.iter().map(|pt| (pt.x(), pt.y())).unzip();
    let (min_x, max_x) = xs.into_iter().minmax().into_option().unwrap();
    let (min_y, max_y) = ys.into_iter().minmax().into_option().unwrap();
    min_x.abs_diff(max_x + 1) as usize * min_y.abs_diff(max_y + 1) as usize - field.len()
}

#[allow(unused)]
fn print_field(field: &Parsed) {
    let (xs, ys): (Vec<_>, Vec<_>) = field.iter().map(|pt| (pt.x(), pt.y())).unzip();
    let (min_x, max_x) = xs.into_iter().minmax().into_option().unwrap();
    let (min_y, max_y) = ys.into_iter().minmax().into_option().unwrap();
    println!("-------");
    for y in min_y..=max_y {
        for x in min_x..=max_x {
            if field.contains(&Point([x, y])) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!("-------");
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        ....#..\n\
        ..###.#\n\
        #...#.#\n\
        .#...##\n\
        #.###..\n\
        ##.#.##\n\
        .#..#..\n\
        ";

    test!(part_1() == 110);
    test!(part_2() == 20);
    bench_parse!(HashSet::len, 2648);
    bench!(part_1() == 4158);
    bench!(part_2() == 1014);
}
