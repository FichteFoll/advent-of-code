#![feature(bool_to_option)]
#![feature(get_mut_unchecked)]
#![feature(drain_filter)]
#![feature(test)]

use std::ops::RangeInclusive;

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 22;

type Parsed = Vec<Cuboid>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

type Ranges = Vec<RangeInclusive<i32>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cuboid {
    on: bool,
    // TODO consider using Range if it makes math easier
    ranges: Ranges,
    holes: Vec<Cuboid>,
}

mod parse {
    use std::str::FromStr;
    use std::num::ParseIntError;

    use super::*;
    use thiserror::Error;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .lines()
            .map(|line| line.parse().unwrap())
            .collect()
    }

    #[derive(Error, Debug)]
    pub enum ParseError {
        #[error("No space char found")]
        NoSpace,
        #[error("Couldn't parse range")]
        BadRange,
        #[error("Couldn't parse integer: {0:?}")]
        BadInt(#[from] ParseIntError),
        #[error("Expected 3 ranges, found {0}")]
        InvalidDimensions(usize),
    }

    impl FromStr for Cuboid {
        type Err = ParseError;
        fn from_str(line: &str) -> Result<Self, Self::Err> {
            let (flag, ranges_s) = line.split_once(' ').ok_or(ParseError::NoSpace)?;
            let on = flag == "on";
            let ranges: Vec<_> = ranges_s.split(',').map(parse_range).collect::<Result<_, _>>()?;
            match ranges.len() {
                3 => Ok(Cuboid { on, ranges, holes: vec![] }),
                d => Err(ParseError::InvalidDimensions(d)),
            }
        }
    }

    fn parse_range(s: &str) -> Result<RangeInclusive<i32>, ParseError> {
        let (_, range) = s.split_once('=').ok_or(ParseError::BadRange)?;
        let split = range.split_once("..").ok_or(ParseError::BadRange)?;
        Ok(split.0.parse()?..=split.1.parse()?)
    }
}

fn part_1(parsed: &Parsed) -> usize {
    println!("{parsed:?}");
    let mut field = Cuboid::start(-50..=50);
    for other in parsed.iter() {
        field.merge(other.clone());
    }
    field.count_holes()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

impl Cuboid {
    fn start(range: RangeInclusive<i32>) -> Self {
        Cuboid {
            on: false,
            ranges: vec![range.clone(), range.clone(), range.clone()],
            holes: vec![]
        }
    }

    // Intersect self with other, consuming it
    // and returning the holed-remainder, if any.
    //
    fn merge(&mut self, mut other: Cuboid) -> (bool, Option<Cuboid>) {
        // TODO
        //  - if self & other have the same flag
        //      1. recurse the intersection into self.holes
        //      2. subtract the intersection from other (by holeing).
        //         if other == intersection, return None
        //  - if they have a different flag
        //      1. if self contains other completely, return None, else subtract from other
        //      2. recurse intersection into self.holes
        //      2.
        println!("merging {other:?} into {self:?}");

        let same_flag = self.on == other.on;
        if let Some(intersection) = other.intersection_c(&self.ranges) {
            let intersection_ranges = intersection.ranges.clone();

            if self.ranges == intersection.ranges {
                // Other should have been reduced sufficiently to account for potential overlaps
                // with earlier holes, while entirely covering self.
                // Thus, we remove self and pass other to the next hole.
                (true, Some(other))
            } else {
                let mut remainder = Some(intersection);
                // recurse the intersection into our holes,
                // keeping track of the remainder
                // and removing holes that the remainder covers completely.
                self.holes.drain_filter(|hole| {
                    let mut remove = false;
                    remainder = remainder.clone().and_then(|curr| { // TODO remove this .clone
                        let new_remainder;
                        (remove, new_remainder) = hole.merge(curr);
                        println!("remove: {remove}; new remainder: {new_remainder:?}");
                        new_remainder.filter(|r| r.count() != 0) // TODO can this even happen?
                    });
                    remove
                });

                if !same_flag {
                    if let Some(rem) = remainder {
                        self.holes.push(rem);
                    }
                    // When the flag is the same,
                    // we don't need need to do anything with the remainder
                    // because everything remaining is already "on" or "off".
                }

                let next_return = (intersection_ranges != other.ranges).then(|| {
                    // Other has not been completely consumed, so consume the intersection of self.
                    // TODO do I need to pass holes here? i dont think so
                    other.merge(Cuboid { on: !other.on, ranges: intersection_ranges, holes: vec![] });
                    other
                });
                (false, next_return)
            }
        } else {
            // no intersection, nothing to do
            (false, Some(other))
        }
    }

    // fn intersection(&self, other: &Cuboid) -> Option<(Cuboid, Cuboid)> {
    fn intersection(&self, other: &Cuboid) -> Option<Ranges> {
        let ranges = Cuboid::ranges_intersection(&self.ranges, &other.ranges);
        ranges.iter()
            .all(|r| !r.clone().is_empty())
            .then(|| {
                ranges
                // (self.without_ranges(&ranges), (other.without_ranges(&ranges)))
            })
    }

    fn intersection_c(&self, other_ranges: &Ranges) -> Option<Cuboid> {
        let ranges = Cuboid::ranges_intersection(&self.ranges, other_ranges);
        ranges.iter()
            .all(|r| !r.clone().is_empty())
            .then(|| {
                Cuboid {
                    on: self.on,
                    ranges: ranges.clone(),
                    holes: self.holes.iter()
                        .filter_map(|hole| hole.intersection_c(&ranges))
                        .collect(),
                }
            })
    }

    fn ranges_intersection(a: &Ranges, b: &Ranges) -> Ranges {
        a.iter().zip(b.iter())
            .map(|(a, b)| *a.start().max(b.start())..=*a.end().min(b.end()))
            .collect()
    }

    fn count(&self) -> usize {
        self.ranges.iter().map(|r| r.clone().count()).product::<usize>()
            .checked_sub(self.count_holes())
            .unwrap_or_else(|| panic!("underflow for {self:?}"))
    }

    fn count_holes(&self) -> usize {
        self.holes.iter().map(|h| h.count()).sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        on x=-20..26,y=-36..17,z=-47..7\n\
        on x=-20..33,y=-21..23,z=-26..28\n\
        on x=-22..28,y=-29..23,z=-38..16\n\
        on x=-46..7,y=-6..46,z=-50..-1\n\
        on x=-49..1,y=-3..46,z=-24..28\n\
        on x=2..47,y=-22..22,z=-23..27\n\
        on x=-27..23,y=-28..26,z=-21..29\n\
        on x=-39..5,y=-6..47,z=-3..44\n\
        on x=-30..21,y=-8..43,z=-13..34\n\
        on x=-22..26,y=-27..20,z=-29..19\n\
        off x=-48..-32,y=26..41,z=-47..-37\n\
        on x=-12..35,y=6..50,z=-50..-2\n\
        off x=-48..-32,y=-32..-16,z=-15..-5\n\
        on x=-18..26,y=-33..15,z=-7..46\n\
        off x=-40..-22,y=-38..-28,z=23..41\n\
        on x=-16..35,y=-41..10,z=-47..6\n\
        off x=-32..-23,y=11..30,z=-14..3\n\
        on x=-49..-5,y=-3..45,z=-29..18\n\
        off x=18..30,y=-20..-8,z=-3..13\n\
        on x=-41..9,y=-7..43,z=-33..15\n\
        on x=-54112..-39298,y=-85059..-49293,z=-27449..7877\n\
        on x=967..23432,y=45373..81175,z=27513..53682\n\
        ";

    test!(part_1() == 590784);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 420);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);

    #[test]
    fn count() {
        let cub: Cuboid = "on x=-20..26,y=-36..17,z=-47..7".parse().unwrap();
        assert_eq!(cub.count(), 139590);
    }

    #[test]
    fn count_with_hole() {
        let mut cub: Cuboid = "on x=-20..26,y=-36..17,z=-47..7".parse().unwrap();
        let hole: Cuboid = "off x=0..26,y=-36..17,z=-47..7".parse().unwrap();
        assert_eq!(hole.count(), 80190);
        cub.holes.push(hole);
        assert_eq!(cub.count(), 139590 - 80190);
    }

    #[test]
    fn intersection_full() {
        let a: Cuboid = "on x=-20..26,y=-36..17,z=-47..7".parse().unwrap();
        let b: Cuboid = "off x=0..26,y=-36..17,z=-47..7".parse().unwrap();
        let expected = Some(b.ranges.clone());
        assert_eq!(&a.intersection(&b), &expected);
        assert_eq!(&b.intersection(&a), &expected);
    }

    #[test]
    fn intersection_partially() {
        let a: Cuboid = "on x=0..3,y=0..3,z=0..3".parse().unwrap();
        let b: Cuboid = "on x=0..1,y=2..4,z=-1..0".parse().unwrap();
        let expected = Some(vec![0..=1, 2..=3, 0..=0]);
        assert_eq!(&a.intersection(&b), &expected);
        assert_eq!(&b.intersection(&a), &expected);
    }

    #[test]
    fn intersection_none() {
        let a: Cuboid = "on x=-20..26,y=-36..17,z=-47..7".parse().unwrap();
        let b: Cuboid = "off x=27..206,y=-36..17,z=-47..7".parse().unwrap();
        assert_eq!(a.intersection(&b), None);
        assert_eq!(b.intersection(&a), None);
    }

    #[test]
    fn merge_full() {
        let mut cub: Cuboid = "on x=-20..26,y=-36..17,z=-47..7".parse().unwrap();
        let hole: Cuboid = "off x=0..26,y=-36..17,z=-47..7".parse().unwrap();
        assert_eq!(hole.count(), 80190);
        assert_eq!(cub.merge(hole), (false, None));
        assert_eq!(cub.count(), 139590 - 80190);
    }

    #[test]
    fn merge_full_same_flag() {
        let mut cub: Cuboid = "on x=-20..26,y=-36..17,z=-47..7".parse().unwrap();
        let hole: Cuboid = "on x=0..26,y=-36..17,z=-47..7".parse().unwrap();
        assert_eq!(hole.count(), 80190);
        assert_eq!(cub.merge(hole), (false, None));
        assert_eq!(cub.count(), 139590);
    }

    #[test]
    fn merge_partial() {
        let mut cub: Cuboid = "on x=0..3,y=0..3,z=0..3".parse().unwrap();
        let hole: Cuboid = "off x=0..3,y=-1..5,z=1..2".parse().unwrap();
        let expected = Cuboid {
            on: false,
            ranges: vec![0..=3, -1..=5, 1..=2],
            holes: vec![
                Cuboid { on: true, ranges: vec![0..=3, 0..=3, 1..=2], holes: vec![] },
            ],
        };
        assert_eq!(hole.count(), 4 * 7 * 2);
        assert_eq!(cub.merge(hole), (false, Some(expected)));
        assert_eq!(cub.count(), 2 * 4 * 4);
    }

    #[test]
    fn merge_partial_same_flag() {
        let mut cub: Cuboid = "on x=0..3,y=0..3,z=0..3".parse().unwrap();
        let hole: Cuboid = "on x=0..3,y=-1..5,z=1..2".parse().unwrap();
        let expected = Cuboid {
            on: true,
            ranges: vec![0..=3, -1..=5, 1..=2],
            holes: vec![
                Cuboid { on: false, ranges: vec![0..=3, 0..=3, 1..=2], holes: vec![] },
            ],
        };
        assert_eq!(hole.count(), 4 * 7 * 2);
        assert_eq!(expected.count(), 4 * 3 * 2);
        assert_eq!(cub.merge(hole), (false, Some(expected)));
        assert_eq!(cub.count(), 4 * 4 * 4);
    }

    #[test]
    fn merge_simple_example() {
        let input = "\
            on x=10..12,y=10..12,z=10..12\n\
            on x=11..13,y=11..13,z=11..13\n\
            off x=9..11,y=9..11,z=9..11\n\
            on x=10..10,y=10..10,z=10..10\n\
            ";
        let cubs = parse_input(input);
        let mut field = Cuboid::start(-50..=50);
        field.merge(cubs[0].clone());
        assert_eq!(field.count_holes(), 27);
        field.merge(cubs[1].clone());
        assert_eq!(field.count_holes(), 27 + 19);
        field.merge(cubs[2].clone());
        assert_eq!(field.count_holes(), 27 + 19 - 8);
        field.merge(cubs[3].clone());
        println!("{field:?}");
        assert_eq!(field.count_holes(), 39);
    }

    // TODO test subsequent merges with and without overlap
}
