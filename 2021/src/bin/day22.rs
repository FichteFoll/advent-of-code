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
    let mut field = Cuboid {
        on: false,
        ranges: vec![-50..=50, -50..=50, -50..=50],
        holes: vec![]
    };
    for other in parsed.iter() {
        field.merge(other.clone());
    }
    field.count_holes()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

impl Cuboid {
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

        let same_flag = self.on == other.on;
        if let Some(intersection) = other.intersection(self) {

            if self.ranges == intersection {
                // Other should have been reduced sufficiently to account for potential overlaps
                // with earlier holes, while entirely covering self.
                // Thus, we remove self and pass other to the next hole.
                (true, Some(other))
            } else {
                let mut remainder = Some(Cuboid { ranges: intersection.clone(), ..other.clone() });
                self.holes.drain_filter(|hole| {
                    let mut remove = false;
                    remainder = remainder.clone().and_then(|curr| { // TODO remove this .clone
                        let new_remainder;
                        (remove, new_remainder) = hole.merge(curr);
                        new_remainder.filter(|r| r.count() != 0) // TODO can this even happen?
                    });
                    remove
                });
                // let mut remainder = Rc::new(Some(intersection.clone()));
                // self.holes.drain_filter(|hole| {
                //     let mut remove = false;
                //     // SAFETY: we have a single thread
                //     let rem_mut = unsafe { Rc::get_mut_unchecked(&mut remainder) };
                //     *rem_mut = rem_mut.clone().and_then(|curr| {
                //         let new_remainder;
                //         (remove, new_remainder) = hole.intersect(curr);
                //         new_remainder.filter(|r| r.count() != 0)
                //     });
                //     remove
                // });
                // for hole in self.holes.iter_mut() {
                //     if let Some(curr) = remainder {
                //         remainder = match hole.intersect(curr) {
                //             Some(next) if next.count() != 0 => Some(next),
                //             _ => None,
                //         }
                //     } else {
                //         break;
                //     }
                // }
                // let remainder = self.holes.iter_mut().scan(intersection.clone(), |curr, hole| {
                //     if let Some(new) = hole.intersect(*curr) {
                //         *curr = new.clone();
                //         (new.count() != 0).then_some(new)
                //     } else {
                //         None
                //     }
                // }).nth(self.holes.len());

                let next_return = (intersection != other.ranges).then(|| {
                    // Other has not been completely consumed, so merge it with the intersection of self.
                    // TODO verify
                    other.merge(Cuboid { ranges: intersection, ..self.clone() });
                    // other.holes.push(Cuboid { ranges: intersection, ..self.clone() });
                    other
                });
                if !same_flag {
                    if let Some(rem) = remainder {
                        self.holes.push(rem);
                    }
                }

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

    fn ranges_intersection(a: &Ranges, b: &Ranges) -> Ranges {
        a.iter().zip(b.iter())
            .map(|(a, b)| *a.start().max(b.start())..=*a.end().min(b.end()))
            .collect()
    }

    // fn without_ranges(&self, ranges: &Ranges) -> Cuboid {
    //     // TODO strip unnecessary holes => shouldn't be needed, I hope
    //     let holes =
    //     Cuboid {
    //         on: self.on,
    //         ranges: self.ranges.clone(),
    //         holes,
    //     }
    // }


    fn count(&self) -> usize {
        println!("counting {self:?}");
        self.ranges.iter().map(|r| r.clone().count()).product::<usize>()
            - self.count_holes() // if this underflows, we done goofed
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
}
