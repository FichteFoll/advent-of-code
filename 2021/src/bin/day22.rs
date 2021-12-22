#![feature(bool_to_option)]
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

#[derive(Debug, Clone)]
pub struct Cuboid {
    on: bool,
    // TODO consider using Range
    ranges: Vec<RangeInclusive<i32>>,
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
        field.intersect(other.clone());
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
    fn intersect(&mut self, mut other: Cuboid) -> Option<Cuboid> {
        // TODO
        //  - if self & other have the same flag
        //      1. recurse the intersection into self.holes
        //      2. subtract the intersection from other (by holeing).
        //         if other == intersection, return None
        //  - if they have a different flag
        //      1. if self contains other completely, return None, else subtract from other
        //      2. recurse intersection into self.holes
        //      2.

        // TODO build intersection
        let intersection = other.clone();
        // TODO self.ranges == intersection.ranges ????
        //  For equal flags, I could replace self with other,
        //  but what about the holes to prevent double-counting?
        //  And what about differing flags?

        // TODO can we do this more intelligently? scan().nth()?
        let mut remainder = Some(intersection.clone());
        for hole in self.holes.iter_mut() {
            if let Some(curr) = remainder {
                remainder = match hole.intersect(curr) {
                    Some(next) if next.count() != 0 => Some(next),
                    _ => None,
                }
            } else {
                break;
            }
        }
        // let remainder = self.holes.iter_mut().scan(intersection.clone(), |curr, hole| {
        //     if let Some(new) = hole.intersect(*curr) {
        //         *curr = new.clone();
        //         (new.count() != 0).then_some(new)
        //     } else {
        //         None
        //     }
        // }).nth(self.holes.len());

        if self.on != other.on {
            if let Some(rem) = remainder {
                // TODO only if flags differ!
                self.holes.push(rem);
            }
        }

        (intersection.ranges != other.ranges).then(|| {
            // TODO verify
            other.holes.push(intersection);
            other
        })
    }

    fn count(&self) -> usize {
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
}
