#![feature(test)]

use aoc2021::*;
use parse::parse_input;

const DAY: usize = 16;

// consider https://crates.io/crates/bitvec
type RawPackage = Vec<bool>;
type Parsed = RawPackage;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .chars()
            .map(|c| u8::from_str_radix(&c.to_string(), 16).unwrap())
            .flat_map(|nibble| (0..4).rev() .map(|i| nibble >> i & 1 == 1).collect::<Vec<_>>())
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> usize {
    let pkg = Package::parse(&parsed);
    // TODO sum over versions
    0
}

fn part_2(_parsed: &Parsed) -> usize {
    0
}

const TYPE_LITERAL: u8 = 4;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Package {
    header: Header,
    body: Body,
    length: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Header {
    version: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Body {
    Literal(usize),
    Operator{ type_: u8, children: Vec<Package> },
}

#[derive(Clone, Copy, Debug)]
enum Length {
    Bits(usize),
    Packets(usize),
}

impl Package {
    fn parse(slice: &[bool]) -> Package {
        let version = to_number!(slice[..3] => u8);
        let header = Header { version };
        let type_ = to_number!(slice[3..6] => u8);
        println!("slice: {slice:?}");
        println!("version: {version}, type_: {type_}");
        if type_ == TYPE_LITERAL {
            let (body, body_length) = Body::parse_literal(&slice[6..]);
            Package { header, body, length: 6 + body_length }
        } else {
            let (body, body_length) = Body::parse_operator(type_, &slice[6..]);
            Package { header, body, length: 6 + body_length }
        }
    }
}

impl Body {
    fn parse_literal(slice: &[bool]) -> (Self, usize) {
        let (literal, size) = slice.chunks_exact(5)
            .map(|sub| (sub[0], to_number!(sub[1..] => usize)))
            .scan(true, |cont, (cont2, n)| match *cont {
                true => { *cont &= cont2; Some(n) },
                false => None,
            })
            .fold((0, 0), |(acc, size), n| ((acc << 4) + n, size + 5));
        (Body::Literal(literal), size)
    }

    fn parse_operator(type_: u8, slice: &[bool]) -> (Self, usize) {
        let raw_length = match &slice[0] {
            false => Length::Bits(to_number!(slice[1..16] => usize)),
            true => Length::Packets(to_number!(slice[1..12] => usize)),
        };
        let mut length = raw_length.bit_length() + 1;
        println!("{raw_length:?}, {length}");
        println!("length slice: {:?}", &slice[1..16]);
        let children = match raw_length {
            Length::Bits(bits) => {
                let mut sub_slice = &slice[length..length + bits];
                let expected_length = length + bits;
                let mut children = Vec::new();
                while !sub_slice.is_empty() {
                    let package = Package::parse(&sub_slice);
                    length += package.length;
                    sub_slice = &sub_slice[package.length..];
                    children.push(package);
                }
                assert_eq!(length, expected_length);
                children
            },
            _ => unimplemented!(),
        };
        // parse subpkgs
        let body = Body::Operator{ type_, children };
        (body, length)
    }
}

impl Length {
    fn bit_length(&self) -> usize {
        match self {
            Length::Bits(_) => 15,
            Length::Packets(_) => 11,
        }
    }
}

#[macro_export]
macro_rules! to_number {
    ($slice:expr => $ident:ident) => {
        $slice.iter().fold(0 as $ident, |acc, &bit| acc << 1 | (bit as $ident))
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    #[test]
    fn test_parse_literal() {
        let input = "D2FE28";
        let parsed = parse_input(input);
        let pkg = Package::parse(&parsed);
        assert_eq!(pkg.header.version, 6);
        match pkg.body {
            Body::Literal(n) => assert_eq!(n, 2021),
            _ => unreachable!(),
        }
        assert_eq!(pkg.length, 21);
    }

    #[test]
    fn test_parse_operator_id_0() {
        let input = "38006F45291200";
        let parsed = parse_input(input);
        let pkg = Package::parse(&parsed);
        let expected = Package {
            header: Header { version: 1 },
            body: Body::Operator {
                type_: 6,
                children: vec![
                    Package {
                        header: Header { version: 6 },
                        body: Body::Literal(10),
                        length: 11,
                    },
                    Package {
                        header: Header { version: 2 },
                        body: Body::Literal(20),
                        length: 16,
                    },
                ],
            },
            length: 49,
        };
        assert_eq!(pkg, expected);
    }

    #[test]
    fn test_parse_operator_id_1() {
        let input = "EE00D40C823060";
        let parsed = parse_input(input);
        let pkg = Package::parse(&parsed);
        let expected = Package {
            header: Header { version: 7 },
            body: Body::Operator {
                type_: 3,
                children: vec![
                    Package {
                        header: Header { version: 5 },
                        body: Body::Literal(1),
                        length: 11,
                    },
                    Package {
                        header: Header { version: 4 },
                        body: Body::Literal(2),
                        length: 11,
                    },
                    Package {
                        header: Header { version: 1 },
                        body: Body::Literal(3),
                        length: 11,
                    },
                ],
            },
            length: 51,
        };
        assert_eq!(pkg, expected);
    }

    test!(
        op_op_literal,
        "8A004A801A8002F478",
        part_1() == 16
    );
    test!(
        op_two_sub_two_literals,
        "620080001611562C8802118E34",
        part_1() == 12
    );
    test!(
        op_two_sub_two_literals_length_type,
        "C0015000016115A2E0802F182340",
        part_1() == 23
    );
    test!(
        op_op_five_literals,
        "A0016C880162017C3686B18A3D4780",
        part_1() == 31
    );
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 1400 * 4);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);
}
