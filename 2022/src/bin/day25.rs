#![feature(custom_test_frameworks)]
#![feature(test)]

use aoc2022::*;

const DAY: usize = 25;

type Parsed = Vec<i64>;

fn main() {
    let input = read_file(DAY);
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    // There is no part 2 on the 25th day.
}

fn parse_input(input: &str) -> Parsed {
    input.lines().map(parse_snafu).collect()
}

fn part_1(parsed: &Parsed) -> String {
    let sum = parsed.iter().sum();
    to_snafu(sum)
}

fn parse_snafu(snafu: &str) -> i64 {
    snafu.bytes().fold(0, |acc, b| {
        acc * 5
            + match b {
                b'-' => -1,
                b'=' => -2,
                b'0'..=b'2' => (b - b'0') as i64,
                _ => panic!("bad input {b}"),
            }
    })
}

fn to_snafu(mut n: i64) -> String {
    match n {
        0 => return '0'.to_string(),
        _ if n < 0 => unimplemented!(),
        _ => (),
    };
    let mut bytes = vec![];
    while n != 0 {
        let carry = match n % 5 {
            rem @ 0..=2 => {
                bytes.push(b'0' + rem as u8);
                0
            }
            3 => {
                bytes.push(b'=');
                1
            }
            4 => {
                bytes.push(b'-');
                1
            }
            _ => unreachable!(),
        };
        n = n / 5 + carry;
    }
    bytes.reverse();
    String::from_utf8(bytes).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;
    use test_case::test_case;

    const TEST_INPUT: &str = "\
        1=-0-2\n\
        12111\n\
        2=0=\n\
        21\n\
        2=01\n\
        111\n\
        20012\n\
        112\n\
        1=-1=\n\
        1-12\n\
        12\n\
        1=\n\
        122\n\
        ";

    test!(part_1() == "2=-1=0");
    bench_parse!(Vec::len, 135);
    bench!(part_1() == "20==1==12=0111=2--20");

    #[test_case("1", 1)]
    #[test_case("2", 2)]
    #[test_case("1=", 3)]
    #[test_case("1-", 4)]
    #[test_case("10", 5)]
    #[test_case("11", 6)]
    #[test_case("12", 7)]
    #[test_case("2=", 8)]
    #[test_case("2-", 9)]
    #[test_case("20", 10)]
    #[test_case("1=0", 15)]
    #[test_case("1-0", 20)]
    #[test_case("1=11-2", 2022)]
    #[test_case("1-0---0", 12345)]
    #[test_case("1121-1110-1=0", 314159265)]
    fn snafu_conversion(snafu: &str, decimal: i64) {
        let n = parse_snafu(snafu);
        assert_eq!(n, decimal);
        assert_eq!(to_snafu(n), snafu);
    }
}
