#![feature(test, str_split_once, bool_to_option)]

use std::collections::HashMap;

#[macro_use]
extern crate lazy_static;

fn read_input() -> String {
    std::fs::read_to_string("input/day04.txt").expect("can’t read file")
}

type Input<'a> = Vec<HashMap<&'a str, &'a str>>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n\n")
        .map(|group| {
            group.split_whitespace()
                .map(|pair| pair.split_once(':').expect("No ':' found"))
                .collect()
        })
        .collect()
}

fn has_all_fields(pp: &HashMap<&str, &str>) -> bool {
    pp.keys().len().saturating_sub(pp.contains_key("cid").then_some(1).unwrap_or(0)) >= 7
}

lazy_static! {
    static ref EYE_COLORS: [&'static str; 7] = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
}

fn has_valid_fields(pp: &HashMap<&str, &str>) -> bool {
    pp.iter().all(|(&k, &v)| match k {
        "byr" => (1920..=2002).contains(&v.parse().unwrap_or(0)),
        "iyr" => (2010..=2020).contains(&v.parse().unwrap_or(0)),
        "eyr" => (2020..=2030).contains(&v.parse().unwrap_or(0)),
        "hgt" => {
            let (n_str, unit) = v.split_at(v.len() - 2);
            match (n_str.parse().unwrap_or(0), unit) {
                (150..=193, "cm") => true,
                (59..=76, "in") => true,
                _ => false,
            }
        },
        "hcl" => v.len() == 7 && match v.split_at(1) {
            ("#", rest) => rest.chars().all(|c| c.is_digit(16)),
            _ => false,
        },
        "ecl" => (&EYE_COLORS).contains(&v),
        "pid" => v.len() == 9 && v.chars().all(|c| c.is_ascii_digit()),
        "cid" => true,
        _ => false,
    })
}

fn part_1(input: &Input) -> usize {
    input.iter()
        .filter(|pp| has_all_fields(pp))
        .count()
}

fn part_2(input: &Input) -> usize {
    input.iter()
        .filter(|pp| has_all_fields(pp))
        .filter(|pp| has_valid_fields(pp))
        .count()
}

fn main() {
    let input_str = read_input();
    let input = parse_input(&input_str);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    extern crate test;
    use test::Bencher;

    lazy_static! {
        static ref EXAMPLE_INPUT_STR: &'static str = "\
            ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
            byr:1937 iyr:2017 cid:147 hgt:183cm\n\
            \n\
            iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
            hcl:#cfa07d byr:1929\n\
            \n\
            hcl:#ae17e1 iyr:2013\n\
            eyr:2024\n\
            ecl:brn pid:760753108 byr:1931\n\
            hgt:179cm\n\
            \n\
            hcl:#cfa07d eyr:2025 pid:166559648\n\
            iyr:2011 ecl:brn hgt:59in\n\
            ";
    }

    #[test]
    fn test_part_1() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_1(&input), 2);
    }

    #[test]
    fn test_part_2_invalid() {
        let input_str = "\
            eyr:1972 cid:100\n\
            hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
            \n\
            iyr:2019\n\
            hcl:#602927 eyr:1967 hgt:170cm\n\
            ecl:grn pid:012533040 byr:1946\n\
            \n\
            hcl:dab227 iyr:2012\n\
            ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
            \n\
            hgt:59cm ecl:zzz\n\
            eyr:2038 hcl:74454a iyr:2023\n\
            pid:3556412378 byr:2007\n\
            ";
        let input = parse_input(input_str);
        assert_eq!(part_2(&input), 0);
    }

    #[test]
    fn test_part_2_valid() {
        let input_str = "\
            pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
            hcl:#623a2f\n\
            \n\
            eyr:2029 ecl:blu cid:129 byr:1989\n\
            iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
            \n\
            hcl:#888785\n\
            hgt:164cm byr:2001 iyr:2015 cid:88\n\
            pid:545766238 ecl:hzl\n\
            eyr:2022\n\
            \n\
            iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n\
            ";
        let input = parse_input(input_str);
        assert_eq!(part_2(&input), 4);
    }

    #[bench]
    fn bench_parse(b: &mut Bencher) {
        let input_str = read_input();
        b.iter(|| {
            let _ = parse_input(&input_str);
        });
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_1(&input), 247);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 145);
        });
    }
}
