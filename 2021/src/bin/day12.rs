#![feature(test)]

use std::collections::HashSet;

use multimap::MultiMap;

use aoc2021::*;

const DAY: usize = 12;

type Parsed = MultiMap<String, String>;

#[derive(Clone, Debug)]
struct Path<'a> {
    path: Vec<&'a str>,
    visited: HashSet<&'a str>,
    allow_twice: bool,
}

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .split('\n')
        .map(|line| line.split_once('-').unwrap_or_else(|| panic!("no '-' in {line}")))
        .flat_map(|(a, b)| [(a.into(), b.into()), (b.into(), a.into())])
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let start = vec![Path { path: vec!["start"], visited: ["start"].into(), allow_twice: false }];
    bfs(parsed, start)
}

fn part_2(parsed: &Parsed) -> usize {
    let start = vec![Path { path: vec!["start"], visited: ["start"].into(), allow_twice: true }];
    bfs(parsed, start)
}

fn bfs<'a>(parsed: &'a Parsed, mut cur_paths: Vec<Path<'a>>) -> usize {
    let mut count = 0;
    while !cur_paths.is_empty() {
        let mut next_paths = vec![];
        for path in cur_paths {
            let last = path.path.last().unwrap().to_owned();
            let children = parsed.get_vec(last).unwrap();
            let extend = children.iter()
                .filter(|&c| match c.as_str() {
                    "start" => false,
                    "end" => { count += 1; false },
                    s if is_lower_case(s) => !path.visited.contains(s) || path.allow_twice,
                    _ => true,
                })
                .map(|c| {
                    let mut new_path = path.clone();
                    new_path.path.push(c);
                    if is_lower_case(c) {
                        new_path.allow_twice &= new_path.visited.insert(c);
                    }
                    new_path
                });
            next_paths.extend(extend);
        }
        cur_paths = next_paths;
    }
    count
}

fn is_lower_case(s: &str) -> bool {
    s.to_lowercase() == s
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT_SIMPLE: &str = "\
        start-A\n\
        start-b\n\
        A-c\n\
        A-b\n\
        b-d\n\
        A-end\n\
        b-end\n\
        ";

    const TEST_INPUT_SLIGHTLY_LARGER: &str = "\
        dc-end\n\
        HN-start\n\
        start-kj\n\
        dc-start\n\
        dc-HN\n\
        LN-dc\n\
        HN-end\n\
        kj-sa\n\
        kj-HN\n\
        kj-dc\n\
        ";

    const TEST_INPUT_EVEN_LARGER: &str = "\
        fs-end\n\
        he-DX\n\
        fs-he\n\
        start-DX\n\
        pj-DX\n\
        end-zg\n\
        zg-sl\n\
        zg-pj\n\
        pj-he\n\
        RW-he\n\
        fs-DX\n\
        pj-RW\n\
        zg-RW\n\
        start-pj\n\
        he-WI\n\
        zg-he\n\
        pj-fs\n\
        start-RW\n\
        ";

    test!(simple, TEST_INPUT_SIMPLE, part_1() == 10);
    test!(simple, TEST_INPUT_SIMPLE, part_2() == 36);
    test!(slightly_larger, TEST_INPUT_SLIGHTLY_LARGER, part_1() == 19);
    test!(slightly_larger, TEST_INPUT_SLIGHTLY_LARGER, part_2() == 103);
    test!(even_larger, TEST_INPUT_EVEN_LARGER, part_1() == 226);
    test!(even_larger, TEST_INPUT_EVEN_LARGER, part_2() == 3509);
    bench_parse!(MultiMap::len, 13);
    bench!(part_1() == 4720);
    // bench!(part_2() == 0);
}
