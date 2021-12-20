#![feature(test)]

use std::hash::Hash;
use std::collections::VecDeque;
use std::collections::HashMap as StdHashMap;
use aoc2021::*;
use aoc2021::collections::HashSet;
use parse::parse_input;


const DAY: usize = 12;

type Parsed<'a> = StdHashMap<Node<'a>, Vec<Node<'a>>>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Node<'a> {
    Start,
    End,
    Small(&'a str),
    Big(&'a str),
}

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use itertools::Itertools;

    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split('\n')
            .map(|line| line.split_once('-').unwrap())
            .flat_map(|(a, b)| [(a.into(), b.into()), (b.into(), a.into())])
            .into_group_map()
    }

    impl<'a> From<&'a str> for Node<'a> {
        fn from(s: &'a str) -> Self {
            match s {
                "start" => Node::Start,
                "end" => Node::End,
                _ if s.chars().all(|c| c.is_ascii_lowercase()) => Node::Small(s),
                _ => Node::Big(s),
            }
        }
    }
}

fn part_1(parsed: &Parsed) -> usize {
    num_paths(parsed, false)
}

fn part_2(parsed: &Parsed) -> usize {
    num_paths(parsed, true)
}

#[derive(Clone, Debug)]
struct Path<'a> {
    pos: &'a Node<'a>,
    visited: HashSet<&'a str>,
    allow_twice: bool,
}

fn num_paths(parsed: &Parsed, allow_twice: bool) -> usize {
    let mut queue: VecDeque<_> = parsed.get(&Node::Start).unwrap().iter()
        .map(|pos| Path { pos, visited: HashSet::default(), allow_twice })
        .collect();
    let mut count = 0;
    while let Some(path) = queue.pop_front() {
        match step(parsed, path) {
            StepResult::Extend(extend) => queue.extend(extend),
            StepResult::Add(add) => count += add,
        };
    }
    count
}

enum StepResult<'a> {
    Extend(Vec<Path<'a>>),
    Add(usize),
}

#[inline]
fn step<'a>(parsed: &'a Parsed<'a>, path: Path<'a>) -> StepResult<'a> {
    match path.pos {
        Node::Start => StepResult::Add(0),
        Node::End => StepResult::Add(1),
        Node::Small(name) if !path.visited.contains(name) || path.allow_twice => StepResult::Extend(
            parsed.get(path.pos).unwrap().iter()
                .map(|pos| {
                    let mut visited = path.visited.clone();
                    let allow_twice = path.allow_twice & visited.insert(name);
                    Path { pos, visited, allow_twice }
                })
                .collect()
        ),
        Node::Small(_) => StepResult::Add(0),
        Node::Big(_) => StepResult::Extend(
            parsed.get(path.pos).unwrap().iter()
                .map(|pos| Path { pos, visited: path.visited.clone(), ..path })
                .collect()
        ),
    }
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
    bench_parse!(StdHashMap::len, 13);
    bench!(part_1() == 4720);
    // test!(&read_input!(), part_2() == 147848);
    // takes about 250ms for a single run
    bench!(part_2() == 147848);
}
