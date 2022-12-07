#![feature(test)]
#![feature(if_let_guard)]

use std::collections::BinaryHeap;

use aoc2022::*;

const DAY: usize = 7;

type Parsed = Item;

#[derive(Clone, Debug)]
enum Item {
    File { name: String, size: usize },
    Dir { name: String, children: Vec<Item> },
}
use Item::{File, Dir};

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    let mut root = Item::new_dir("/".to_string());
    let mut path: Vec<&str> = vec![];
    macro_rules! current {
        () => { path.iter().try_fold(&mut root, |i, name| i.find_child_mut(name)).unwrap() };
    }

    for line in input.lines() {
        let words: Vec<_> = line.split_ascii_whitespace().collect();
        match &words[..] {
            ["$", "cd", "/"] => {
                // we already did this in our initialization
            },
            ["$", "ls"] => {
                // ignore
            },
            ["$", "cd", ".."] => {
                path.pop();
            },
            ["$", "cd", name] => {
                path.push(name);
            },
            ["dir", name] => {
                current!().append_child(Item::new_dir(name.to_string()));
            },
            [size_str, name] if let Ok(size) = size_str.parse() => {
                current!().append_child(File { name: name.to_string(), size });
            },
            _ => panic!("couldn't match {line}"),
        }
    }
    root
}

fn part_1(parsed: &Parsed) -> usize {
    sum_deletable_dirs(parsed)
}

const MAX_CAPACITY: usize = 70_000_000 - 30_000_000;

fn part_2(parsed: &Parsed) -> usize {
    let mut sizes = Default::default();
    collect_sizes_dirs(parsed, &mut sizes);
    let sizes_vec = sizes.into_sorted_vec();
    let total = sizes_vec.last().unwrap();
    let min_to_free = total - MAX_CAPACITY;
    sizes_vec.into_iter()
        .find(|&size| size > min_to_free)
        .unwrap()
}

impl Item {
    fn new_dir(name: String) -> Self {
        Dir { name, children: vec![] }
    }

    fn name(&self) -> &str {
        match self {
            File { name, .. } => name,
            Dir { name, .. } => name,
        }
    }

    fn append_child(&mut self, child: Self) {
        if let Dir { children, ..} = self {
            children.push(child)
        } else {
            panic!("not a directory")
        }
    }

    fn find_child_mut(&mut self, name: &str) -> Option<&mut Self> {
        if let Dir { children, ..} = self {
            children.iter_mut().find(|c| c.name() == name)
        } else {
            panic!("not a directory")
        }
    }

    fn size(&self) -> usize {
        match self {
            File { size, .. } => *size,
            Dir { children, .. } =>
                children.iter().map(|c| c.size()).sum(),
        }
    }
}

fn sum_deletable_dirs(item: &Item) -> usize {
    if let Dir { children, .. } = item {
        let sum_self = Some(item.size()).filter(|&s| s < 100_000).unwrap_or(0);
        let sum_children: usize = children.iter()
            .map(|c| sum_deletable_dirs(&c))
            .sum();
        sum_self + sum_children
    } else {
        0
    }
}

fn collect_sizes_dirs(item: &Item, sizes: &mut BinaryHeap<usize>) -> usize {
    match item {
        File { size, .. } => *size,
        Dir { children, .. } => {
            let this_size = children.iter()
                .map(|c| collect_sizes_dirs(&c, sizes))
                .sum();
            sizes.push(this_size);
            this_size
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        $ cd /\n\
        $ ls\n\
        dir a\n\
        14848514 b.txt\n\
        8504156 c.dat\n\
        dir d\n\
        $ cd a\n\
        $ ls\n\
        dir e\n\
        29116 f\n\
        2557 g\n\
        62596 h.lst\n\
        $ cd e\n\
        $ ls\n\
        584 i\n\
        $ cd ..\n\
        $ cd ..\n\
        $ cd d\n\
        $ ls\n\
        4060174 j\n\
        8033020 d.log\n\
        5626152 d.ext\n\
        7214296 k\n\
        ";

    test!(part_1() == 95437);
    test!(part_2() == 24933642);
    bench_parse!(|p: &Parsed| p.size(), 48008081);
    bench!(part_1() == 1141028);
    bench!(part_2() == 8278005);
}
