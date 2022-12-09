#![feature(binary_heap_into_iter_sorted)]
#![feature(if_let_guard)]
#![feature(test)]

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

main!();

fn parse_input(input: &str) -> Parsed {
    let mut root = Item::new_dir("/".to_string());
    let mut path: Vec<&str> = vec![];
    macro_rules! current {
        () => { path.iter().try_fold(&mut root, |i, name| i.find_child_mut(name)).unwrap() };
    }

    for line in input.lines() {
        let words: Vec<_> = line.split_ascii_whitespace().collect();
        match &words[..] {
            ["$", "cd", "/"] => {}, // we already did this in our initialization
            ["$", "ls"] => {}, // ignore
            ["$", "cd", ".."] => { path.pop(); },
            ["$", "cd", name] => path.push(name),
            ["dir", name] => current!().append_child(Item::new_dir(name.to_string())),
            [size_str, name] if let Ok(size) = size_str.parse() => {
                current!().append_child(File { name: name.to_string(), size });
            },
            _ => panic!("couldn't match {line}"),
        }
    }
    root
}

fn part_1(parsed: &Parsed) -> usize {
    let mut sizes = Default::default();
    collect_sizes_dirs(parsed, &mut sizes);
    sizes.into_iter_sorted()
        .skip_while(|&size| size > 100_000)
        .sum()
}

const MAX_CAPACITY: usize = 70_000_000 - 30_000_000;

fn part_2(parsed: &Parsed) -> usize {
    let mut sizes = Default::default();
    collect_sizes_dirs(parsed, &mut sizes);
    let total = sizes.peek().unwrap();
    let min_to_free = total - MAX_CAPACITY;
    sizes.into_iter_sorted()
        .take_while(|&size| size > min_to_free)
        .last()
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
}

fn collect_sizes_dirs(item: &Item, sizes: &mut BinaryHeap<usize>) -> usize {
    match item {
        File { size, .. } => *size,
        Dir { children, .. } => {
            let this_size = children.iter()
                .map(|c| collect_sizes_dirs(c, sizes))
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
    bench_parse!(|p: &Parsed| match p { Dir { children, .. } => children.len(), _ => panic!(), }, 8);
    bench!(part_1() == 1141028);
    bench!(part_2() == 8278005);
}
