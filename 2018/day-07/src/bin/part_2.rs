#![feature(test)]

extern crate test;
use std::collections::{HashMap, HashSet};

fn next_char(candidates: &mut Vec<char>, done: &[char], rev_dep_map: &HashMap<char, HashSet<char>>) -> Option<char> {
    loop {
        if candidates.len() == 0 {
            break None;
        }
        let c = candidates.remove(0);
        if done.contains(&c) {
            continue;
        }
        let deps_satisfied = rev_dep_map.get(&c).unwrap().iter()
            .all(|dep_c| done.contains(dep_c));
        if deps_satisfied {
            break Some(c);
        }
    }
}

fn process(input: &str, step_offset: usize, workers: usize) -> usize {
    let mut dep_map: HashMap<char, HashSet<char>> = HashMap::new();
    let mut rev_dep_map: HashMap<char, HashSet<char>> = HashMap::new();
    for line in input.lines() {
        let mut chars = line.chars().skip(5);
        let from = chars.next().expect("no source");
        let to = chars.skip(30).next().expect("no target");

        dep_map.entry(from).or_insert_with(|| HashSet::new()).insert(to);
        rev_dep_map.entry(to).or_insert_with(|| HashSet::new()).insert(from);
        rev_dep_map.entry(from).or_insert_with(|| HashSet::new()); // ensure all chars are added
    }
    let mut current: Vec<_> = rev_dep_map.iter().filter_map(|(k, v)| {
        match v.len() {
            0 => Some(*k),
            _ => None
        }
    }).collect();
    assert!(current.len() > 0);

    let mut done: Vec<char> = Vec::with_capacity(input.len());
    let mut queue: Vec<(char, usize)> = Vec::with_capacity(workers);
    let mut steps: usize = 0;
    loop {
        // Fill queue
        current.sort_unstable();
        while queue.len() < workers {
            // Get firstchar that satisfies all prereqs
            match next_char(&mut current, &done, &rev_dep_map) {
                Some(c) => queue.push((c, step_offset + (c as usize - 0x40))),
                None => break,
            }
        }

        // process
        if queue.len() == 0 {
            break;
        }
        let min_remaining = queue.iter().map(|(_, remaining)| remaining).min().unwrap();
        steps += min_remaining;

        let mut new_done: Vec<char> = Vec::new();
        queue = queue.iter().filter_map(|(c, mut remaining)| {
            remaining -= min_remaining;
            if remaining == 0 {
                new_done.push(*c);
                None
            } else {
                Some((*c, remaining))
            }
        }).collect();

        // collect
        for c in new_done {
            done.push(c);
            if let Some(iter) = dep_map.get(&c) {
                current.extend(iter);
            }
        }
    }
    steps
}

fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    let input = input_str.trim();
    println!("The result is {:?}", process(&input, 60, 5));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn simple_input() {
        let input_str = "\
            Step C must be finished before step A can begin.\n\
            Step C must be finished before step F can begin.\n\
            Step A must be finished before step B can begin.\n\
            Step A must be finished before step D can begin.\n\
            Step B must be finished before step E can begin.\n\
            Step D must be finished before step E can begin.\n\
            Step F must be finished before step E can begin.";
        assert_eq!(process(&input_str, 0, 2), 15);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        let input = input_str.trim();
        b.iter(|| {
            assert_eq!(process(&input, 60, 5), 1081);
        });
    }
}
