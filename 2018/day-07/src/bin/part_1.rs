#![feature(test)]

extern crate test;
use std::collections::{HashMap, HashSet};


fn process(input: &str) -> String {
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
    println!("{:?}", current);
    assert!(current.len() > 0);

    let mut out: Vec<char> = Vec::with_capacity(input.len());
    'outer: loop {
        current.sort_unstable();
        // Get char that satisfies all prereqs
        let c = loop {
            if current.len() == 0 {
                break 'outer;
            }
            let c = current.remove(0);
            if out.contains(&c) {
                continue;
            }
            let deps_satisfied = rev_dep_map.get(&c).unwrap().iter()
                .all(|dep_c| out.contains(dep_c));
            if deps_satisfied {
                break c;
            }
        };
        out.push(c);
        if let Some(iter) = dep_map.get(&c) {
            current.extend(iter);
        }
    }
    out.into_iter().collect()
}

fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    let input = input_str.trim();
    println!("The result is {:?}", process(&input));
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
        assert_eq!(process(&input_str), "CABDFE");
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        let input = input_str.trim();
        b.iter(|| {
            assert_eq!(process(&input), "AEMNPOJWISZCDFUKBXQTHVLGRY");
        });
    }
}
