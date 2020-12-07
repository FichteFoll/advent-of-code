#![feature(test, str_split_once)]

use std::collections::{HashMap, HashSet, VecDeque};

fn read_input() -> String {
    std::fs::read_to_string("input/day07.txt").expect("can’t read file")
}

type Input = HashMap<String, Vec<(String, usize)>>;

fn parse_input(input_str: &str) -> Input {
    input_str
        .trim()
        .split("\n")
        .map(|line| {
            let (color, contents) = line
                .strip_suffix(".").expect("line did not have a dot")
                .split_once(" bags contain ").expect("could not split");
            let rules = contents.split(", ")
                .filter_map(|rule| {
                    if rule == "no other bags" {
                        None
                    } else {
                        let mut words = rule.split_whitespace();
                        let n: usize = words.next().unwrap().parse().unwrap();
                        let sub_color = words.take(2).collect::<Vec<_>>().join(" ");
                        Some((sub_color, n))
                    }
                })
                .collect();
            (color.to_string(), rules)
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    // reverse the input graph
    let mut graph: HashMap<&str, Vec<&str>> = HashMap::new();
    for (color, sub_colors) in input {
        for (sub_color, _) in sub_colors.iter() {
            graph.entry(sub_color).or_default().push(color);
        }
    }
    // traverse graph
    let mut queue: VecDeque<&str> = vec!["shiny gold"].into_iter().collect();
    let mut seen = HashSet::new();
    loop {
        if let Some(color) = queue.pop_front() {
            if !seen.insert(color) {
                continue;
            }
            graph.get(color).map(|sub_colors| queue.extend(sub_colors));
        } else {
            break;
        }
    }
    seen.len() - 1
}

fn count_bags(input: &Input, color: &str, mut seen: &mut HashMap<String, usize>) -> usize {
    if let Some(&n) = seen.get(color) {
        n
    } else {
        let n = 1 + input.get(color).expect("bag color not found")
            .iter()
            .map(|(sub_color, multiplier)| {
                multiplier * count_bags(input, sub_color, &mut seen)
            })
            .sum::<usize>();
        seen.insert(color.to_string(), n);
        n
    }
}

fn part_2(input: &Input) -> usize {
    let mut seen = HashMap::new();
    count_bags(input, "shiny gold", &mut seen) - 1
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

    const EXAMPLE_INPUT_STR: &str = "\
        light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
        dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
        bright white bags contain 1 shiny gold bag.\n\
        muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
        shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
        dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
        vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
        faded blue bags contain no other bags.\n\
        dotted black bags contain no other bags.\n\
        ";

    #[test]
    fn test_part_1() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_1(&input), 4);
    }

    #[test]
    fn test_part_2() {
        let input = parse_input(&EXAMPLE_INPUT_STR);
        assert_eq!(part_2(&input), 32);
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
            assert_eq!(part_1(&input), 213);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        let input_str = read_input();
        let input = parse_input(&input_str);
        b.iter(|| {
            assert_eq!(part_2(&input), 38426);
        });
    }
}
