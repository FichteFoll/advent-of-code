#![feature(bool_to_option)]
#![feature(map_first_last)]
#![feature(test)]

use std::collections::{BTreeSet, BTreeMap, HashMap};

use itertools::{Itertools, iproduct};
use lazy_static::lazy_static;

use aoc2021::*;

const DAY: usize = 8;

type Parsed = Vec<Line>;
struct Line { digits: Vec<Digit>, output: Vec<Digit> }
type Digit = BTreeSet<Segment>;
type Segment = char;

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
        .map(|line| {
            let (left, right) = line.split_once(" | ").unwrap();
            let digits = left.split_whitespace()
                .map(|digit| digit.chars().collect())
                .collect();
            let output = right.split_whitespace()
                .map(|digit| digit.chars().collect())
                .collect();
            Line { digits, output }
        })
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|line| line.output.iter()
            .filter(|a| SEG_COUNTS_WITH_SINGLE_OPTION.contains(&a.len()))
            .count()
        )
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    parsed.iter()
        .map(|line| {
            let map = segments_to_digit_map(line);
            line.output.iter()
                .map(|digit| map.get(digit).unwrap())
                .fold(0, |a, b| a * 10 + b)
        })
        .sum()
}

//   0:      1:      2:      3:      4:
//  aaaa    ....    aaaa    aaaa    ....
// b    c  .    c  .    c  .    c  b    c
// b    c  .    c  .    c  .    c  b    c
//  ....    ....    dddd    dddd    dddd
// e    f  .    f  e    .  .    f  .    f
// e    f  .    f  e    .  .    f  .    f
//  gggg    ....    gggg    gggg    ....

//   5:      6:      7:      8:      9:
//  aaaa    aaaa    aaaa    aaaa    aaaa
// b    .  b    .  .    c  b    c  b    c
// b    .  b    .  .    c  b    c  b    c
//  dddd    dddd    ....    dddd    dddd
// .    f  e    f  .    f  e    f  .    f
// .    f  e    f  .    f  e    f  .    f
//  gggg    gggg    ....    gggg    gggg
lazy_static! {
    static ref SEGMENTS: Vec<BTreeSet<Segment>> =
        [
            "abcefg",  // 0
            "cf",      // 1
            "acdeg",   // 2
            "acdfg",   // 3
            "bcdf",    // 4
            "abdfg",   // 5
            "abdefg",  // 6
            "acf",     // 7
            "abcdefg", // 8
            "abcdfg",  // 9
        ]
            .into_iter()
            .map(|s| s.chars().collect())
            .collect();

    static ref SEG_COUNT_TO_DIGITS: HashMap<usize, BTreeSet<usize>> =
        SEGMENTS.iter().enumerate()
            .map(|(i, ws)| (ws.len(), i))
            .into_grouping_map()
            .fold(BTreeSet::new(), |mut acc, _, v| { acc.insert(v); acc });

    // [2, 3, 4, 7];
    static ref SEG_COUNTS_WITH_SINGLE_OPTION: BTreeSet<usize> =
        SEG_COUNT_TO_DIGITS.iter()
            .filter_map(|(n, segments)| (segments.len() == 1).then_some(n))
            .cloned()
            .collect();

    static ref COMMON_SEGMENT_COUNTS: HashMap<(usize, usize), usize> =
        iproduct!(0..SEGMENTS.len(), 0..SEGMENTS.len())
            .filter(|(a, b)| a != b)
            .map(|(a, b)| ((a, b), SEGMENTS[a].intersection(&SEGMENTS[b]).count()))
            .collect();
}

// This function is way too cursed.
// I wanted to generalize my approach with finding common intersections
// but refused to give up when the implementation grew in complexity.
// In the end, I'm just glad that my idea worked, and it's not even that slow.
// Alas, I spent enough time on this already, so it's gonna stay like this.
fn segments_to_digit_map(line: &Line) -> BTreeMap<Digit, usize> {
    let all_digits: BTreeSet<_> = line.digits.iter()
        .chain(line.output.iter())
        .cloned()
        .collect();
    let mut by_seg_count: BTreeMap<usize, BTreeSet<_>> = all_digits.into_iter()
        .into_group_map_by(BTreeSet::len)
        .into_iter()
        .map(|(n, vec)| (n, vec.into_iter().collect()))
        .collect();
    let mut map = BTreeMap::new();
    // Start with the known single result segment counts
    // and add them to our `map`.
    for n in SEG_COUNTS_WITH_SINGLE_OPTION.iter() {
        if let Some(groups) = by_seg_count.remove(n) {
            assert_eq!(groups.len(), 1);
            let group = groups.first().unwrap();
            let d = *SEG_COUNT_TO_DIGITS.get(n).unwrap().first().unwrap();
            map.insert(group.clone(), d);
        }
    }

    // The remaining numbers of segments are: 5, 6.
    // The numbers to find are: 2, 3, 5, 6, 9, 0.
    // We proceed by counting the number of intersecting segments
    // with an unknown segment set and all already known sets
    // and compare that with our precomputed map `COMMON_SEGMENT_COUNTS`.
    // When all counts for an unknown number match,
    // it is a valid candidate for this combination of segments.
    for (num_segments, groups) in by_seg_count {
        for segments in groups {
            let opts: Vec<_> = SEG_COUNT_TO_DIGITS.get(&num_segments)
                .unwrap()
                .iter()
                .filter(|n| !map.values().contains(n))
                .filter(|&&n|
                    map.iter().all(|(m_segments, &m)| {
                        let common = segments.intersection(m_segments).count();
                        let expected = *COMMON_SEGMENT_COUNTS.get(&(n, m)).unwrap();
                        expected == common
                    })
                )
                .cloned()
                .collect();
            if opts.len() == 1 {
                map.insert(segments.clone(), opts[0]);
            } else {
                unimplemented!("opts.len() != 0 is not considered");
            }
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    #[test]
    fn test_segments_to_digit_map() {
        let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
        let parsed = parse_input(input);
        let line = parsed.first().unwrap();

        assert_eq!(segments_to_digit_map(line).len(), 10);
    }

    const TEST_INPUT: &str = "\
        be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
        edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
        fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
        fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
        aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
        fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
        dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
        bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
        egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
        gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n\
        ";

    test!(line_1, "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe", part_2() == 8394);

    test!(part_1() == 26);
    test!(part_2() == 61229);
    bench_parse!(Vec::len, 200);
    bench!(part_1() == 390);
    bench!(part_2() == 1011785);
}
