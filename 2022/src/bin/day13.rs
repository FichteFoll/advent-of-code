#![feature(binary_heap_into_iter_sorted)]
#![feature(is_sorted)]
#![feature(test)]

use std::cmp::Ordering;
use std::collections::BinaryHeap;

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 13;

type Parsed = Vec<[Item; 2]>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Item {
    Num(u8),
    List(Vec<Item>),
}

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        input
            .trim()
            .split("\n\n")
            .map(|block| {
                let pair = block.split_once('\n').unwrap();
                [parse_item(pair.0), parse_item(pair.1)]
            })
            .collect()
    }

    fn parse_item(slice: &str) -> Item {
        let (item, length) = parse_list(slice);
        debug_assert_eq!(length, slice.len());
        item
    }

    fn parse_list(slice: &str) -> (Item, usize) {
        let mut items = vec![];
        let bytes = slice.as_bytes();
        let mut i = 1; // skip first '['
        let mut digits = vec![];
        macro_rules! finalize_digits {
            () => {
                if !digits.is_empty() {
                    let num = digits.drain(..).fold(0, |a, b| a * 10 + (b - b'0'));
                    items.push(Item::Num(num));
                }
            };
        }
        loop {
            match bytes[i] {
                b']' => {
                    finalize_digits!();
                    i += 1;
                    break;
                }
                b',' => {
                    finalize_digits!();
                    i += 1;
                },
                b'[' => {
                    let (item, length) = parse_list(&slice[i..]);
                    items.push(item);
                    i += length;
                }
                b => {
                    digits.push(b);
                    i += 1;
                }
            }
        }
        (Item::List(items), i)
    }
}

fn part_1(parsed: &Parsed) -> usize {
    parsed.iter()
        .enumerate()
        .flat_map(|(i, pair)| pair.is_sorted().then_some(i + 1))
        .sum()
}

fn part_2(parsed: &Parsed) -> usize {
    let divider_packets = [
        Item::List(vec![Item::List(vec![Item::Num(2)])]),
        Item::List(vec![Item::List(vec![Item::Num(6)])]),
    ];
    let items: BinaryHeap<_> = parsed.iter().flatten().chain(&divider_packets).cloned().collect();
    let count = items.len();
    items.into_iter_sorted()
        .enumerate()
        .flat_map(|(i, item)| divider_packets.contains(&item).then_some(count - i))
        .product()
}

impl std::cmp::PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Item::Num(n1), Item::Num(n2)) => equal_is_none(n1.cmp(n2)),
            (i1 @ Item::Num(_), i2 @ Item::List(_)) => Item::List(vec![i1.clone()]).partial_cmp(i2),
            (i1 @ Item::List(_), i2 @ Item::Num(_)) => i1.partial_cmp(&Item::List(vec![i2.clone()])),
            (Item::List(l1), Item::List(l2)) => {
                l1.iter().zip(l2)
                    .find_map(|(i1, i2)| i1.partial_cmp(i2))
                    .or_else(|| equal_is_none(l1.len().cmp(&l2.len())))
            }
        }
    }
}

impl std::cmp::Ord for Item {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

#[inline(always)]
fn equal_is_none(ord: Ordering) -> Option<Ordering> {
    Some(ord).filter(|o| o != &Ordering::Equal)
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        [1,1,3,1,1]\n\
        [1,1,5,1,1]\n\
        \n\
        [[1],[2,3,4]]\n\
        [[1],4]\n\
        \n\
        [9]\n\
        [[8,7,6]]\n\
        \n\
        [[4,4],4,4]\n\
        [[4,4],4,4,4]\n\
        \n\
        [7,7,7,7]\n\
        [7,7,7]\n\
        \n\
        []\n\
        [3]\n\
        \n\
        [[[]]]\n\
        [[]]\n\
        \n\
        [1,[2,[3,[4,[5,6,7]]]],8,9]\n\
        [1,[2,[3,[4,[5,6,0]]]],8,9]\n\
        ";

    test!(part_1() == 13);
    test!(part_2() == 140);
    bench_parse!(Vec::len, 450 / 3);
    bench!(part_1() == 5557);
    bench!(part_2() == 22425);

    #[test]
    fn test_partial_cmp_num_num() {
        assert_eq!(Item::Num(0).partial_cmp(&Item::Num(0)), None);
        assert_eq!(Item::Num(1).partial_cmp(&Item::Num(0)), Some(Ordering::Greater));
        assert_eq!(Item::Num(1).partial_cmp(&Item::Num(2)), Some(Ordering::Less));
    }

    #[test]
    fn test_partial_cmp_num_list() {
        assert_eq!(Item::Num(0).partial_cmp(&Item::List(vec![Item::Num(0)])), None);
        assert_eq!(Item::Num(1).partial_cmp(&Item::List(vec![Item::Num(0)])), Some(Ordering::Greater));
        assert_eq!(Item::Num(1).partial_cmp(&Item::List(vec![Item::Num(2)])), Some(Ordering::Less));

        assert_eq!(Item::Num(0).partial_cmp(&Item::List(vec![])), Some(Ordering::Greater));
        assert_eq!(Item::Num(0).partial_cmp(&Item::List(vec![Item::Num(0), Item::Num(0)])), Some(Ordering::Less));
    }

    #[test]
    fn test_partial_cmp_list_list() {
    }
}
