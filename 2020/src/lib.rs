use std::{collections::{HashMap, HashSet}, hash::Hash};

extern crate impl_ops;

pub mod grid2d;
pub mod hashgrid;
pub mod coord;

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(
        std::env::args()
            .skip(1)
            .find(|s| !s.starts_with("--"))
            .unwrap_or(format!("input/day{:0>2}.txt", day)),
    )
    .unwrap()
}

// requires DAY
#[macro_export]
macro_rules! read_input {
    () => {
        read_file(DAY)
    };
}

// requires TEST_INPUT_STR & parse_input
#[macro_export]
macro_rules! test {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part>]() {
                let input = parse_input(TEST_INPUT_STR);
                assert_eq!($part(&input$(, $param)*), $expected);
            }
        }
    };
    ($input: expr, $part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part>]() {
                let input = parse_input($input);
                assert_eq!($part(&input$(, $param)*), $expected);
            }
        }
    };
    ($suffix: ident, $input: expr, $part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[test]
            fn [<test_ $part _ $suffix>]() {
                let input = parse_input($input);
                assert_eq!($part(&input$(, $param)*), $expected);
            }
        }
    };
}

// requires parse_input
#[macro_export]
macro_rules! bench {
    ($part: ident ($($param: expr),*) == $expected:expr) => {
        paste::paste! {
            #[bench]
            fn [<bench_ $part>](b: &mut test::Bencher) {
                let input_str = read_input!();
                let input = parse_input(&input_str);
                b.iter(|| assert_eq!($part(test::black_box(&input) $(, $param)* ), $expected));
            }
        }
    };
}

// requires parse_input
#[macro_export]
macro_rules! bench_parse {
    ($fn:ident, $expected_len:expr) => {
        #[bench]
        fn bench_parse(b: &mut test::Bencher) {
            let raw = read_input!();
            b.iter(|| assert_eq!(parse_input(test::black_box(&raw)).$fn(), $expected_len));
        }
    };
    () => {
        #[bench]
        fn bench_parse(b: &mut test::Bencher) {
            let raw = read_input!();
            b.iter(|| {
                let _ = parse_input(test::black_box(&raw));
            });
        }
    };
}

// Given a slice of type T, return a Vec containing the powerset,
// i.e. the set of all subsets.
//
// This works by treating each int the range [0, 2**n) (where n is the
// length of the slice) as a bitmask, selecting only the members of
// the original slice whose corresponding positional bits are flipped
// on in each mask.
pub fn powerset<T: Clone>(slice: &[T]) -> Vec<Vec<T>> {
    let mut v: Vec<Vec<T>> = Vec::new();

    for mask in 0..(1 << slice.len()) {
        let mut ss: Vec<T> = vec![];
        let mut bitset = mask;
        while bitset > 0 {
            // isolate the rightmost bit to select one item
            let rightmost: u64 = bitset & !(bitset - 1);
            // turn the isolated bit into an array index
            let idx = rightmost.trailing_zeros();
            let item = (*slice.get(idx as usize).unwrap()).clone();
            ss.push(item);
            // zero the trailing bit
            bitset &= bitset - 1;
        }
        v.push(ss);
    }
    v
}


// Given a map with a set of candidates, reduce the candidates to one each
pub fn resolve_multimap<K, V>(multimap: &HashMap<K, HashSet<V>>, singlemap: HashMap<K, V>) -> Option<HashMap<K, V>>
where
    K: Clone + Eq + Hash,
    V: Clone + Eq + Hash,
{
    // recursive depth-first search
    let mut sorted_map: Vec<_> = multimap.iter().collect();
    sorted_map.sort_unstable_by_key(|(_, x)| x.len());
    if let Some((k, vs)) = sorted_map.into_iter().next() {
        for v in vs.iter() {
            let mut new_singlemap = singlemap.clone();
            new_singlemap.insert(k.clone(), v.clone());
            let mut new_multimap = multimap.clone();
            for other_vs in new_multimap.values_mut() {
                other_vs.retain(|x| x != v)
            }
            new_multimap.remove(&k);
            let result = resolve_multimap(&new_multimap, new_singlemap);
            if result.is_some() {
                return result;
            }
        }
        None
    } else {
        Some(singlemap)
    }
}
