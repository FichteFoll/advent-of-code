// Provides helpful methods, data structures and macros
// for at least more than one task.

#![allow(incomplete_features)]
#![feature(generic_const_exprs)]

pub mod coord;
pub mod test;

#[cfg(feature = "fnv")]
pub mod collections {
    pub use fnv::{FnvHashSet as HashSet, FnvHashMap as HashMap};
}

#[cfg(not(feature = "fnv"))]
pub mod collections {
    use std::collections::{self, hash_map::RandomState};
    pub type HashMap<K, V> = collections::HashMap<K, V, RandomState>;
    pub type HashSet<T> = collections::HashSet<T, RandomState>;
}

pub fn read_file(day: usize) -> String {
    std::fs::read_to_string(
        std::env::var("INPUT")
            .unwrap_or(format!("input/day{day:0>2}.txt")),
    )
    .unwrap()
}

// requires `DAY` & `parse_input`
#[macro_export]
macro_rules! main {
    () => {
        fn main() {
            let input = read_file(DAY);
            let parsed = parse_input(&input);
            println!("Part 1: {}", part_1(&parsed));
            println!("Part 2: {}", part_2(&parsed));
        }
    };
}
