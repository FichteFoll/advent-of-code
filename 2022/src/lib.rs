// Provides helpful methods, data structures and macros
// for at least more than one task.

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
