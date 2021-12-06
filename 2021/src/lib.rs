// Provides helpful methods, data structures and macros
// for at least more than one task.

pub mod coord;
pub mod test;

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
