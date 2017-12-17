const STEPS: usize = 50_000_000;
const SKIP: usize = 354;

fn main() {
    let mut i = 0;
    let mut value_after_zero: usize = 0;
    for value in 1..(STEPS + 1) {
        i = (i + SKIP) % value + 1; // value coincides with the length of the buffer
        if i == 1 {
            value_after_zero = value;
            println!("new value: {}", value);
        }
    }
    println!("value after 0: {}", value_after_zero);
}
