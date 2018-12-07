// use std::env;
use std::fs::File;
use std::io::prelude::*;

fn get_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


fn main() {
    let input = get_input();
    let bytes = input.trim().as_bytes();

    let first_byte = &bytes[0];
    let mut previous_byte = first_byte;
    let mut sum: usize = 0;
    // do the counting
    for c in bytes.iter() {
        if previous_byte != first_byte && *c == *previous_byte {
            sum += (*c - b'0') as usize;
        }
        previous_byte = c;
    }
    // wrap around
    if previous_byte != first_byte && *previous_byte == *first_byte {
        sum += (*previous_byte - b'0') as usize;
    }

    println!("The sum is: {}", sum);
}
