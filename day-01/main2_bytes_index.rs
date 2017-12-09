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

    let mut sum: usize = 0;
    let offset: usize = bytes.len() / 2;
    // can iterate over half of the list and multiply sum by 2
    for i in 0..(bytes.len() / 2) {
    // for i in 0..bytes.len() {
        let j = (i + offset) % bytes.len();
        if bytes[i] == bytes[j] {
            sum += (bytes[i] - b'0') as usize;
        }
    }

    sum *= 2;

    println!("The sum is: {}", sum);
}
