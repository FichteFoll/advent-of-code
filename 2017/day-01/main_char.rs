// use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");

    let first_char = contents.chars().nth(0).expect("input is empty");
    contents.pop();
    contents.push(first_char);
    // println!("contents: {:?}", contents);

    let mut slice = contents.as_str();
    slice = slice.trim();

    let mut last_char: char = '\0';
    let mut sum = 0;
    for c in slice.chars() {
        if c == '\n' {
            break;
        }
        else if c == last_char && last_char != '\0' {
            sum += c.to_digit(10).expect("invalid input string");
        }
        last_char = c;
    }

    println!("The sum is: {}", sum);
}
