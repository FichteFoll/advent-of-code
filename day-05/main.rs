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
    let input_string = get_input();
    let input = input_string.trim();

    let mut jump_list: Vec<isize> = input.split("\n")
                                         .map(|x| { let y: isize = x.parse().unwrap(); y })
                                         .collect();

    let mut step_counter: usize = 0;
    let mut i: usize = 0;
    while i < jump_list.len() {
        let offset = jump_list[i];
        jump_list[i] += 1;
        let new_i: isize = i as isize + offset;
        if new_i < 0 {
            panic!("Underflowing to {} after {} steps", new_i, step_counter);
        }
        else {
            i = new_i as usize;
        }
        step_counter += 1;
    }

    println!("The number of required steps is: {}", step_counter);
}
