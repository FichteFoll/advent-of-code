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
    let bytes = input.trim();

    let mut sum: usize = 0;
    for line in bytes.split("\n") {
        let mut min: usize = 99999;
        let mut max: usize = 0;
        for num_str in line.split("\t") {
            let num: usize = num_str.parse().unwrap();
            if min > num {
                min = num;
            }
            if max < num {
                max = num;
            }
        }
        sum += max - min;
    }

    println!("The sum is: {}", sum);
}
