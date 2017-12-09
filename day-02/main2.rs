// use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::cmp;

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
    'lineloop: for (i, line) in bytes.split("\n").enumerate() {
        println!("line {}", i);
        let mut nums: Vec<usize> = Vec::new();
        for num_str in line.split("\t") {
            let num: usize = num_str.parse().unwrap();
            nums.push(num);
        }

        let mut min: usize;
        let mut max: usize;
        for num in nums.iter() {
            for num2 in nums.iter() {
                if num == num2 {
                    continue;
                }
                min = cmp::min(*num, *num2);
                max = cmp::max(*num, *num2);
                if max % min == 0 {
                    println!("{} divides {}", min, max);
                    println!("adding {:?}", max / min);
                    sum += max / min;
                    continue 'lineloop;
                }
            }
        }
    }

    println!("The sum is: {}", sum);
}
