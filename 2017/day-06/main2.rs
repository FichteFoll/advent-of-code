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


fn reallocate2(initial_sizes: &Vec<usize>) -> usize {
    let mut banks = initial_sizes.clone();
    let mut configs_seen: Vec<Vec<usize>> = Vec::new();

    let mut anchor_state: Vec<usize> = vec!{};
    let mut step_counter: usize = 0;
    loop {
        step_counter += 1;
        // find biggest bank
        let mut biggest_bank: (usize, usize) = (0, 0);
        for (i, &size) in banks.iter().enumerate() {
            if biggest_bank.1 < size {
                biggest_bank = (i, size);
            }
        }
        // println!("biggest bank: {:?}", biggest_bank);

        // redistribute its blocks
        {
            let (mut i, mut to_distribute) = biggest_bank;
            banks[i] = 0;
            while to_distribute > 0 {
                i = (i + 1) % banks.len();
                banks[i] += 1;
                to_distribute -= 1;
            }
        }

        if anchor_state.len() > 0 {
            if anchor_state == banks {
                break;
            }
        }
        else if configs_seen.as_slice().contains(&banks) {
            anchor_state = banks.clone();
            step_counter = 0;
        }
        // println!("{:?}", banks);
        configs_seen.push(banks.clone());
    }

    println!("The number of required steps is: {}", step_counter);
    println!("The anchor_state is: {:?}", banks);
    step_counter
}


fn main() {
    let input_string = get_input();
    let input = input_string.trim();

    let initial_sizes: Vec<usize> = input.split("\t")
                                         .map(|x| { let y: usize = x.parse().unwrap(); y })
                                         .collect();
    reallocate2(&initial_sizes);

    // let vec: Vec<usize> = vec!{0, 2, 7, 0};
    // reallocate2(&vec);
}
