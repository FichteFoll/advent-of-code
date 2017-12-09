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

fn sort_word(word: &str) -> Vec<char> {
    let mut char_list: Vec<char> = word.chars().collect();
    char_list.sort();
    char_list
}

fn main() {
    let input_string = get_input(); // maintain ownership
    let input = input_string.trim();

    let mut valid_counter: usize = 0;
    'lineloop: for line in input.split("\n") {
        let mut words: Vec<Vec<char>> = Vec::new();
        for word in line.split(" ") {
            let sorted_word = sort_word(&word);
            if words.as_slice().contains(&sorted_word) {
                println!("invalid pass phrase: {:?}", line);
                continue 'lineloop;
            }
            words.push(sorted_word);
        }
        valid_counter += 1;
    }

    println!("The number of valid phrases is: {}", valid_counter);
}
