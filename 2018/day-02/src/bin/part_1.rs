use std::fs::File;
use std::io::prelude::*;
use std::collections::HashSet;


fn read_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


type Input = Vec<String>;

fn parse_input(input: &str) -> Input {
    input.trim().split("\n").map(|x| x.to_owned()).collect()
}


fn process(input: Input) -> usize {
    let mut hash_multi = [0usize; 4]; // first element is useless
    for word in input {
        let mut count = [0u8; 26];
        for c in word.bytes() {
            count[(c as usize) - ('a' as usize)] += 1
        }
        let counts: HashSet<&u8> = count.iter().filter(|&n| *n > 0).collect();
        for n in counts {
            if (*n as usize) < hash_multi.len() {
                hash_multi[*n as usize] += 1
            }
        }
    }
    hash_multi[2] * hash_multi[3]
}


fn main() {
    let input_str = read_input();
    let input = parse_input(&input_str);

    let result = process(input);
    println!("The result is {}", result);
}


#[cfg(test)]
mod tests {
    use super::*;

    fn test_input() -> String {
        "abcdef\n\
            bababc\n\
            abbcde\n\
            abcccd\n\
            aabcdd\n\
            abcdee\n\
            ababab".to_string()
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(input), 12);
    }
}
