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
type Output = String;

fn parse_input(input: &str) -> Input {
    input.trim().split("\n").map(|x| x.to_owned()).collect()
}


fn process(input: Input) -> Output {
    let max_len = input.iter().next().unwrap().len();
    for i in 0..max_len {
        let mut dict: HashSet<String> = HashSet::new();
        for word in input.iter() {
            let mut sub_word = String::new();
            sub_word.push_str(&word[..i]);
            sub_word.push_str(&word[i + 1..]);
            if dict.contains(&sub_word) {
                return sub_word;
            }
            dict.insert(sub_word);
        }
    }
    "".to_string()
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
        "abcde\n\
            fghij\n\
            klmno\n\
            pqrst\n\
            fguij\n\
            axcye\n\
            wvxyz".to_string()
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(input), "fgij".to_string());
    }
}
