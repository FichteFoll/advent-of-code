use std::fs::File;
use std::io::prelude::*;

#[macro_use]
extern crate itertools;


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
    for word1 in input.iter() {
        for word2 in input.iter() {
            if word1 == word2 {
                continue;
            }
            let mut differences = 0;
            let mut last_pos = 0;
            for (i, c1, c2) in izip!(0.., word1.chars(), word2.chars()) {
                if c1 != c2 {
                    differences += 1;
                    last_pos = i
                }
            }
            if differences == 1 {
                println!("matching words: {} {}", word1, word2);
                let mut result = String::new();
                result.push_str(&word1[..last_pos]);
                result.push_str(&word1[last_pos + 1..]);
                return result
            }
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
