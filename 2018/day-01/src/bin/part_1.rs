use std::fs::File;
use std::io::prelude::*;


fn read_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


type Input = Vec<i32>;

fn parse_input(input: &str) -> Input {
    input.trim().split("\n").map(|x| x.parse::<i32>().unwrap()).collect()
}


fn process(input: Input) -> i32 {
    input.iter().sum()
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
        "+1\n\
            -2\n\
            +3\n\
            +1".to_string()
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(input), 3);
    }
}
