use std::fs::File;
use std::io::prelude::*;
use std::collections::BTreeSet;

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
    let mut seen: BTreeSet<i32> = BTreeSet::new();
    let mut acc = 0i32;
    for change in input.iter().cycle() {
        seen.insert(acc);
        acc += *change;
        if seen.contains(&acc) {
            return acc;
        }
    }
    panic!("Should never be reached");
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
        assert_eq!(process(input), 2);
    }

    #[test]
    fn more_tests() {
        assert_eq!(process(vec![1, -1]), 0);
        assert_eq!(process(vec![3, 3, 4, -2, -4]), 10);
        assert_eq!(process(vec![-6, 3, 8, 5, -6]), 5);
        assert_eq!(process(vec![7, 7, -2, -7, -4]), 14);
    }
}
