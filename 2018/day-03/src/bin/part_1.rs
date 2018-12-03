use std::fs::File;
use std::io::prelude::*;

#[macro_use]
extern crate itertools;
extern crate regex;
use regex::Regex;

#[derive(Debug)]
struct Pt(usize, usize);
// type Pt = (usize, usize);

fn read_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


type Input = Vec<(Pt, Pt)>;
type Output = usize;

fn parse_input(input: &str) -> Input {
    let re = Regex::new(r"@ (\d+),(\d+): (\d+)x(\d+)$").unwrap();
    input.trim().lines().map(|line| {
        let caps = re.captures(line).unwrap();
        assert_eq!(caps.len(), 5);
        let values: Vec<usize> = caps.iter()
            .skip(1)
            .map(|x| x.unwrap().as_str().parse::<usize>().unwrap())
            .collect();
        (Pt(values[0], values[1]),
         Pt(values[2], values[3]))
    }).collect()
}


fn process(input: Input) -> Output {
    const SIZE: usize = 1000;
    const COUNT: usize = SIZE * SIZE;
    let mut fabric = [0u8; COUNT]; // stack overflow with usize :S
    for claim in input {
        let (pos, size) = claim;
        let rows = pos.1..(pos.1 + size.1);
        let cols = pos.0..(pos.0 + size.0);
        for (row, col) in iproduct!(rows, cols) {
            fabric[row * 1000 + col] += 1;
        }
    }
    fabric.iter().filter(|&x| *x > 1).count()
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
        "#1 @ 1,3: 4x4\n\
            #2 @ 3,1: 4x4\n\
            #3 @ 5,5: 2x2".to_string()
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(input), 4);
    }
}
