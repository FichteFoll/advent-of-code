#![feature(test)]

extern crate test;
extern crate rayon;

use rayon::prelude::*;


fn react_len(array: &[u8]) -> usize {
    let mut vec: Vec<&u8> = Vec::with_capacity(array.len() / 2);
    for c in array {
        if let Some(&last_c) = vec.last() {
            if last_c ^ c == 32 { // abs(last_c - c) == 32
                vec.pop();
                continue;
            }
        }
        vec.push(c);
    }
    vec.len()
}

fn process(input: &str) -> usize {
    let array: Vec<_> = input.bytes().collect();
    (0..26u8).into_par_iter().map(|i| {
        let unit = 0x61 + i; // lower case
        let filtered_array: Vec<u8> = array.clone().into_iter()
            .filter(|&x| x | 32 != unit).collect();
        react_len(&filtered_array)
    }).min().unwrap()
}

fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    let input = input_str.trim();
    println!("The result is {:?}", process(&input));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn simple_input() {
        assert_eq!(process("dabAcCaCBAcCcaDA"), 4);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        let input = input_str.trim();
        b.iter(|| {
            assert_eq!(process(&input), 5310);
        });
    }
}
