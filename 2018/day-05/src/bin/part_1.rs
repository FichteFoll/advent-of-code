#![feature(test)]

extern crate test;


fn process(input: &str) -> usize {
    let array: Vec<_> = input.bytes().collect();
    let out: Vec<u8> = array.iter().fold(
        Vec::with_capacity(array.len() / 2),
        |mut vec, &c| {
            if let Some(&last_c) = vec.last() {
                if (last_c as i8 - c as i8).abs() == 32 { // 'a' - 'A'
                    vec.pop();
                    return vec;
                }
            }
            vec.push(c);
            vec
        });
    out.len()
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
        assert_eq!(process("aA"), 0);
        assert_eq!(process("abBA"), 0);
        assert_eq!(process("abAB"), 4);
        assert_eq!(process("aabAAB"), 6);
        assert_eq!(process("dabAcCaCBAcCcaDA"), 10);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        let input = input_str.trim();
        b.iter(|| {
            assert_eq!(process(&input), 11894);
        });
    }
}
