#![feature(test)]

extern crate test;


fn reduce_len(array: &[u8]) -> usize {
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

fn process(input: &str) -> usize {
    let array: Vec<_> = input.bytes().collect();
    (0..26u8).map(|i| {
        let unit = 65 + i;
        // let filtered_array: Vec<u8> = array.iter().filter(|&&x| x == unit || x == unit + 32).collect();
        let filtered_array: Vec<u8> = array.iter()
            .filter_map(|&x| {
                if x == unit || x == unit + 32 {
                    None
                } else {
                    Some(x)
                }
            }).collect();
        reduce_len(&filtered_array)
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
