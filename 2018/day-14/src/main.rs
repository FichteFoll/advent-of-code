#![feature(test)]

extern crate test;


fn process(input: usize) -> String {
    let mut recipes: Vec<usize> = vec![3, 7];
    let mut elves: Vec<usize> = vec![0, 1]; // indices of recipes that elves process
    while recipes.len() < input + 10 {
        let sum: usize = elves.iter().map(|&elf| recipes[elf]).sum();
        let (a, b) = (sum / 10, sum % 10);
        if a > 0 {
            recipes.push(a);
        }
        recipes.push(b);
        for elf in elves.iter_mut() {
            *elf = (*elf + recipes[*elf] + 1) % recipes.len();
        }
    }
    recipes.into_iter().skip(input).take(10)
        .map(|n| (n as u8 + 0x30) as char).collect()
}

fn main() {
    println!("Part 1: {:?}", process(47801));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;


    #[test]
    fn simple_input() {
        assert_eq!(process(9), "5158916779");
    }

    #[test]
    fn more_input() {
        assert_eq!(process(5),    "0124515891");
        assert_eq!(process(18),   "9251071085");
        assert_eq!(process(2018), "5941429882");
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(process(47801), "1342316410");
        });
    }
}
