#![feature(test)]

extern crate test;


fn part_1(input: usize) -> String {
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

fn part_2(input: &str) -> usize {
    let target: Vec<_> = input.bytes().map(|c| (c - 0x30) as usize).collect();
    let mut recipes: Vec<usize> = vec![3, 7];
    let mut elves: Vec<usize> = vec![0, 1];
    let mut candidates: Vec<usize> = Vec::new();

    let push = |recipes: &mut Vec<usize>, candidates: &mut Vec<usize>, n| {
        recipes.push(n);
        let last_i = recipes.len() - 1;
        if target[0] == n {
            candidates.push(last_i);
        }
        // cannot replace candidates itself; need to modify in-place
        let cand_copy = candidates.clone();
        candidates.clear();
        cand_copy.iter()
            .filter(|&&j| target[last_i - j] == n)
            .for_each(|&x| candidates.push(x));
        match candidates.first() {
            Some(&j) if j + target.len() <= recipes.len() => Some(j),
            _ => None,
        }
    };

    loop {
        let sum: usize = elves.iter().map(|&elf| recipes[elf]).sum();
        let (a, b) = (sum / 10, sum % 10);
        if a > 0 {
            if let Some(j) = push(&mut recipes, &mut candidates, a) {
                return j;
            }
        }
        if let Some(j) = push(&mut recipes, &mut candidates, b) {
            return j;
        }
        for elf in elves.iter_mut() {
            *elf = (*elf + recipes[*elf] + 1) % recipes.len();
        }
    }
}

fn main() {
    println!("Part 1: {:?}", part_1(47801));
    println!("Part 2: {:?}", part_2("047801"));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn part_1_examples() {
        assert_eq!(part_1(9),    "5158916779");
        assert_eq!(part_1(5),    "0124515891");
        assert_eq!(part_1(18),   "9251071085");
        assert_eq!(part_1(2018), "5941429882");
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(part_1(47801), "1342316410");
        });
    }

    #[test]
    fn part_2_examples() {
        assert_eq!(part_2("51589"), 9);
        assert_eq!(part_2("01245"), 5);
        assert_eq!(part_2("92510"), 18);
        assert_eq!(part_2("59414"), 2018);
    }

    #[bench]
    #[ignore]
    fn bench_part_2(b: &mut Bencher) {
        // takes ~13s
        b.iter(|| {
            assert_eq!(part_2("047801"), 20235230);
        });
    }
}
