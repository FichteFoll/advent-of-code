use std::cell::Cell;
use std::iter::Cycle;
use std::slice::Iter;


const LIST_SIZE: usize = 256;
const ROTATIION_SUFFIX: [u8; 5] = [17, 31, 73, 47, 23];


struct KnotHash {
    list: Vec<Cell<u8>>,
    rotations: Vec<u8>,
}


impl KnotHash {
    fn _round(&self, list_iter: &mut Cycle<Iter<Cell<u8>>>, mut skip: usize) -> usize {
        for &n in self.rotations.iter() {
            let section: Vec<_> = list_iter.clone().take(n as usize).collect();

            for (i, cell1) in section.iter().rev().enumerate() {
                if let Some(cell2) = list_iter.next() {
                    if i < section.len() / 2 {
                        cell1.swap(&cell2);
                    }
                }
                else {
                    panic!("This should never happen");
                }
            }
            for _ in 0..skip {
                list_iter.next();
            }
            skip += 1;
        }
        skip
    }

    fn xor(&self) -> [u8; 16] {
        let mut result = [0u8; 16];
        let mut iter = self.list.iter();
        for i in 0..result.len() {
            for _ in 0..16 {
                result[i] ^= iter.next().unwrap().get();
            }
        }
        result
    }

    fn run(&mut self) -> [u8; 16] {
        let mut list_iter = self.list.iter().cycle();
        let mut skip: usize = 0;
        for _ in 0..64 {
            skip = self._round(&mut list_iter, skip);
        }
        self.xor()
    }

    fn hash(input: &Vec<u8>) -> [u8; 16] {
        let list: Vec<Cell<u8>> = (0..LIST_SIZE).map(|x| Cell::new(x as u8)).collect();
        let mut rotations = input.clone();
        rotations.extend_from_slice(&ROTATIION_SUFFIX[..]);
        let mut hash = KnotHash{list, rotations};
        hash.run()
    }
}


fn hash_to_str(hash: &[u8]) -> String {
    let mut result = String::new();
    for byte in hash.iter() {
        let byte_str = format!("{:02x}", byte);
        result.push_str(&byte_str[..]);
    }
    result
}


fn main() {
    let input_str = "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62";
    // let input_str = "AoC 2017";
    let input: Vec<u8> = input_str.bytes().collect();
    // const LIST_SIZE: usize = 5;
    // let input: [usize; 4] = [3, 4, 1, 5];
    let hash = KnotHash::hash(&input);
    let hash_str = hash_to_str(&hash[..]);

    println!("hash: {:?}", hash);
    println!("hash_str: {:?}", hash_str);
}
