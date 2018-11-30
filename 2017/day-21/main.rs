#![feature(slice_concat_ext)]

use std::fs::File;
use std::io::prelude::*;
use std::fmt;


fn get_input() -> String {
    let input_filename = "input.txt";
    // let input_filename = "test_input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}

type Fields = Vec<bool>;

fn sqrt_size(size: usize) -> usize {
    let root = (size as f32).sqrt();
    if root.fract() != 0. {
        panic!("Invalid size {:?} - does not have whole square root", size);
    }
    root as usize
}

#[derive(Debug, PartialEq, Clone)]
struct Mosaic {
    size: usize,
    fields: Fields,
}

impl fmt::Display for Mosaic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let chars =
            self.fields
            .iter()
            .map(|&b| match b {true => '#', false => '.'}).collect::<Vec<_>>();
        let chunks = chars.as_slice().chunks(self.size);
        let string: String =
            chunks.fold(String::new(), |mut v, chunk| {
                if v.len() != 0 {
                    v.push('/');
                }
                for &c in chunk {
                    v.push(c);
                }
                v
            });
        write!(f, "{}", string)
    }
}

impl Mosaic {
    fn new(fields: Fields) -> Self {
        Mosaic{size: sqrt_size(fields.len()), fields}
    }

    fn from_str(input: &str) -> Self {
        let fields: Fields =
            input
            .chars()
            .filter_map(|c| match c {
                '#'  => Some(true),
                '.'  => Some(false),
                '\n' |
                '/'  => None,
                _    => unimplemented!()
            })
            .collect();
        Mosaic::new(fields)
    }

    fn remapped(&self, order: &[usize]) -> Mosaic {
        assert_eq!(order.len(), self.fields.len());
        let new_fields: Vec<_> = order.iter().map(|&i| self.fields[i]).collect();
        Mosaic{size: self.size, fields: new_fields}
    }

    fn row(&self, row: usize) -> Fields {
        self.fields
            .iter()
            .skip(self.size * row)
            .take(self.size)
            .cloned()
            .collect()
    }
}


struct MosaicSet {
    size: usize,
    mosaic_size: usize,
    mosaics: Vec<Mosaic>,
}

impl MosaicSet {
    // split a "big mosaic" into smaller mosaics
    // that we have rules for (of size 2 or 3)
    fn from_fields(fields: &Fields) -> MosaicSet {
        let size = sqrt_size(fields.len());
        let chunk_size: usize =
            if size % 2 == 0 { 2 }
             else if size % 3 == 0 { 3 }
             else { unimplemented!() };
        let set_size = size / chunk_size;

        let mut mosaic_fields: Vec<Vec<bool>> = (0..set_size.pow(2)).map(|_| Vec::new()).collect();
        for (i, chunk) in fields.chunks(chunk_size).enumerate() {
            // invariant: (i+1) * chunk_size <= size
            let mos_col = i % set_size;
            let mos_row = i / chunk_size / set_size;

            let field_index = mos_row * set_size + mos_col;
            mosaic_fields[field_index].extend_from_slice(&chunk[..]);
        }
        let mosaics = mosaic_fields.into_iter().map(|f| Mosaic::new(f)).collect();
        MosaicSet::from_mosaics(mosaics)
    }

    fn from_mosaics(mosaics: Vec<Mosaic>) -> MosaicSet {
        let size = sqrt_size(mosaics.len());
        let mosaic_size = mosaics[0].size;
        MosaicSet{size, mosaic_size, mosaics}
    }

    fn row(&self, row: usize) -> Vec<Mosaic> {
        // alternatively implement a .rows iterator
        self.mosaics
            .iter()
            .skip(self.size * row)
            .take(self.size)
            .cloned()
            .collect()
    }

    fn to_fields(&self) -> Fields {
        let mut fields: Fields = Vec::new();
        for set_row_i in 0..self.size {
            let set_row = self.row(set_row_i);
            for mosaic_row_i in 0..self.mosaic_size {
                for mosaic in set_row.iter() {
                    fields.append(&mut mosaic.row(mosaic_row_i));
                }
            }
        }
        fields
    }

    fn translate(&mut self, rules: &RuleBook) -> MosaicSet {
        let (flip_indicies, rotate_indicies) = match self.mosaic_size {
            2 => (
                vec![
                    vec![0, 1, 2, 3], // identity
                    vec![1, 0, 3, 2], // flipped horizontally
                    vec![2, 3, 0, 1], // flipped vertically
                ],
                vec![
                    vec![2, 0, 3, 1], // rotate 90°
                    vec![3, 2, 1, 0], // rotate 180°
                    vec![1, 3, 0, 2], // rotate 270°
                ]),
            3 => (
                vec![
                    vec![0, 1, 2, 3, 4, 5, 6, 7, 8], // identity
                    vec![6, 7, 8, 3, 4, 5, 0, 1, 2], // flipped horizontally
                    vec![2, 1, 0, 5, 4, 3, 8, 7, 6], // flipped vertically
                ],
                vec![
                    vec![6, 3, 0, 7, 4, 1, 8, 5, 2], // rotate 90°
                    vec![8, 7, 6, 5, 4, 3, 2, 1, 0], // rotate 180°
                    vec![2, 5, 8, 1, 4, 7, 0, 3, 6], // rotate 270°
                ]),
            _ => unimplemented!(),
        };

        let new_mosaics: Vec<_> =
            self.mosaics
            .iter()
            .map(|mosaic| {
                for flip_map in flip_indicies.iter() {
                    let flipped_mosaic = mosaic.remapped(flip_map);
                    if let Some(m) = rules.lookup(&flipped_mosaic) {
                        return m;
                    }

                    for rotate_map in rotate_indicies.iter() {
                        let rotated_mosaic = flipped_mosaic.remapped(rotate_map);
                        if let Some(m) = rules.lookup(&rotated_mosaic) {
                            return m;
                        }
                    }
                }
                unimplemented!("no match in ruleset for {}", mosaic);
            })
            .cloned()
            .collect();
        MosaicSet::from_mosaics(new_mosaics)
    }
}


#[derive(Debug)]
struct RuleBook {
    rules: Vec<(Mosaic, Mosaic)>,
}

impl RuleBook {
    fn new(input: &str) -> RuleBook {
        let mut rules: Vec<(Mosaic, Mosaic)> = Vec::new();
        for line in input.split("\n") {
            let mosaics: Vec<_> = line.split(" => ").map(Mosaic::from_str).collect();
            assert_eq!(mosaics.len(), 2);

            let mut mosa_iter = mosaics.into_iter();
            let (to, from) = (mosa_iter.next().unwrap(), mosa_iter.next().unwrap());
            rules.push((to, from));
        }
        RuleBook{rules}
    }

    fn lookup(&self, needle: &Mosaic) -> Option<&Mosaic> {
        for &(ref from, ref to) in self.rules.iter() {
            if *from == *needle {
                return Some(to);
            }
        }
        None
    }
}


fn main() {
    let input_str = get_input();
    let input = input_str.trim();
    let base_str = ".#./..#/###"; //"###/..#/.#.";

    let rules = RuleBook::new(&input);
    input.split("\n");
    let base_mosaic = Mosaic::from_str(base_str);

    let mut current_mosaic = base_mosaic;
    // for i in 1..3 { // 2 steps for test input
    // for i in 1..6 { // 5 steps for first part
    for i in 1..19 {
        let mut set = MosaicSet::from_fields(&current_mosaic.fields);
        set = set.translate(&rules);
        current_mosaic = Mosaic::new(set.to_fields());
        let num_active = current_mosaic.fields.iter().filter(|x| **x).count();
        println!("Step {}; #active fields: {}; set size: {}; mosaic size: {}",
                 i, num_active, set.size, set.mosaic_size);
        // println!("full mosaic: {}", current_mosaic);
    }
}
