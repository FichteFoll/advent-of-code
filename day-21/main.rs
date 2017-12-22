#![feature(slice_concat_ext)]

use std::fs::File;
use std::io::prelude::*;
use std::fmt;
use std::collections::HashSet;


fn get_input() -> String {
    // let input_filename = "input.txt";
    let input_filename = "test_input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}

type Fields = Vec<bool>;

fn fields_size(fields: &Fields) -> usize {
    let root = (fields.len() as f32).sqrt();
    if root.fract() != 0. {
        panic!("Invalid size {:?} - does not have whole square root", fields.len());
    }
    root as usize
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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
        Mosaic{size: fields_size(&fields), fields}
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

    fn with_fields_reordered(&self, order: &[usize]) -> Mosaic {
        assert_eq!(order.len(), self.size.pow(2));
        let new_fields: Vec<_> = order.iter().map(|&i| self.fields[i]).collect();
        Mosaic{size: self.size, fields: new_fields}
    }
}

enum Lookup<T: Clone> {
    Found(T),
    New(T)
}


struct MosaicSet {
    mosaics: Vec<Mosaic>,
}

impl MosaicSet {
    // split a "big mosaic" into multiple slices
    // that we have rules for (of size 2 or 3)
    fn from_fields(fields: &Fields) -> MosaicSet {
        let size = fields_size(fields);
        let chunk_size =
            (if size % 2 == 0 { 2 }
             else if size % 3 == 0 { 3 }
             else { unimplemented!() } as usize)
            .pow(2);

        let mosaics: Vec<_> =
            fields
            .chunks(chunk_size)
            .map(|chunk| Mosaic::new(chunk.into_iter().cloned().collect()))
            .collect();
        MosaicSet{mosaics}
    }

    fn to_fields(&self) -> Fields {
        // TODO this is wrong
        self.mosaics.iter().flat_map(|x| x.fields.iter().cloned()).collect()
    }

    fn translate(&mut self, rules: &RuleBook) {
        static FLIP_INDICIES: [[usize; 9]; 3] = [
            [0, 1, 2, 3, 4, 5, 6, 7, 8], // normal
            [6, 7, 8, 3, 4, 5, 0, 1, 2], // flipped horizontally
            [2, 1, 0, 5, 4, 3, 8, 7, 6], // flipped vertically
        ];
        static ROTATE_INDICIES: [[usize; 9]; 4] = [
            [0, 1, 2, 3, 4, 5, 6, 7, 8], // normal
            [6, 3, 0, 7, 4, 1, 8, 5, 2], // rotate 90°
            [8, 7, 6, 5, 4, 3, 2, 1, 0], // rotate 180°
            [2, 5, 8, 1, 4, 7, 0, 3, 6], // rotate 270°
        ];

        let new_mosaics: Vec<_> =
            self.mosaics
            .iter()
            // TODO debug this
            .map(|mosaic| {
                // TODO use a macro
                let mut all = Vec::new(); // must live longer than cache
                let mut cache = HashSet::new();
                if let Some(m) = rules.lookup(&mosaic) {
                    return m;
                }
                cache.insert(mosaic.clone());
                for flip_map in FLIP_INDICIES.iter() {
                    let flipped_mosaic = {
                        let mos = mosaic.with_fields_reordered(flip_map);
                        match self.cache(&mut cache, mos) {
                            Some(mos_ref) => mos_ref,
                            None => continue,
                        }
                        if let Some(mos_ref) =  {
                            return mos_ref
                        }
                    println!("base: {} flipped: {}", mosaic, flipped_mosaic);
                    let flipped_mosaic = &all[all.len() - 1];
                    if let Some(m) = self.cached_lookup(&rules, &mut cache, &flipped_mosaic) {
                        return m;
                    }
                    for rotate_map in ROTATE_INDICIES.iter() {
                        let rotated_mosaic = flipped_mosaic.with_fields_reordered(rotate_map);
                        println!("base: {} rotated: {}", flipped_mosaic, rotated_mosaic);
                        for &(ref from, ref to) in rules.rules.iter() {
                            if *from == rotated_mosaic {
                                println!("translating to {}", to);
                                return to;
                            }
                        }
                    }
                }
                panic!("no match in ruleset for {}", mosaic);
            })
            .cloned()
            .collect();
        self.mosaics = new_mosaics;
    }

    fn cache<'a>(cache: &'a mut HashSet<Mosaic>, needle: Mosaic) -> Lookup<&'a Mosaic> {
        match cache.get(&needle) {
            Some(found) => Lookup::Found(found),
            None => {
                let ref need = needle;
                cache.insert(needle);
                Lookup::New(need)
            }
        }
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
            println!("rule {:?}", mosaics);
            assert_eq!(mosaics.len(), 2);
            let mut mosa_iter = mosaics.into_iter();
            let (to, from) = (mosa_iter.next().unwrap(), mosa_iter.next().unwrap());
            rules.push((from, to));
        }
        RuleBook{rules}
    }

    fn lookup(&self, needle: &Mosaic) -> Option<&Mosaic> {
        for &(ref from, ref to) in self.rules.iter() {
            if *from == *needle {
                println!("translating to {}", to);
                return Some(to);
            }
        }
        None
    }
}



fn main() {
    let input_str = get_input();
    let input = input_str.trim();
    let base_str = "###/..#/.#.";

    let rules = RuleBook::new(&input);
    input.split("\n");
    let base = Mosaic::from_str(base_str);

    let mut current_mosaic = base;
    for i in 1..4 {
        let mut set = MosaicSet::from_fields(&current_mosaic.fields);
        set.translate(&rules);
        current_mosaic = Mosaic::new(set.to_fields());
        let num_active = current_mosaic.fields.iter().filter(|x| **x).count();
        println!("Step {}; #active fields: {}; full mosaic: {}",
                 i, num_active, current_mosaic);
    }
}
