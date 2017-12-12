use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;

fn get_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


const DIRECTIONS: [&str; 6] = ["n", "ne", "se", "s", "sw", "nw"];

const PAIRS: [(&str, &str, usize); 5] = [
    // the list is prioritized
    ("n", "s", 0),
    ("ne", "sw", 0),
    ("se", "nw", 0),
    ("se", "sw", 1),
    ("ne", "nw", 1),
];


fn main() {
    let input_str = get_input();
    let input = input_str.trim();

    let mut count_map: HashMap<&str, usize> = HashMap::new();
    for dir in DIRECTIONS.iter() {
        count_map.insert(dir, 0);
    }

    let directions: Vec<&str> = input.split(",").collect();
    for dir in directions {
        let mut count = count_map.get_mut(dir).unwrap();
        *count += 1;
    }
    println!("count_map: {:?}", count_map);

    let mut steps: usize = 0;
    for &(dir1, dir2, steps_total) in PAIRS.iter() {
        let dirs = [dir1, dir2];
        let min = dirs.iter().map(|x| count_map[x]).min().unwrap();
        if min == 0 {
            continue;
        }
        steps += min * steps_total;
        for dir in dirs.iter() {
            let mut count = count_map.get_mut(dir).unwrap();
            *count -= min;
        }
    }

    println!("after reducing");
    println!("count_map: {:?}", count_map);
    println!("steps: {:?}", steps);
    for v in count_map.values() {
        steps += v;
    }
    println!("final steps: {:?}", steps);
}
