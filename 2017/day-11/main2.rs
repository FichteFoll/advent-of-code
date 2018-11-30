use std::fs::File;
use std::io::prelude::*;


fn get_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


fn main() {
    let input_str = get_input();
    let input = input_str.trim();

    let directions: Vec<&str> = input.split(",").collect();
    let mut pos: (isize, isize) = (0, 0); // x, y; increasing numbers point right and downwards
    let mut max_distance = 0;
    for dir in directions {
        let pos_adjust = match dir {
            "n"  => ( 0, -2),
            "ne" => ( 1, -1),
            "se" => ( 1,  1),
            "s"  => ( 0,  2),
            "sw" => (-1,  1),
            "nw" => (-1, -1),
            _ => panic!("unexpected: {}", dir),
        };
        pos = (pos.0 + pos_adjust.0, pos.1 + pos_adjust.1);
        let distance = (pos.0.abs() + pos.1.abs()) / 2;
        if distance > max_distance {
            max_distance = distance;
        }
    }
    println!("max distance: {}", max_distance);
    // bonus simple implementation of part 1
    let final_distance = (pos.0.abs() + pos.1.abs()) / 2;
    println!("final distance: {}", final_distance);
}
