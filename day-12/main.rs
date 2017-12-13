use std::fs::File;
use std::io::prelude::*;


fn get_input() -> String {
    let input_filename = "input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}

struct PipeAssociation {
    pipe: usize,
    neighbours: Vec<usize>,
}

impl PipeAssociation {
    fn new(string: &str) -> Self {
        let segments: Vec<_> = string.split(" <-> ").collect();
        let pipe: usize = segments[0].parse().unwrap();
        let neighbours: Vec<usize> = segments[1].split(", ").map(|x| x.parse().unwrap()).collect();
        PipeAssociation{pipe, neighbours}
    }
}

fn main() {
    let input_str = get_input();
    let input = input_str.trim();

    let mut associations: Vec<PipeAssociation> = vec![];

    for line in input.split("\n") {
        associations.push(PipeAssociation::new(line))
    }

    let mut pipes_visited: Vec<usize> = vec![];
    let mut pipes_to_visit: Vec<usize> = vec![0];

    while let Some(pipe) = pipes_to_visit.pop() {
        if pipes_visited.contains(&pipe) {
            continue
        }
        pipes_visited.push(pipe);
        pipes_to_visit.extend(&associations[pipe].neighbours[..]);
    }

    // let mut group: Vec<usize> = Vec::new();
    println!("group size: {}", pipes_visited.len());
}
