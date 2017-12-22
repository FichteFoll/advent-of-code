use std::fs::File;
use std::io::prelude::*;
use std::ops::{Add, AddAssign};
use std::collections::HashSet;


fn get_input() -> String {
    let input_filename = "input.txt";
    // let input_filename = "test_input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Pt(isize, isize);

impl Pt {
    // these are supposed to be used like "uniform" vectors
    fn up() -> Pt {
        Pt(0, -1)
    }

    // fn right() -> Pt {
    //     Pt(1, 0)
    // }

    // fn down() -> Pt {
    //     Pt(0, 1)
    // }

    // fn left() -> Pt {
    //     Pt(-1, 0)
    // }

    fn rot_left(self) -> Pt {
        // ( 0, -1) => (-1,  0)
        // ( 1,  0) => ( 0, -1)
        // ( 0,  1) => ( 1,  0)
        // (-1,  0) => ( 0,  1)
        Pt(self.1, -self.0)
    }

    fn rot_right(self) -> Pt {
        // ( 0, -1) => ( 1,  0)
        // ( 1,  0) => ( 0,  1)
        // ( 0,  1) => (-1,  0)
        // (-1,  0) => ( 0, -1)
        Pt(-self.1, self.0)
    }
}

impl Add for Pt{
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Pt(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl AddAssign for Pt{
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
        self.1 += other.1;
    }
}


fn main() {
    let input_str = get_input();
    let input = input_str.trim();

    let size = input.chars().filter(|&x| x == '\n').count() + 1;
    assert!(size % 2 == 1);
    let base_i = -(size as isize / 2);

    let mut infected: HashSet<Pt> = HashSet::new();
    for (i, line) in input.split("\n").enumerate() {
        for (j, c) in line.chars().enumerate() {
            match c {
                '#' => {infected.insert(Pt(base_i + j as isize, base_i + i as isize));},
                '.' => (),
                _   => unimplemented!(),
            }
        }
    }
    println!("initially infected: {:?}", infected);

    let mut pos = Pt(0, 0);
    let mut dir = Pt::up();
    let mut times_infected = 0;
    for _i in 0..10000 {
        if let Some(_) = infected.take(&pos) {
            dir = dir.rot_right();
        }
        else {
            dir = dir.rot_left();
            infected.insert(pos.clone());
            times_infected += 1;
        }
        pos += dir;
        // println!("step: {}, pos: {:?}, dir: {:?}, infected: {:?}",
        //          _i + 1, pos, dir, infected);
    }
    println!("times infected: {:?}", times_infected);
    println!("num infected: {}", infected.iter().count());
}
