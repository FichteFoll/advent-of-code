use std::fs::File;
use std::io::prelude::*;
use std::ops::Add;
use std::slice::Iter;


fn get_input() -> String {
    // let input_filename = "input.txt";
    let input_filename = "test_input.txt";
    let mut f = File::open(input_filename).expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");
    contents
}


fn make_grid(input: &str) -> Vec<Vec<char>> {
    let mut lines: Vec<Vec<char>> = Vec::new();
    for line in input.split("\n") {
        let chars: Vec<char> = line.chars().collect();
        lines.push(chars);
    }
    lines
}


#[derive(Debug, PartialEq, Copy, Clone)]
struct Point {
    x: usize,
    y: usize,
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Debug)]
enum Direction {
    RIGHT,
    DOWN,
    LEFT,
    UP,
}

impl Direction {
    pub fn iterator() -> Iter<'static, Direction> {
        use self::Direction::*;
        static DIRECTIONS: [Direction;  4] = [RIGHT, DOWN, LEFT, UP];
        DIRECTIONS.into_iter()
    }

    fn value(&self) -> i8 {
        // needed for comparisons
        use self::Direction::*;
        match *self {
            RIGHT => 1,
            DOWN  => 2,
            LEFT  => -1,
            UP    => -2,
        }
    }

    fn move_point(&self, point: &mut Point) {
        use self::Direction::*;
        let (adj_x, adj_y) = match *self {
            RIGHT => ( 1,  0),
            DOWN  => ( 0,  1),
            LEFT  => (-1,  0),
            UP    => ( 0, -1),
        };
        point.x = (point.x as isize + adj_x) as usize;
        point.y = (point.y as isize + adj_y) as usize;
    }

    fn next_point(&self, mut point: Point) -> Point {
        // let mut next_point = (*point).clone();
        self.move_point(&mut point);
        point
    }
}


fn main() {
    let input_str = get_input();
    let input = &input_str[..];
    // let input = input_str.trim();

    // grid uses y, x coordinates
    let grid = make_grid(input);
    // println!("grid: {:?}", grid);

    // find start cell
    let mut pos = Point{x: 0, y: 0};
    for (i, cell) in grid[0].iter().enumerate() {
        if *cell == '|' {
            println!("{}, {}", i, cell);
            pos = Point{x: i, y: 0};
            break
        }
    }

    println!("start pos: {:?}", pos);
    assert!(pos != Point{x: 0, y: 0});
    let mut direction = &Direction::DOWN;

    // we start at 0 because we also count the last step into an empty cell
    let mut steps: usize = 0;
    let mut chars: Vec<char> = Vec::new();
    loop {
        direction.move_point(&mut pos);
        steps += 1;
        let cell = &grid[pos.y][pos.x];
        println!("pos {:?}, cell {:?}; direction {:?}", pos, cell, direction);
        match *cell {
            ' ' => break, // reached the end
            '|' |
            '-' => continue,
            '+' => (),
            _   => {chars.push(*cell); continue},
        }

        // need to determine new direction
        for dir in Direction::iterator() {
            if dir.value() == -direction.value() {
                // don't check opposite direction
                continue
            }
            let test_pos = dir.next_point(pos);
            let test_cell = &grid[test_pos.y][test_pos.x];
            if *test_cell == ' ' {
                continue
            }
            direction = dir;
            break
        };
    };

    let text: String = chars.iter().collect();
    println!("Collected text: {}", text);
    println!("steps: {}", steps);
}
