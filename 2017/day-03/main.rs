#[derive(Debug)]
enum Direction {
    RIGHT,
    UP,
    LEFT,
    DOWN,
}

impl Direction {
    fn get_next(&self) -> Self {
        match *self {
            Direction::RIGHT => Direction::UP,
            Direction::UP => Direction::LEFT,
            Direction::LEFT => Direction::DOWN,
            Direction::DOWN => Direction::RIGHT,
        }
    }
}

#[derive(Debug)]
struct Position {
    // 17  16  15  14  13
    // 18   5   4   3  12
    // 19   6   1   2  11
    // 20   7   8   9  10
    // 21  22  23---> ...
    direction: Direction,
    dimension: usize,
    dimension_size: usize,
    dimension_pos: usize,
}

impl Default for Position {
    fn default() -> Position {
        Position { direction: Direction::RIGHT,
                   dimension: 0,
                   dimension_size: 1,
                   dimension_pos: 0 }
    }
}

impl Position {
    fn increase(&mut self) {
        self.dimension_pos += 1;
        if self.dimension_pos == self.dimension_size {
            self.turn()
        }
    }

    fn turn(&mut self) {
        self.direction = self.direction.get_next();
        self.dimension = match self.direction {
            Direction::UP => self.dimension + 1,
            _             => self.dimension,
        };
        self.dimension_size = self.dimension * 2;
        self.dimension_pos = 0;
    }

    fn get_steps(&self) -> usize {
        let mut steps: usize;
        let half_dimension = (self.dimension_size + 1) / 2; // ceil for index 1 with size 1
        // go to the middle of the side
        if self.dimension_pos >= half_dimension {
            steps = self.dimension_pos - half_dimension + 1;
        }
        else {
            steps = half_dimension - (self.dimension_pos + 1);
        }
        // go to the center
        steps += self.dimension;
        steps
    }
}


fn calc_steps(num: usize) -> usize {
    if num == 0 {
        panic!("Cannot handle 0")
    }
    let mut position = Position::default();
    let mut i = 1;
    while i < num {
        position.increase();
        i += 1;
    }
    println!("position for {}: {:?}", num, position);
    position.get_steps()
}


fn main() {
    let input = 361527;

    assert_eq!(calc_steps(1), 0);
    assert_eq!(calc_steps(2), 1);
    assert_eq!(calc_steps(3), 2);
    assert_eq!(calc_steps(4), 1);
    assert_eq!(calc_steps(8), 1);
    assert_eq!(calc_steps(9), 2);
    assert_eq!(calc_steps(10), 3);
    assert_eq!(calc_steps(11), 2);

    let steps = calc_steps(input);
    println!("The number of required steps is: {}", steps);
}
