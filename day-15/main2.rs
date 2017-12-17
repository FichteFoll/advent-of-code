struct Generator {
    modulo: usize,
    factor: usize,
    last_value: usize,
}

impl Generator {
    fn new(modulo: usize, factor: usize, initial_value: usize) -> Generator {
        Generator{modulo, factor, last_value: initial_value}
    }

    fn next(&mut self) -> usize {
        let value = (self.last_value * self.factor) % self.modulo;
        self.last_value = value;
        value
    }
}

fn check_match(values: &[usize]) -> bool {
    let truncated_val = values[0] as u16; // truncate
    for value in values[1..].iter() {
        let new_val = *value as u16;
        if new_val != truncated_val {
            return false
        }
    }
    true
}

fn main() {
    // let mut generators = [
    //     Generator::new(2147483647, 16807, 65),
    //     Generator::new(2147483647, 48271, 8921),
    // ];

    // for _ in 0..5 {
    //     let new_values: [usize; 2] = [generators[0].next(), generators[1].next()];
    //     println!("new_values: {:032b} {:032b}; match: {}", new_values[0], new_values[1], check_match(&new_values[..]));
    // }

    let mut generators = [
        Generator::new(2147483647, 16807, 883),
        Generator::new(2147483647, 48271, 879),
    ];

    let mut count: usize = 0;
    for _ in 0..40_000_000 {
        let new_values: [usize; 2] = [generators[0].next(), generators[1].next()];
        if check_match(&new_values[..]) {
            count += 1;
        }
    }
    println!("count: {}", count);
}
