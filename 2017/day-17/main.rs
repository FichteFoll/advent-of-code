#[derive(Debug)]
struct Spinlock {
    buffer: Vec<usize>,
    position: usize,
}

impl Spinlock {
    fn new() -> Self {
        Spinlock{buffer: vec![0], position: 0}
    }

    fn fill(&mut self, skip: usize, steps: usize) {
        let mut i = self.position;
        for value in 1..(steps + 1) {
            i = (i + skip) % self.buffer.len() + 1;
            self.buffer.insert(i, value);
            // println!("buffer: {:?}; i: {}", self.buffer, i);
        }
        self.position = i;
    }

    fn next_val(&self) -> usize {
        let i = (self.position + 1) % self.buffer.len();
        self.buffer[i]
    }
}


fn main() {
    let mut spinlock = Spinlock::new();

    // spinlock.fill(3, 10);
    spinlock.fill(354, 2017);
    println!("next val: {}", spinlock.next_val());
}
