#![feature(test)]

#[macro_use] extern crate itertools;
extern crate test;

use std::collections::VecDeque;

trait Rotate<T> {
    fn rotate(&mut self, i: isize);
}

impl<T> Rotate<T> for VecDeque<T> {
    fn rotate(&mut self, i: isize) {
        if self.len() == 0 {
            return;
        }
        let left = i < 0;
        for _ in 0..i.abs() {
            if left {
                let x = self.pop_front().unwrap();
                self.push_back(x);
            } else {
                let x = self.pop_back().unwrap();
                self.push_front(x);
            }
        }
    }
}


fn process(players: usize, last_marble: usize) -> usize {
    let mut points = vec![0usize; players];
    let mut marbles: VecDeque<usize> = VecDeque::new();
    marbles.push_back(0);
    for (n, player) in izip!(1..=last_marble, (0..players).cycle()) {
        if n % 23 == 0 {
            marbles.rotate(7);
            points[player] += n + marbles.pop_front().expect("index error");
        } else {
            marbles.rotate(-2);
            marbles.push_front(n);
        }
    }
    points.into_iter().max().expect("no maximum")
}

fn main() {
    println!("part 1: {:?}", process(465, 71498));
    println!("part 2: {:?}", process(465, 71498 * 100));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn simple_input() {
        assert_eq!(process(9, 25), 32);
    }

    #[test]
    fn more_input() {
        assert_eq!(process(10, 1618), 8317);
        assert_eq!(process(13, 7999), 146373);
        assert_eq!(process(17, 1104), 2764);
        assert_eq!(process(21, 6111), 54718);
        assert_eq!(process(30, 5807), 37305);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(process(465, 71498), 383475);
        });
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(process(465, 71498 * 100), 3148209772);
        });
    }
}
