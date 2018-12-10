#![feature(test)]

#[macro_use] extern crate itertools;
extern crate test;

use std::collections::LinkedList;

trait BetterLL<T> {
    fn insert_at(&mut self, i: usize, x: T);
    fn pop_at(&mut self, i: usize) -> Option<T>;
}

impl<T> BetterLL<T> for LinkedList<T> {
    // split_off is O(n), the rest O(1)
    fn insert_at(&mut self, i: usize, x: T) {
        let mut ll2 = self.split_off(i);
        self.push_back(x);
        self.append(&mut ll2);
    }

    fn pop_at(&mut self, i: usize) -> Option<T> {
        let mut ll2 = self.split_off(i);
        let ret = ll2.pop_front();
        self.append(&mut ll2);
        ret
    }
}


fn process(players: usize, last_marble: usize) -> usize {
    let mut points = vec![0usize; players];
    let mut marbles: LinkedList<usize> = LinkedList::new();
    marbles.push_back(0);
    let mut current = 0usize;
    for (n, player) in izip!(1..=last_marble, (0..players).cycle()) {
        if n % 23 == 0 {
            current = (current + marbles.len() - 7) % marbles.len();
            points[player] += n + marbles.pop_at(current).expect("index error");
        } else {
            current = (current + 2) % marbles.len();
            if current == 0 {
                current = marbles.len();
                marbles.push_back(n);
            } else {
                marbles.insert_at(current, n);
            }
        }
    }
    points.into_iter().max().expect("no maximum")
}

fn main() {
    // println!("The result is {:?}", process(465, 71498));
    println!("The result is {:?}", process(465, 71498 * 100));
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

    #[test]
    #[ignore]
    fn part2() {
        // runs for 2800s, probably (that's 47min)
        assert_eq!(process(465, 71498 * 100), 3148209772);
    }

    #[bench]
    #[ignore]
    fn bench_real_input(b: &mut Bencher) {
        // this takes ~28s to run
        b.iter(|| {
            assert_eq!(process(465, 71498), 383475);
        });
    }
}
