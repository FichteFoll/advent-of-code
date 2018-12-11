#![feature(test)]

#[macro_use] extern crate itertools;
extern crate test;



#[derive(Clone, Debug, Eq, PartialEq)]
struct Pt(usize, usize);

impl Pt {
    fn power_level(&self, serial: usize) -> i8 {
        let rack = self.0 + 10;
        let total = (rack * self.1 + serial) * rack;
        ((total / 100) % 10) as i8 - 5
    }
}


fn process(grid_size: usize, serial: usize) -> Pt {
    let mut grid = vec![0i8; grid_size * grid_size];
    // fill grid
    for (x, y) in iproduct!(1..=grid_size, 1..=grid_size) {
        let i = (x - 1) + (y - 1) * grid_size;
        grid[i] = Pt(x, y).power_level(serial);
    }

    // calc sums
    let (res, _) = iproduct!(1..=(grid_size - 2), 1..=(grid_size - 2))
        .map(|(x, y)| -> (Pt, i8) {
            let power = (0..3).map(|row_offset| {
                let first_i = (x - 1) + (y - 1 + row_offset) * grid_size;
                grid[first_i] + grid[first_i + 1] + grid[first_i + 2]
            }).sum();
            (Pt(x, y), power)
        }).max_by_key(|(_, p)| *p).expect("no maximum");
    res
}


fn main() {
    println!("The result is {:?}", process(300, 5235));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn simple_power_level() {
        assert_eq!(Pt(3, 5).power_level(8), 4);
    }

    #[test]
    fn more_power_level() {
        assert_eq!(Pt(122,  79).power_level(57), -5);
        assert_eq!(Pt(217, 196).power_level(39),  0);
        assert_eq!(Pt(101, 153).power_level(71),  4);
    }

    #[test]
    fn simple_input() {
        assert_eq!(process(300, 18), Pt(33, 45));
        assert_eq!(process(300, 42), Pt(21, 61));
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(process(300, 5235), Pt(33, 54));
        });
    }
}
