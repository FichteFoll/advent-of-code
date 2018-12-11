#![feature(test)]

#[macro_use] extern crate itertools;
extern crate test;


#[derive(Clone, Debug, Eq, PartialEq)]
struct Pt(usize, usize);

impl Pt {
    fn power_level(&self, serial: usize) -> i32 {
        let rack = self.0 + 10;
        let total = (rack * self.1 + serial) * rack;
        ((total / 100) % 10) as i32 - 5
    }
}


#[derive(Clone, Debug)]
struct Grid<T> {
    fields: Vec<T>,
    size: usize,
    default: T,
}

impl<T: Copy> Grid<T> {
    fn new(size: usize, default: T) -> Self {
        let fields = vec![default.clone(); size * size];
        Grid {fields, size, default}
    }

    fn get(&self, x: usize, y: usize) -> T {
        self.fields[x + y * self.size]
    }

    fn get_default(&self, x: i32, y: i32) -> T {
        let i_size = self.size as i32;
        let i = x + y * i_size;
        if x < 0 || y < 0 || x >= i_size || y >= i_size {
            self.default
        } else {
            self.fields[i as usize]
        }
    }

    fn set(&mut self, x: usize, y: usize, value: T) {
        self.fields[x + y * self.size] = value;
    }
}

impl<T> Grid<T>
    where T: Copy + std::ops::Add<Output=T> + std::ops::Sub<Output=T>
{
    fn build_summed(&self) -> Self {
        let mut summed_grid = self.clone();
        for row in 0..self.size {
            for col in 0..self.size {
                let (irow, icol) = (row as i32, col as i32);
                let value = self.get(col, row)
                    + summed_grid.get_default(icol - 1, irow)
                    + summed_grid.get_default(icol,     irow - 1)
                    - summed_grid.get_default(icol - 1, irow - 1);
                summed_grid.set(col, row, value);
            }
        }
        summed_grid
    }

    fn area_sum (&self, x: usize, y: usize, size: usize) -> T {
        let (row, col) = (y as i32 - 1, x as i32 - 1);
        self.get(col as usize + size - 1, row as usize + size - 1)
            + self.get_default(col - 1, row - 1)
            - self.get_default(col + size as i32 - 1, row - 1)
            - self.get_default(col - 1, row + size as i32 - 1)
    }
}


fn process(grid_size: usize, serial: usize, square_size: Option<usize>) -> (Pt, usize) {
    let mut grid = Grid::new(grid_size, 0i32);
    for (x, y) in iproduct!(1..=grid_size, 1..=grid_size) {
        grid.set(x - 1, y - 1, Pt(x, y).power_level(serial));
    }
    let summed_grid = grid.build_summed();

    let square_iter = match square_size {
        Some(n) => n..=n,
        None => 2..=grid_size, // 1 is basically impossible, so skip it
    };
    let (pt, square, _) = square_iter.map(|square| {
        let max_i = grid_size - square + 1;
        let (pt, power) = iproduct!(1..=max_i, 1..=max_i)
            .map(|(x, y)| -> (Pt, i32) {
                (Pt(x, y), summed_grid.area_sum(x, y, square))
            }).max_by_key(|(_, p)| *p).expect("no maximum");
        (pt, square, power)
    }).max_by_key(|(_, _, p)| *p).expect("no maximum 2");
    (pt, square)
}


fn main() {
    println!("Part 1: {:?}", process(300, 5235, Some(3)).0);
    println!("Part 2: {:?}", process(300, 5235, None));
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
        assert_eq!(process(300, 18, Some(3)), (Pt(33, 45), 3));
        assert_eq!(process(300, 42, Some(3)), (Pt(21, 61), 3));
    }

    #[bench]
    fn bench_part_1(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(process(300, 5235, Some(3)), (Pt(33, 54), 3));
        });
    }

    #[test]
    fn part_2_examples() {
        assert_eq!(process(300, 18, None), (Pt(90, 269), 16));
        assert_eq!(process(300, 42, None), (Pt(232, 251), 12));
    }

    #[bench]
    fn bench_part_2(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(process(300, 5235, None), (Pt(232, 289), 8));
        });
    }
}
