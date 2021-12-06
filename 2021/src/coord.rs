use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Sub};

use itertools::Itertools;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point<const N: usize> {
    pub coord: [i32; N],
}

impl<const N: usize> Default for Point<N> {
    fn default() -> Self {
        Point { coord: [0; N] }
    }
}

impl<const N: usize> Point<N> {
    pub const ZERO: Self = Self { coord: [0; N] };

    pub fn new(coord: [i32; N]) -> Self {
        Point { coord }
    }

    pub fn from_padded(slice: &[i32]) -> Self {
        let mut coord = [0; N];
        for i in 0..N.min(slice.len()) {
            coord[i] = slice[i];
        }
        Self { coord }
    }

    #[inline]
    pub fn x(&self) -> i32 {
        self.coord[0]
    }

    #[inline]
    pub fn y(&self) -> i32 {
        self.coord[1]
    }

    #[inline]
    pub fn z(&self) -> i32 {
        self.coord[2]
    }

    // TODO
    // pub fn neighbors(&self) -> Vec<Point<N>> {
    //     vec![];
    // }

    pub fn normalized(&self) -> Self {
        if self.coord.iter().filter(|&&n| n != 0).count() == 1 {
            let mut coord = [0i32; N];
            for i in 0..N {
                coord[i] = coord[i].signum();
            }
            Self { coord }
        } else {
            let d = self.coord.iter().cloned().fold1(gcd).unwrap();
            self / d
        }
    }

    pub fn manhattan(&self) -> i32 {
        self.coord.iter().cloned().map(i32::abs).sum()
    }
}

impl<const N: usize> Display for Point<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.coord.iter();
        write!(f, "Point<{}>([{}", N, iter.next().unwrap())?;
        for n in iter {
            write!(f, ", {}", n)?;
        }
        write!(f, "])")
    }
}

impl<const N: usize> Add for Point<N> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let mut coord = self.coord;
        for i in 0..N {
            coord[i] += rhs.coord[i];
        }
        Self::Output { coord }
    }
}

impl<const N: usize> Sub for Point<N> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        let mut coord = self.coord;
        for i in 0..N {
            coord[i] -= rhs.coord[i];
        }
        Self::Output { coord }
    }
}

impl<const N: usize> Add<i32> for Point<N> {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        let mut coord = self.coord;
        for n in coord.iter_mut() {
            *n += rhs;
        }
        Self::Output { coord }
    }
}

impl<const N: usize> Div<i32> for &Point<N> {
    type Output = Point<N>;
    fn div(self, rhs: i32) -> Self::Output {
        let mut coord = self.coord;
        for n in coord.iter_mut() {
            *n /= rhs;
        }
        Self::Output { coord }
    }
}

// (const) generics not yet supported by auto_ops crate.
// impl_op_ex!(+ |a: &Point<2>, b: &i32| -> Point<2> { Point { coord: [a.coord[0] + b, a.coord[1] + b] } });
// impl_op_ex!(+ |a: &Point<const N: usize>, b: &i32| -> Point {
//     let mut coord = self.coord.clone();
//     for i in 0..N {
//         coord[i] += rhs;
//     }
//     Self { coord }
// });

mod point2 {
    use super::Point;

    impl Point<2> {
        pub const NW: Self = Self { coord: [-1, -1] };
        pub const N:  Self = Self { coord: [ 0, -1] };
        pub const NE: Self = Self { coord: [ 1, -1] };
        pub const W:  Self = Self { coord: [-1,  0] };
        pub const E:  Self = Self { coord: [ 1,  0] };
        pub const SW: Self = Self { coord: [-1,  1] };
        pub const S:  Self = Self { coord: [ 0,  1] };
        pub const SE: Self = Self { coord: [ 1,  1] };

        pub fn rotate_right(&mut self, by: i32) {
            self.rotate_left(360 - by);
        }

        pub fn rotate_left(&mut self, by: i32) {
            match by % 360 {
                0 => (),
                180 => {
                    self.coord[0] *= -1;
                    self.coord[1] *= -1;
                },
                90 => {
                    self.coord.swap(0, 1);
                    self.coord[1] *= -1;
                },
                270 => {
                    self.coord.swap(0, 1);
                    self.coord[0] *= -1;
                },
                _ => panic!("invalid rotation {}", by),
            }
        }
    }

    impl From<(usize, usize)> for Point<2> {
        fn from(tpl: (usize, usize)) -> Self {
            Self { coord: [tpl.0 as i32, tpl.1 as i32] }
        }
    }

    impl From<(i32, i32)> for Point<2> {
        fn from(tpl: (i32, i32)) -> Self {
            Self { coord: [tpl.0, tpl.1] }
        }
    }
}

fn gcd(a: i32, b: i32) -> i32 {
    let mut x = a;
    let mut y = b;
    while y != 0 {
        (x, y) = (y, x % y);
    }
    x
}
