use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Sub};

use itertools::{Itertools, iproduct};

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
    pub const NUM_NEIGHBORS: usize = 3usize.pow(N as u32) - 1;

    pub fn new(coord: [i32; N]) -> Self {
        Point { coord }
    }

    pub fn from_padded(slice: &[i32]) -> Self {
        let mut coord = [0; N];
        let bound = N.min(slice.len());
        coord[..bound].clone_from_slice(&slice[..bound]);
        Self { coord }
    }

    #[inline(always)]
    pub fn x(&self) -> i32 {
        self.coord[0]
    }

    #[inline(always)]
    pub fn y(&self) -> i32 {
        self.coord[1]
    }

    #[inline(always)]
    pub fn z(&self) -> i32 {
        self.coord[2]
    }

    #[inline(always)]
    pub fn x_mut(&mut self) -> &mut i32 {
        &mut self.coord[0]
    }

    #[inline(always)]
    pub fn y_mut(&mut self) -> &mut i32 {
        &mut self.coord[1]
    }

    #[inline(always)]
    pub fn z_mut(&mut self) -> &mut i32 {
        &mut self.coord[2]
    }

    pub fn direct_neighbors(&self) -> Vec<Self> {
        (0..N)
            .flat_map(|i| {
                [-1, 1].into_iter()
                    .map(move |offset| {
                        let mut coord = self.coord;
                        coord[i] += offset;
                        Point { coord }
                    })
            })
            .collect()
    }

    pub fn neighbors(&self) -> Vec<Self> {
        iproduct!(
            (0..N).powerset(),
            (0..N).powerset()
        )
            .map(Point::<N>::difference)
            .unique()
            .filter(|(pos, neg)| pos != neg)
            .map(|(pos, neg)| {
                let mut coord = self.coord;
                for i in pos {
                    coord[i] += 1;
                }
                for i in neg {
                    coord[i] -= 1;
                }
                Point { coord }
            })
            .collect()
    }

    fn difference<T>((pos, neg): (Vec<T>, Vec<T>)) -> (Vec<T>, Vec<T>)
        where T: Eq + Clone
    {
        let posb = &pos.clone();
        (pos, neg.iter().cloned().filter(|x| !posb.contains(x)).collect())
    }


    pub fn normalized(&self) -> Self {
        if self.coord.iter().filter(|&&n| n != 0).count() == 1 {
            let mut coord = [0i32; N];
            #[allow(clippy::needless_range_loop)]
            for i in 0..N {
                coord[i] = coord[i].signum();
            }
            Self { coord }
        } else {
            let d = self.coord.iter().cloned().reduce(gcd).unwrap();
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

impl<const N: usize> From<[i32; N]> for Point<N> {
    fn from(coord: [i32; N]) -> Self {
        Point { coord }
    }
}

// Cannot implement generically for T: TryInto<[i32; N]>,
// because of https://github.com/rust-lang/rust/issues/50133.
impl<const N: usize> TryFrom<Vec<i32>> for Point<N> {
    type Error = <Vec<i32> as TryInto<[i32; N]>>::Error;
    fn try_from(value: Vec<i32>) -> Result<Self, Self::Error> {
        let coord: [_; N] = value.try_into()?;
        Ok(Point { coord })
    }
}

impl<const N: usize> Add for Point<N> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let mut coord = self.coord;
        #[allow(clippy::needless_range_loop)]
        for i in 0..N {
            coord[i] += rhs.coord[i];
        }
        Self::Output { coord }
    }
}

impl<const N: usize> Add for &Point<N> {
    type Output = Point<N>;
    fn add(self, rhs: Self) -> Self::Output {
        let mut coord = self.coord;
        #[allow(clippy::needless_range_loop)]
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
        #[allow(clippy::needless_range_loop)]
        for i in 0..N {
            coord[i] -= rhs.coord[i];
        }
        Self::Output { coord }
    }
}

impl<const N: usize> Sub for &Point<N> {
    type Output = Point<N>;
    fn sub(self, rhs: Self) -> Self::Output {
        let mut coord = self.coord;
        #[allow(clippy::needless_range_loop)]
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

mod point3 {
    use super::Point;

    impl Point<3> {
        pub fn rotate_right_3(&mut self, axis: usize, by: i32) {
            self.rotate_left_3(axis, 360 - by);
        }

        pub fn rotate_left_3(&mut self, axis: usize, by: i32) {
            assert!(axis < 3, "invalid axis");
            let axes: Vec<_> = (0..3).cycle().skip(axis + 1).take(2).collect();
            // let axes = [[1, 2], [2, 0], [0, 1]][axis];
            match by % 360 {
                0 => (),
                180 => {
                    self.coord[axes[0]] *= -1;
                    self.coord[axes[1]] *= -1;
                },
                90 => {
                    self.coord.swap(axes[0], axes[1]);
                    self.coord[axes[1]] *= -1;
                },
                270 => {
                    self.coord.swap(axes[0], axes[1]);
                    self.coord[axes[0]] *= -1;
                },
                _ => panic!("invalid rotation {}", by),
            }
        }
    }
}

fn gcd(a: i32, b: i32) -> i32 {
    let mut x = a;
    let mut y = b;
    while y != 0 {
        std::mem::swap(&mut y, &mut x);
        y = x % y;
    }
    x
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_neighbors_2() {
        let pt = Point::new([0, 0]);
        let pts: Vec<_> = pt.neighbors();
        println!("{pts:?}");
        assert_eq!(pts.len(), 8);
        let pts_hashed: HashSet<_> = pts.iter().cloned().collect();
        assert_eq!(pts.len(), pts_hashed.len());
    }

    #[test]
    fn test_neighbors_3() {
        let pt = Point::new([0, 0, 0]);
        let pts: Vec<_> = pt.neighbors();
        println!("{pts:?}");
        assert_eq!(pts.len(), 26);
        let pts_hashed: HashSet<_> = pts.iter().cloned().collect();
        assert_eq!(pts.len(), pts_hashed.len());
    }

    #[test]
    fn test_rotate_3_left() {
        let mut pt = Point::new([1, 2, 3]);
        pt.rotate_left_3(0, 90); // rotate around x axis
        assert_eq!(pt, Point::new([1, 3, -2]));
        pt.rotate_left_3(1, 90); // rotate around y axis
        assert_eq!(pt, Point::new([2, 3, 1]));
        pt.rotate_left_3(2, 90); // rotate around z axis
        assert_eq!(pt, Point::new([3, -2, 1]));
    }
}
