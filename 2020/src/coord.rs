
use std::{fmt::{Display, Formatter}, unimplemented};
use std::ops;

use impl_ops::*;
use itertools::iproduct;

pub trait Coordinate: Sized + PartialEq {
    const ZERO: Self;

    fn neighbors(&self) -> Vec<Self>;
    fn normalized(&self) -> Self;
    fn manhattan(&self) -> i32;
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point2D(i32, i32);

impl Point2D {

    pub const NW: Self = Self(-1, -1);
    pub const N:  Self = Self( 0, -1);
    pub const NE: Self = Self( 1, -1);
    pub const W:  Self = Self(-1,  0);
    pub const E:  Self = Self( 1,  0);
    pub const SW: Self = Self(-1,  1);
    pub const S:  Self = Self( 0,  1);
    pub const SE: Self = Self( 1,  1);

    pub fn rotate_right(&mut self, by: i32) {
        self.rotate_left(360 - by);
    }

    pub fn rotate_left(&mut self, by: i32) {
        match by % 360 {
            0 => (),
            180 => {
                self.0 *= -1;
                self.1 *= -1;
            },
            90 => {
                std::mem::swap(&mut self.0, &mut self.1);
                self.1 *= -1;
            },
            270 => {
                std::mem::swap(&mut self.0, &mut self.1);
                self.0 *= -1;
            },
            _ => panic!("invalid rotation {}", by),
        }
    }

}

impl Coordinate for Point2D {
    const ZERO: Self = Self(0, 0);

    fn neighbors(&self) -> Vec<Self> {
        vec![
            Self(self.0 - 1, self.1 - 1),
            Self(self.0    , self.1 - 1),
            Self(self.0 + 1, self.1 - 1),
            Self(self.0 - 1, self.1    ),
            Self(self.0 + 1, self.1    ),
            Self(self.0 - 1, self.1 + 1),
            Self(self.0    , self.1 + 1),
            Self(self.0 + 1, self.1 + 1),
        ]
    }

    fn normalized(&self) -> Self {
        if self.0 == 0 || self.1 == 0 {
            Self(self.0.signum(), self.1.signum())
        } else {
            let d = gcd(self.0.abs(), self.1.abs());
            self / d
        }
    }

    fn manhattan(&self) -> i32 {
        self.0.abs() + self.1.abs()
    }
}

impl Default for Point2D {
    fn default() -> Self {
        Self(0, 0)
    }
}

impl Display for Point2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl From<(usize, usize)> for Point2D {
    fn from(tpl: (usize, usize)) -> Self {
        Self(tpl.0 as i32, tpl.1 as i32)
    }
}

impl From<(i32, i32)> for Point2D {
    fn from(tpl: (i32, i32)) -> Self {
        Self(tpl.0, tpl.1)
    }
}

impl_op!(+ |a: &Point2D, b:  i32| -> Point2D { Point2D(a.0 + b, a.1 + b) });
impl_op!(+ |a:  Point2D, b:  i32| -> Point2D { Point2D(a.0 + b, a.1 + b) });
impl_op!(+ |a: &Point2D, b: &i32| -> Point2D { Point2D(a.0 + b, a.1 + b) });
impl_op!(+ |a:  Point2D, b: &i32| -> Point2D { Point2D(a.0 + b, a.1 + b) });
impl_op!(- |a: &Point2D, b:  i32| -> Point2D { Point2D(a.0 - b, a.1 - b) });
impl_op!(- |a:  Point2D, b:  i32| -> Point2D { Point2D(a.0 - b, a.1 - b) });
impl_op!(- |a: &Point2D, b: &i32| -> Point2D { Point2D(a.0 - b, a.1 - b) });
impl_op!(- |a:  Point2D, b: &i32| -> Point2D { Point2D(a.0 - b, a.1 - b) });
impl_op!(* |a: &Point2D, b:  i32| -> Point2D { Point2D(a.0 * b, a.1 * b) });
impl_op!(* |a:  Point2D, b:  i32| -> Point2D { Point2D(a.0 * b, a.1 * b) });
impl_op!(* |a: &Point2D, b: &i32| -> Point2D { Point2D(a.0 * b, a.1 * b) });
impl_op!(* |a:  Point2D, b: &i32| -> Point2D { Point2D(a.0 * b, a.1 * b) });
impl_op!(/ |a: &Point2D, b:  i32| -> Point2D { Point2D(a.0 / b, a.1 / b) });
impl_op!(/ |a:  Point2D, b:  i32| -> Point2D { Point2D(a.0 / b, a.1 / b) });
impl_op!(/ |a: &Point2D, b: &i32| -> Point2D { Point2D(a.0 / b, a.1 / b) });
impl_op!(/ |a:  Point2D, b: &i32| -> Point2D { Point2D(a.0 / b, a.1 / b) });

impl_op!(+ |a: &Point2D, b: &Point2D| -> Point2D { Point2D(a.0 + b.0, a.1 + b.1) });
impl_op!(+ |a: &Point2D, b:  Point2D| -> Point2D { Point2D(a.0 + b.0, a.1 + b.1) });
impl_op!(+ |a:  Point2D, b: &Point2D| -> Point2D { Point2D(a.0 + b.0, a.1 + b.1) });
impl_op!(+ |a:  Point2D, b:  Point2D| -> Point2D { Point2D(a.0 + b.0, a.1 + b.1) });
impl_op!(- |a: &Point2D, b: &Point2D| -> Point2D { Point2D(a.0 - b.0, a.1 - b.1) });
impl_op!(- |a: &Point2D, b:  Point2D| -> Point2D { Point2D(a.0 - b.0, a.1 - b.1) });
impl_op!(- |a:  Point2D, b: &Point2D| -> Point2D { Point2D(a.0 - b.0, a.1 - b.1) });
impl_op!(- |a:  Point2D, b:  Point2D| -> Point2D { Point2D(a.0 - b.0, a.1 - b.1) });

impl_op!(+= |a: &mut Point2D, b:  Point2D| { a.0 += b.0; a.1 += b.1; });
impl_op!(+= |a: &mut Point2D, b: &Point2D| { a.0 += b.0; a.1 += b.1; });
impl_op!(-= |a: &mut Point2D, b:  Point2D| { a.0 -= b.0; a.1 -= b.1; });
impl_op!(-= |a: &mut Point2D, b: &Point2D| { a.0 -= b.0; a.1 -= b.1; });


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point3D(i32, i32, i32);

impl Point3D {
}

impl Coordinate for Point3D {
    const ZERO: Self = Self(0, 0, 0);

    fn neighbors(&self) -> Vec<Self> {
        iproduct!(
            -1..=1,
            -1..=1,
            -1..=1
        )
            .map(Self::from)
            .filter(|x| x != &Self::ZERO)
            .map(|x| self + x)
            .collect()
    }

    fn normalized(&self) -> Self {
        unimplemented!();
    }

    fn manhattan(&self) -> i32 {
        self.0.abs() + self.1.abs() + self.2.abs()
    }
}

impl Default for Point3D {
    fn default() -> Self {
        Self(0, 0, 0)
    }
}

impl Display for Point3D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl From<(i32, i32)> for Point3D {
    fn from(tpl: (i32, i32)) -> Self {
        Self(tpl.0, tpl.1, 0)
    }
}

impl From<(i32, i32, i32)> for Point3D {
    fn from(tpl: (i32, i32, i32)) -> Self {
        Self(tpl.0, tpl.1, tpl.2)
    }
}

impl From<(usize, usize)> for Point3D {
    fn from(tpl: (usize, usize)) -> Self {
        Self(tpl.0 as i32, tpl.1 as i32, 0)
    }
}

impl From<(usize, usize, usize)> for Point3D {
    fn from(tpl: (usize, usize, usize)) -> Self {
        Self(tpl.0 as i32, tpl.1 as i32, tpl.2 as i32)
    }
}

impl_op!(+ |a: &Point3D, b:  Point3D| -> Point3D { Point3D(a.0 + b.0, a.1 + b.1, a.2 + b.2) });
impl_op!(+ |a:  Point3D, b:  Point3D| -> Point3D { Point3D(a.0 + b.0, a.1 + b.1, a.2 + b.2) });
impl_op!(+ |a: &Point3D, b: &Point3D| -> Point3D { Point3D(a.0 + b.0, a.1 + b.1, a.2 + b.2) });
impl_op!(+ |a:  Point3D, b: &Point3D| -> Point3D { Point3D(a.0 + b.0, a.1 + b.1, a.2 + b.2) });


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point4D(i32, i32, i32, i32);

impl Point4D {
}

impl Coordinate for Point4D {
    const ZERO: Self = Self(0, 0, 0, 0);

    fn neighbors(&self) -> Vec<Self> {
        iproduct!(
            -1..=1,
            -1..=1,
            -1..=1,
            -1..=1
        )
            .map(Self::from)
            .filter(|x| x != &Self::ZERO)
            .map(|x| self + x)
            .collect()
    }

    fn normalized(&self) -> Self {
        unimplemented!();
    }

    fn manhattan(&self) -> i32 {
        self.0.abs() + self.1.abs() + self.2.abs() + self.3.abs()
    }
}

impl Default for Point4D {
    fn default() -> Self {
        Self(0, 0, 0, 0)
    }
}

impl Display for Point4D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.0, self.1)
    }
}

impl From<(i32, i32)> for Point4D {
    fn from(tpl: (i32, i32)) -> Self {
        Self(tpl.0, tpl.1, 0, 0)
    }
}

impl From<(i32, i32, i32)> for Point4D {
    fn from(tpl: (i32, i32, i32)) -> Self {
        Self(tpl.0, tpl.1, tpl.2, 0)
    }
}

impl From<(i32, i32, i32, i32)> for Point4D {
    fn from(tpl: (i32, i32, i32, i32)) -> Self {
        Self(tpl.0, tpl.1, tpl.2, tpl.3)
    }
}

impl From<(usize, usize)> for Point4D {
    fn from(tpl: (usize, usize)) -> Self {
        Self(tpl.0 as i32, tpl.1 as i32, 0, 0)
    }
}

impl From<(usize, usize, usize)> for Point4D {
    fn from(tpl: (usize, usize, usize)) -> Self {
        Self(tpl.0 as i32, tpl.1 as i32, tpl.2 as i32, 0)
    }
}

impl From<(usize, usize, usize, usize)> for Point4D {
    fn from(tpl: (usize, usize, usize, usize)) -> Self {
        Self(tpl.0 as i32, tpl.1 as i32, tpl.2 as i32, tpl.3 as i32)
    }
}

impl_op!(+ |a: &Point4D, b:  Point4D| -> Point4D { Point4D(a.0 + b.0, a.1 + b.1, a.2 + b.2, a.3 + b.3) });
impl_op!(+ |a:  Point4D, b:  Point4D| -> Point4D { Point4D(a.0 + b.0, a.1 + b.1, a.2 + b.2, a.3 + b.3) });
impl_op!(+ |a: &Point4D, b: &Point4D| -> Point4D { Point4D(a.0 + b.0, a.1 + b.1, a.2 + b.2, a.3 + b.3) });
impl_op!(+ |a:  Point4D, b: &Point4D| -> Point4D { Point4D(a.0 + b.0, a.1 + b.1, a.2 + b.2, a.3 + b.3) });


fn gcd(a: i32, b: i32) -> i32 {
    if a == b {
        a
    } else if a > b {
        gcd(a - b, b)
    } else {
        gcd(a, b - a)
    }
}
