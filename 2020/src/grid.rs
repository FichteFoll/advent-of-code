use std::{fmt::{Display, Error, Formatter}, iter::FromIterator, unimplemented};

use itertools::iproduct;
use std::ops;

use impl_ops::*;

type Size = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid<T> {
    grid: Vec<Vec<T>>,
    size: Size,
}

impl<T> Grid<T> {
    pub fn get(&self, pt: &Point) -> Option<&T> {
        self.grid.get(pt.y as usize).map(|row| row.get(pt.x as usize)).flatten()
    }

    pub fn contains(&self, pt: &Point) -> bool {
        (0..self.size.0 as i32).contains(&pt.x)
            && (0..self.size.1 as i32).contains(&pt.y)
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.grid.iter()
            .flat_map(|row| row.iter())
    }

    pub fn iter_enumerate(&self) -> impl Iterator<Item=(Point, &T)> {
        self.grid.iter().enumerate()
            .flat_map(|(y, row)| {
                row.iter().enumerate()
                   .map(|(x, cell)| (Point { x: x as i32, y: y as i32 }, cell))
                   .collect::<Vec<_>>()
            })
    }

    pub fn saturated_neighbors(&self, pt: &Point) -> impl Iterator<Item=Point> {
        pt.saturated_neighbors(self.size.0 - 1, self.size.1 - 1)
    }

    pub fn neighbor_values(&self, pt: &Point) -> Vec<&T> {
        pt.neighbors().iter()
            .filter_map(|x| self.get(&x))
            .collect()
    }

    pub fn indices(&self) -> impl Iterator<Item=Point> {
        iproduct!(
            0..self.size.0,
            0..self.size.1
        )
            .map(|x| Point::from(x))
    }

    pub fn new_map<F>(&self, f: F) -> Self
        where F: Fn(&T) -> T
    {
        self.grid.iter()
            .map(|row| {
                row.iter()
                   .map(|cell| f(cell))
                   .collect::<Vec<_>>()
            })
            .collect()
    }

    pub fn new_map_enumerate<F>(&self, f: F) -> Self
        where F: Fn(&Point, &T) -> T
    {
        self.grid.iter().enumerate()
            .map(|(y, row)| {
                row.iter().enumerate()
                   .map(|(x, cell)| f(&Point { x: x as i32, y: y as i32 }, cell))
                   .collect::<Vec<_>>()
            })
            .collect()
    }
}

// impl<T> IntoIterator for Grid<T> {
//     type Item = T;
//     type IntoIter = impl Iterator<Item=T>;
//     fn into_iter(self) -> Self::IntoIter {
//         self.grid.into_iter()
//             .flat_map(|row| row.into_iter())
//     }
// }

impl<A, T> FromIterator<A> for Grid<T>
    where A: IntoIterator<Item=T>
{
    fn from_iter<I: IntoIterator<Item=A>>(iter: I) -> Self {
        // Assumes all rows have the same length
        let grid: Vec<Vec<T>> = iter.into_iter()
                .map(|nested_iter| nested_iter.into_iter().collect())
                .collect();
        let size = (grid.first().map(Vec::len).unwrap_or(0), grid.len());
        Grid { grid, size }
    }
}

impl<T> Display for Grid<T>
    where T: Display
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for row in self.grid.iter() {
            for cell in row {
                cell.fmt(f)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub const NW: Self = Self { x: -1, y: -1 };
    pub const N:  Self = Self { x:  0, y: -1 };
    pub const NE: Self = Self { x:  1, y: -1 };
    pub const W:  Self = Self { x: -1, y:  0 };
    pub const E:  Self = Self { x:  1, y:  0 };
    pub const SW: Self = Self { x: -1, y:  1 };
    pub const S:  Self = Self { x:  0, y:  1 };
    pub const SE: Self = Self { x:  1, y:  1 };

    pub const ZERO: Self = Self { x: 0, y: 0 };


    pub fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }

    pub fn neighbors(&self) -> Vec<Point> {
        vec![
            Point { x: self.x - 1, y: self.y - 1 },
            Point { x: self.x    , y: self.y - 1 },
            Point { x: self.x + 1, y: self.y - 1 },
            Point { x: self.x - 1, y: self.y     },
            Point { x: self.x + 1, y: self.y     },
            Point { x: self.x - 1, y: self.y + 1 },
            Point { x: self.x    , y: self.y + 1 },
            Point { x: self.x + 1, y: self.y + 1 },
        ]
    }

    pub fn saturated_neighbors(&self, max_x: usize, max_y: usize) -> impl Iterator<Item=Point> {
        let exclude = self.clone();
        iproduct!(
            0.max(self.x - 1)..=(max_x as i32).min(self.x + 1),
            0.max(self.y - 1)..=(max_y as i32).min(self.y + 1)
        )
            .filter(move |(x, y)| *x != exclude.x || *y != exclude.y)
            .map(|tpl| Point::from(tpl))
    }

    pub fn normalized(&self) -> Point {
        if self.x == 0 || self.y == 0 {
            Point { x: self.x.signum(), y: self.y.signum()}
        } else {
            let d = gcd(self.x.abs(), self.y.abs());
            self / d
        }
    }

    pub fn rotate_right(&mut self, by: i32) {
        self.rotate_left(360 - by);
    }

    pub fn rotate_left(&mut self, by: i32) {
        match by % 360 {
            0 => (),
            180 => {
                self.x *= -1;
                self.y *= -1;
            },
            90 => {
                std::mem::swap(&mut self.x, &mut self.y);
                self.y *= -1;
            },
            270 => {
                std::mem::swap(&mut self.x, &mut self.y);
                self.x *= -1;
            },
            _ => panic!("invalid rotation {}", by),
        }
    }

    pub fn manhattan(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }

}

impl Default for Point {
    fn default() -> Self {
        Point { x: 0, y: 0}
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl From<(usize, usize)> for Point {
    fn from(tpl: (usize, usize)) -> Self {
        Self { x: tpl.0 as i32, y: tpl.1 as i32 }
    }
}

impl From<(i32, i32)> for Point {
    fn from(tpl: (i32, i32)) -> Self {
        Self { x: tpl.0, y: tpl.1 }
    }
}

impl_op!(+ |a: &Point, b:  i32| -> Point { Point { x: a.x + b, y: a.y + b } });
impl_op!(+ |a:  Point, b:  i32| -> Point { Point { x: a.x + b, y: a.y + b } });
impl_op!(+ |a: &Point, b: &i32| -> Point { Point { x: a.x + b, y: a.y + b } });
impl_op!(+ |a:  Point, b: &i32| -> Point { Point { x: a.x + b, y: a.y + b } });
impl_op!(- |a: &Point, b:  i32| -> Point { Point { x: a.x - b, y: a.y - b } });
impl_op!(- |a:  Point, b:  i32| -> Point { Point { x: a.x - b, y: a.y - b } });
impl_op!(- |a: &Point, b: &i32| -> Point { Point { x: a.x - b, y: a.y - b } });
impl_op!(- |a:  Point, b: &i32| -> Point { Point { x: a.x - b, y: a.y - b } });
impl_op!(* |a: &Point, b:  i32| -> Point { Point { x: a.x * b, y: a.y * b } });
impl_op!(* |a:  Point, b:  i32| -> Point { Point { x: a.x * b, y: a.y * b } });
impl_op!(* |a: &Point, b: &i32| -> Point { Point { x: a.x * b, y: a.y * b } });
impl_op!(* |a:  Point, b: &i32| -> Point { Point { x: a.x * b, y: a.y * b } });
impl_op!(/ |a: &Point, b:  i32| -> Point { Point { x: a.x / b, y: a.y / b } });
impl_op!(/ |a:  Point, b:  i32| -> Point { Point { x: a.x / b, y: a.y / b } });
impl_op!(/ |a: &Point, b: &i32| -> Point { Point { x: a.x / b, y: a.y / b } });
impl_op!(/ |a:  Point, b: &i32| -> Point { Point { x: a.x / b, y: a.y / b } });

impl_op!(+ |a: &Point, b: &Point| -> Point { Point { x: a.x + b.x, y: a.y + b.y } });
impl_op!(+ |a: &Point, b:  Point| -> Point { Point { x: a.x + b.x, y: a.y + b.y } });
impl_op!(+ |a:  Point, b: &Point| -> Point { Point { x: a.x + b.x, y: a.y + b.y } });
impl_op!(+ |a:  Point, b:  Point| -> Point { Point { x: a.x + b.x, y: a.y + b.y } });
impl_op!(- |a: &Point, b: &Point| -> Point { Point { x: a.x - b.x, y: a.y - b.y } });
impl_op!(- |a: &Point, b:  Point| -> Point { Point { x: a.x - b.x, y: a.y - b.y } });
impl_op!(- |a:  Point, b: &Point| -> Point { Point { x: a.x - b.x, y: a.y - b.y } });
impl_op!(- |a:  Point, b:  Point| -> Point { Point { x: a.x - b.x, y: a.y - b.y } });

impl_op!(+= |a: &mut Point, b:  Point| { a.x += b.x; a.y += b.y; });
impl_op!(+= |a: &mut Point, b: &Point| { a.x += b.x; a.y += b.y; });
impl_op!(-= |a: &mut Point, b:  Point| { a.x -= b.x; a.y -= b.y; });
impl_op!(-= |a: &mut Point, b: &Point| { a.x -= b.x; a.y -= b.y; });


fn gcd(a: i32, b: i32) -> i32 {
    if a == b {
        a
    } else if a > b {
        gcd(a - b, b)
    } else {
        gcd(a, b - a)
    }
}
