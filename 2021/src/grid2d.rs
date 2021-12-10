use std::fmt::{Display, Error, Formatter};
use std::iter::FromIterator;

use itertools::iproduct;

use crate::coord::Point;

pub type Size = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid2D<T> {
    grid: Vec<Vec<T>>,
    pub size: Size,
}

impl<T> Grid2D<T> {
    pub fn get(&self, pt: &Point<2>) -> Option<&T> {
        self.grid.get(pt.y() as usize).map(|row| row.get(pt.x() as usize)).flatten()
    }

    pub fn contains(&self, pt: &Point<2>) -> bool {
        (0..self.size.0 as i32).contains(&pt.x())
            && (0..self.size.1 as i32).contains(&pt.y())
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.grid.iter()
            .flat_map(|row| row.iter())
    }

    pub fn iter_enumerate(&self) -> impl Iterator<Item=(Point<2>, &T)> {
        self.grid.iter().enumerate()
            .flat_map(|(y, row)| {
                row.iter().enumerate()
                   .map(|(x, cell)| (Point::<2>::new([x as i32, y as i32]), cell))
                   .collect::<Vec<_>>()
            })
    }

    pub fn contained(&self, pts: Vec<Point<2>>) -> impl Iterator<Item=Point<2>> + '_ {
        pts
            .into_iter()
            .filter(|pt| (0..self.size.0 as i32).contains(&pt.x())
                && (0..self.size.1 as i32).contains(&pt.y()))
    }

    pub fn neighbor_values(&self, pt: &Point<2>) -> Vec<&T> {
        pt.neighbors().iter()
            .filter_map(|x| self.get(x))
            .collect()
    }

    pub fn indices(&self) -> impl Iterator<Item=Point<2>> {
        iproduct!(
            0..self.size.0,
            0..self.size.1
        )
            .map(Point::<2>::from)
    }

    pub fn new_map<F>(&self, f: F) -> Self
        where F: Fn(&T) -> T + Copy
    {
        self.grid.iter()
            .map(|row| row.iter().map(f))
            .collect()
    }

    pub fn new_map_enumerate<F>(&self, f: F) -> Self
        where F: Fn(&Point<2>, &T) -> T
    {
        self.grid.iter().enumerate()
            .map(|(y, row)| {
                row.iter().enumerate()
                   .map(|(x, cell)| f(&Point::<2>::new([x as i32, y as i32]), cell))
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

impl<A, T> FromIterator<A> for Grid2D<T>
    where A: IntoIterator<Item=T>
{
    fn from_iter<I: IntoIterator<Item=A>>(iter: I) -> Self {
        // Assumes all rows have the same length
        let grid: Vec<Vec<T>> = iter.into_iter()
                .map(|nested_iter| nested_iter.into_iter().collect())
                .collect();
        let size = (grid.first().map(Vec::len).unwrap_or(0), grid.len());
        Grid2D { grid, size }
    }
}

impl<T> Display for Grid2D<T>
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
