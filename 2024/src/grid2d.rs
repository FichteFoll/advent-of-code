use std::fmt::{Display, Error, Formatter};
use std::iter::FromIterator;
use std::ops::Range;

use itertools::iproduct;

use crate::point::Point;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Grid2D<T> {
    pub grid: Vec<Vec<T>>,
    pub size: Size,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Size(pub usize, pub usize);

impl<T> Grid2D<T> {
    pub fn get(&self, pt: &Point<2>) -> Option<&T> {
        self.grid
            .get(pt.y() as usize)
            .and_then(|row| row.get(pt.x() as usize))
    }

    pub fn get_mut(&mut self, pt: &Point<2>) -> Option<&mut T> {
        self.grid
            .get_mut(pt.y() as usize)
            .and_then(|row| row.get_mut(pt.x() as usize))
    }

    pub fn set(&mut self, pt: &Point<2>, value: T) {
        self.grid[pt.y() as usize][pt.x() as usize] = value;
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.grid.iter().flat_map(|row| row.iter())
    }

    pub fn iter_enumerate(&self) -> impl Iterator<Item = (Point<2>, &T)> {
        self.grid.iter().enumerate().flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(move |(x, cell)| (Point([x as i32, y as i32]), cell))
        })
    }

    pub fn iter_enumerate_mut(&mut self) -> impl Iterator<Item = (Point<2>, &mut T)> {
        self.grid.iter_mut().enumerate().flat_map(|(y, row)| {
            row.iter_mut()
                .enumerate()
                .map(move |(x, cell)| (Point([x as i32, y as i32]), cell))
        })
    }

    pub fn position<P>(&self, predicate: P) -> Option<Point<2>>
    where
        P: Fn(&T) -> bool,
    {
        self.iter_enumerate()
            .find_map(|(pt, c)| predicate(c).then_some(pt))
    }

    #[must_use]
    pub fn new_map<F>(&self, f: F) -> Self
    where
        F: Fn(&T) -> T + Copy,
    {
        self.grid.iter().map(|row| row.iter().map(f)).collect()
    }

    #[must_use]
    pub fn new_map_enumerate<F>(&self, f: F) -> Self
    where
        F: Fn(Point<2>, &T) -> T,
    {
        self.grid
            .iter()
            .enumerate()
            .map(|(y, row)| {
                row.iter()
                    .enumerate()
                    .map(|(x, cell)| f(Point([x as i32, y as i32]), cell))
                    .collect::<Vec<_>>()
            })
            .collect()
    }
}

impl Size {
    #[inline(always)]
    fn x_range(&self) -> Range<i32> {
        0..self.0 as i32
    }
    #[inline(always)]
    fn y_range(&self) -> Range<i32> {
        0..self.1 as i32
    }

    pub fn contains(&self, pt: &Point<2>) -> bool {
        self.x_range().contains(&pt.x()) && self.y_range().contains(&pt.y())
    }

    pub fn contained(&self, pts: Vec<Point<2>>) -> impl Iterator<Item = Point<2>> + '_ {
        pts.into_iter().filter(|pt| self.contains(pt))
    }

    pub fn indices(&self) -> impl Iterator<Item = Point<2>> {
        iproduct!(self.x_range(), self.y_range()).map(Point::<2>::from)
    }
}

impl<T> IntoIterator for Grid2D<T> {
    type Item = T;
    type IntoIter = impl Iterator<Item = T>;

    fn into_iter(self) -> Self::IntoIter {
        self.grid.into_iter().flat_map(|row| row.into_iter())
    }
}

impl<L, C> FromIterator<L> for Grid2D<C>
where
    L: IntoIterator<Item = C>,
{
    fn from_iter<I: IntoIterator<Item = L>>(iter: I) -> Self {
        // Assumes all rows have the same length
        let grid: Vec<Vec<C>> = iter
            .into_iter()
            .map(|nested_iter| nested_iter.into_iter().collect())
            .collect();
        let size = Size(grid.first().map(Vec::len).unwrap_or(0), grid.len());
        Grid2D { grid, size }
    }
}

impl<T> Display for Grid2D<T>
where
    T: Display,
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::collections::HashSet;

    #[test]
    fn test_contained_neighbors_grid() {
        let grid: Grid2D<char> = ["abcd", "abcd"].into_iter().map(str::chars).collect();
        let pt = Point([2, 0]);
        let pts: Vec<_> = grid.size.contained(pt.neighbors()).collect();
        let pts_hashed: HashSet<_> = pts.iter().cloned().collect();
        assert_eq!(pts.len(), pts_hashed.len());
        assert_eq!(pts.len(), 5);
    }

    #[test]
    fn test_contained_neighbors_size() {
        let grid: Grid2D<char> = ["abcd", "abcd"].into_iter().map(str::chars).collect();
        let pt = Point([2, 0]);
        let pts: Vec<_> = grid.size.contained(pt.neighbors()).collect();
        let pts_hashed: HashSet<_> = pts.iter().cloned().collect();
        assert_eq!(pts.len(), pts_hashed.len());
        assert_eq!(pts.len(), 5);
    }

    #[test]
    fn test_position() {
        let grid: Grid2D<char> = ["abcd", "abcd"].into_iter().map(str::chars).collect();
        let pt = grid.position(|c| *c == 'c');
        assert_eq!(pt, Some(Point([2, 0])));
    }
}
