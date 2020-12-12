use std::{fmt::{Display, Error, Formatter}, iter::FromIterator};

use itertools::iproduct;
// use impl_ops::impl_op;

type Size = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid<T> {
    grid: Vec<Vec<T>>,
    size: Size,
}

impl<T> Grid<T> {
    pub fn get(&self, pt: &Point) -> &T {
        &self.grid[pt.y as usize][pt.x as usize]
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.grid.iter()
            .flat_map(|row| row.iter())
    }

    pub fn saturated_neighbors(&self, pt: &Point) -> impl Iterator<Item=Point> {
        pt.saturated_neighbors(self.size.0 - 1, self.size.1 - 1)
    }

    pub fn neighbor_values(&self, pt: &Point) -> Vec<&T> {
        self.saturated_neighbors(pt)
            .map(|x| self.get(&x))
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
                   .collect::<Vec<T>>()
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
                   .collect::<Vec<T>>()
            })
            .collect()
        // (0..self.size.0)
        //     .map(|y| (0..self.size.1)
        //         .map(move |x| Point { x, y })
        //         .map(|p| f(&p, self.get(&p)))
        //     )
        //     .collect()
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

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub fn saturated_neighbors(&self, max_x: usize, max_y: usize) -> impl Iterator<Item=Point> {
        let exclude = self.clone();
        iproduct!(
            0.max(self.x - 1)..=(max_x as i32).min(self.x + 1),
            0.max(self.y - 1)..=(max_y as i32).min(self.y + 1)
        )
            .filter(move |(x, y)| *x != exclude.x || *y != exclude.y)
            .map(|tpl| Point::from(tpl))
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

// TODO impl_op!
