use std::{fmt::{Display, Error, Formatter}, iter::FromIterator};

use itertools::iproduct;
// use impl_ops::impl_op;


type Size = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Grid<T> {
    grid: Vec<Vec<T>>,
    dimensions: Size,
}

impl<T> Grid<T> {
    pub fn get(&self, pt: &Point) -> &T {
        &self.grid[pt.y][pt.x]
    }

    pub fn iter(&self) -> impl Iterator<Item=&T> {
        (0..self.dimensions.0)
            .flat_map(move |y| (0..self.dimensions.1)
                .map(move |x| Point { x, y })
                .map(move |p| self.get(&p))
            )
    }

    pub fn saturated_neighbors(&self, pt: &Point) -> impl Iterator<Item=Point> {
        pt.saturated_neighbors(self.dimensions.0 - 1, self.dimensions.1 - 1)
    }

    pub fn neighbor_values(&self, pt: &Point) -> Vec<&T> {
        self.saturated_neighbors(pt)
            .map(|x| self.get(&x))
            .collect()
    }

    pub fn indices(&self) -> impl Iterator<Item=Point> {
        iproduct!(
            0..self.dimensions.0,
            0..self.dimensions.1
        )
            .map(|x| Point::from(x))
    }

    pub fn new_map<F>(&self, f: F) -> Self
        where F: Fn(&T) -> T
    {
        (0..self.dimensions.0)
            .map(|y| (0..self.dimensions.1)
                .map(move |x| Point { x, y })
                .map(|p| f(self.get(&p)))
            )
            .collect()
    }

    pub fn new_map_enumerate<F>(&self, f: F) -> Self
        where F: Fn(&Point, &T) -> T
    {
        self.grid.iter().enumerate()
            .map(|(y, row)| {
                row.iter().enumerate()
                   .map(|(x, cell)| f(&Point { x, y }, cell))
                   .collect::<Vec<T>>()
            })
            .collect()
        // (0..self.dimensions.0)
        //     .map(|y| (0..self.dimensions.1)
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
        let dimensions = (grid.first().map(Vec::len).unwrap_or(0), grid.len());
        Grid {
            grid,
            dimensions,
        }
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
    pub x: usize,
    pub y: usize,
}

impl Point {
    pub fn saturated_neighbors(&self, max_x: usize, max_y: usize) -> impl Iterator<Item=Point> {
        let exclude = self.clone();
        iproduct!(
            self.x.saturating_sub(1)..=max_x.min(self.x + 1),
            self.y.saturating_sub(1)..=max_y.min(self.y + 1)
        )
            .filter(move |(x, y)| !(*x == exclude.x && *y == exclude.y))
            .map(|tpl| Point::from(tpl))
    }
}

impl From<(usize, usize)> for Point {
    fn from(tpl: (usize, usize)) -> Self {
        Self { x: tpl.0, y: tpl.1 }
    }
}

// TODO impl_op!
