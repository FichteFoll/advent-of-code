use std::{collections::HashMap, fmt::{Debug, Display, Error, Formatter}, hash::Hash, iter::FromIterator};

use crate::coord::Coordinate;

#[derive(Clone, Debug)]
pub struct HashGrid<K, V>
where K: Hash
{
    grid: HashMap<K, V>,
}

impl<K, V> HashGrid<K, V>
where K: Hash
{
    pub fn iter(&self) -> impl Iterator<Item=(&K, &V)> {
        self.grid.iter()
    }

    pub fn keys(&self) -> impl Iterator<Item=&K> {
        self.grid.keys()
    }

    pub fn values(&self) -> impl Iterator<Item=&V> {
        self.grid.values()
    }
}

impl<K, V> HashGrid<K, V>
where K: Hash + Eq + Clone, V: Clone
{
    pub fn with_keys(keys: &[K], default: V) -> Self {
        keys.iter()
            .map(|key| (key.clone(), default.clone()))
            .collect()
    }
}

impl<K, V> HashGrid<K, V>
where K: Hash + Eq
{
    pub fn get(&self, pt: &K) -> Option<&V> {
        self.grid.get(pt)
    }
}

impl<K, V> HashGrid<K, V>
where K: Hash + Clone + Eq
{
    pub fn new_map<F, U>(&self, f: F) -> HashGrid<K, U>
        where F: Fn(&V) -> U
    {
        self.grid.iter()
            .map(|(k, cell)| (k.clone(), f(cell)))
            .collect()
    }

    pub fn new_map_with_key<F, U>(&self, f: F) -> HashGrid<K, U>
        where F: Fn(&K, &V) -> U
    {
        self.grid.iter()
            .map(|(k, cell)| (k.clone(), f(k, cell)))
            .collect()
    }
}

impl<K, V> HashGrid<K, V>
where K: Hash + Eq + Coordinate
{
    pub fn neighbor_values(&self, k: &K) -> Vec<&V> {
        k.neighbors().iter()
            .filter_map(|x| self.grid.get(&x))
            .collect()
    }

    pub fn count_neighbors(&self, k: &K) -> usize {
        k.neighbors().iter()
            .filter(|x| self.grid.contains_key(&x))
            .count()
    }
}

impl<K, V> FromIterator<(K, V)> for HashGrid<K, V>
where K: Hash + Eq
{
    fn from_iter<I: IntoIterator<Item=(K, V)>>(iter: I) -> Self {
        // Assumes all rows have the same length
        let grid: HashMap<K, V> = iter.into_iter().collect();
        HashGrid { grid }
    }
}

// impl<K, V> Display for HashGrid<K, V>
// where
//     K: Hash + Display,
//     V: Display,
// {
//     fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
//         write!(f, "HashGrid{{")?;
//         for (k, v) in self.grid.iter() {
//             writeln!(f, "  [{}] = {},", k, v)?;
//         }
//         write!(f, "}}")?;
//         Ok(())
//     }
// }

impl<K, V> Display for HashGrid<K, V>
where
    K: Hash + Display,
    V: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "HashGrid{{")?;
        for (k, v) in self.grid.iter() {
            writeln!(f, "  [{}] = {:?},", k, v)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}
