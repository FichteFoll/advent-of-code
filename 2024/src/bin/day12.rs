#![feature(test)]

use std::iter::successors;

use itertools::Itertools;

use aoc2024::*;
use collections::{HashMap, HashSet};
use grid2d::Grid2D;
use point::Point;

const DAY: usize = 12;

type Parsed = Grid2D<u8>;
type P = Point<2>;

main!();

fn parse_input(input: &str) -> Parsed {
    input.lines().map(str::bytes).collect()
}

fn part_1(grid: &Parsed) -> usize {
    collect_regions(grid)
        .into_values()
        .flat_map(Vec::into_iter)
        .map(price_perimeter)
        .sum()
}

fn collect_regions(grid: &Parsed) -> HashMap<u8, Vec<HashSet<P>>> {
    let mut regions_by_char: HashMap<u8, Vec<HashSet<P>>> = Default::default();
    for (pt, &c) in grid.iter_enumerate() {
        let char_regions = regions_by_char.entry(c).or_default();
        let region_is = [pt + P::N, pt + P::W]
            .iter()
            .filter_map(|apt| char_regions.iter().position(|r| r.contains(apt)))
            .unique()
            .collect_vec();
        match *region_is {
            [] => char_regions.push(HashSet::from_iter([pt])),
            [i] => {
                char_regions[i].insert(pt);
            }
            [i, j] => {
                // This point connects two previously disjoint regions
                let (min, max) = (i.min(j), i.max(j));
                let r2 = char_regions.remove(max);
                char_regions[min].extend(r2);
                char_regions[min].insert(pt);
            }
            _ => unreachable!(),
        }
    }
    regions_by_char
}

fn price_perimeter(region: HashSet<P>) -> usize {
    let area = region.len();
    let perimeter = region
        .iter()
        .flat_map(|pt| pt.direct_neighbors())
        .filter(|pt| !region.contains(pt))
        .count();
    area * perimeter
}

fn part_2(grid: &Parsed) -> usize {
    collect_regions(grid)
        .into_values()
        .flat_map(Vec::into_iter)
        .map(price_sides)
        .map(|p| dbg!(p))
        .sum()
}

// TODO this does not consider holes
fn price_sides(region: HashSet<P>) -> usize {
    let area = region.len();
    // The minimum point is guaranteed to have an edge on the west side and north side,
    // so we begin traversing the west side
    // but drop the first item to complete the iteration
    // when we reach our starting point again.
    let top_left = region.iter().min().unwrap().clone();
    let start = (top_left, P::W);
    // Move along edges, clockwise
    let num_sides = successors(Some(start.clone()), |(pt, edge_dir)| {
        let move_dir = edge_dir.rotated_right(90);
        let pt_beyond_edge = pt + edge_dir;
        let pt_forward = pt + &move_dir;
        if region.contains(&pt_beyond_edge) {
            // side ends inwards
            Some((pt_beyond_edge, -move_dir))
        } else if !region.contains(&pt_forward) {
            // side ends outwards; don't move!
            Some((*pt, move_dir))
        } else {
            // side continues
            Some((pt_forward, *edge_dir))
        }
    })
    .skip(1)
    .take_while_inclusive(|s| s != &start)
    .map(|(_, e)| e)
    .dedup_with_count()
    .count();
    area * num_sides
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    use grid2d::Size;

    const TEST_INPUT_ABCDE: &str = "\
        AAAA\n\
        BBCD\n\
        BBCC\n\
        EEEC\n\
        ";

    const TEST_INPUT_OX: &str = "\
        OOOOO\n\
        OXOXO\n\
        OOOOO\n\
        OXOXO\n\
        OOOOO\n\
        ";

    const TEST_INPUT_EX: &str = "\
        EEEEE\n\
        EXXXX\n\
        EEEEE\n\
        EXXXX\n\
        EEEEE\n\
        ";

    const TEST_INPUT_AB: &str = "\
        AAAAAA\n\
        AAABBA\n\
        AAABBA\n\
        ABBAAA\n\
        ABBAAA\n\
        AAAAAA\n\
        ";

    const TEST_INPUT: &str = "\
        RRRRIICCFF\n\
        RRRRIICCCF\n\
        VVRRRCCFFF\n\
        VVRCCCJFFF\n\
        VVVVCJJCFE\n\
        VVIVCCJJEE\n\
        VVIIICJJEE\n\
        MIIIIIJJEE\n\
        MIIISIJEEE\n\
        MMMISSJEEE\n\
        ";

    test!(abcde, TEST_INPUT_ABCDE, part_1() == 140);
    test!(ox, TEST_INPUT_OX, part_1() == 772);
    test!(part_1() == 1930);

    test!(abcde, TEST_INPUT_ABCDE, part_2() == 80);
    test!(ex, TEST_INPUT_EX, part_2() == 236);
    test!(ab, TEST_INPUT_AB, part_2() == 368);
    test!(part_2() == 1206);

    bench_parse!(|p: &Parsed| p.size, Size(140, 140));
    bench!(part_1() == 1421958);
    // bench!(part_2() == 0);
}
