#![feature(test)]

use std::ops::RangeInclusive;

use aoc2021::*;
use itertools::iproduct;
use parse::parse_input;

const DAY: usize = 17;

type Parsed = (RangeInclusive<i32>, RangeInclusive<i32>);

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

mod parse {
    use std::ops::RangeInclusive;
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let words: Vec<_> = input.trim().split_whitespace().collect();
        (parse_range(words[2]), parse_range(words[3]))
    }

    fn parse_range(s: &str) -> RangeInclusive<i32> {
        let (start, stop) = s[2..].trim_end_matches(',').split_once("..").unwrap();
        RangeInclusive::new(start.parse().unwrap(), stop.parse().unwrap())
    }
}

fn part_1((_, y_target): &Parsed) -> i32 {
    // We can do any amount of iterations
    // because we will always be able to find an appropriate x,
    // so we only look at y.
    // The first half is a parable,
    // meaning we will always land at 0 after we left the ground,
    // assuming y < 0.
    // Thus, the highest vy_0 we can choose is the one
    // that makes us land just at the lower bound.
    assert!(*y_target.start() < 0, "This algorithm doesn't work for y >= 0");
    let v = -y_target.start() - 1;
    (1..=v).sum()
}

fn part_2((x_target, y_target): &Parsed) -> usize {
    // ty_0 <= vy_0 <= (-ty_0 - 1)
    // min_vx(tx_0) <= vx_0 <= tx_1
    let y_range = *y_target.start()..=-y_target.start() - 1;
    let x_range = min_vx(*x_target.start())..=*x_target.end();
    iproduct!(x_range, y_range)
        .filter(|(mut vx, mut vy)| {
            let (mut x, mut y) = (0, 0);
            while x <= *x_target.end() && y >= *y_target.start() {
                x += vx;
                y += vy;
                vy -= 1;
                vx -= vx.signum();
                if x_target.contains(&x) && y_target.contains(&y) {
                    return true;
                }
            }
            false
        })
        .count()
}

fn min_vx(min_x: i32) -> i32 {
    // This is a normal quadratic equation.
    // tx_0 <= vx_0 * (vx_0 + 1) / 2
    // tx_0 * 2 <= vx_0 * (vx_0 + 1)
    // tx_0 * 2 <= vx_0² + vx_0
    // tx_0 * 2 + 1/4 <= vx_0² + vx_0 + (1/4)
    // tx_0 * 2 + 1/4 <= (vx_0 + 1/2)²
    // sqrt(tx_0 * 2 + 1/4) <= vx_0 + 1/2
    // sqrt(tx_0 * 2 + 1/4) - 1/2 <= vx_0
    // (1/2) (2*sqrt(tx_0 * 2 + 1/4) - 1) <= vx_0
    // (1/2) (sqrt(tx_0 * 8 + 1) - 1) <= vx_0
    (((min_x as f32 * 8. + 1.).sqrt() - 1.) / 2.).ceil() as i32
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "target area: x=20..30, y=-10..-5";

    test!(part_1() == 45);
    test!(part_2() == 112);
    bench_parse!(|x| x, &(14i32..=50, -267i32..=-225));
    bench!(part_1() == 35511);
    bench!(part_2() == 3282);
}
