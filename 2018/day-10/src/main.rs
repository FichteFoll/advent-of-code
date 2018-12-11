#![feature(test)]

extern crate test;

use regex::Regex;
use std::cmp::{min, max};


#[derive(Debug,Clone)]
struct Pt(isize, isize);


fn parse_input(input: &str) -> Vec<(Pt, Pt)> {
    let re = Regex::new(r"position=<\s*([-\d]+),\s*([-\d]+)> velocity=<\s*([-\d]+),\s*([-\d]+)>").unwrap();
    let lines: Vec<_> = input.trim().lines().collect();
    lines.iter().map(|line| {
        let caps = re.captures(line).expect("unexpected line");
        let values: Vec<isize> = caps.iter()
            .skip(1)
            .map(|x| x.unwrap().as_str().parse::<isize>().unwrap())
            .collect();
        (Pt(values[0], values[1]), Pt(values[2], values[3]))
    }).collect()
}

fn get_dimensions(points: &[Pt]) -> (Pt, Pt) {
    let base = points.iter().next().unwrap();
    let (mut start, mut end) = (base.clone(), base.clone());
    for pt in points {
        start.0 = min(start.0, pt.0);
        start.1 = min(start.1, pt.1);
        end.0 = max(end.0, pt.0);
        end.1 = max(end.1, pt.1);
    }
    (start, end)
}


fn visualize(particles: &[Pt]) {
    let (start, end) = get_dimensions(particles);
    let width = (end.0 - start.0 + 1) as usize;
    let heigth = (end.1 - start.1 + 1) as usize;
    let mut grid = vec![vec!['.'; width]; heigth];
    println!("{}x{}", width, heigth);
    for pt in particles {
        grid[(pt.1 - start.1) as usize][(pt.0 - start.0) as usize] = '#';
    }
    for line in grid {
        println!("{}", line.into_iter().collect::<String>());
    }
}


fn process(input_str: &str, max_heigth: isize) -> usize {
    let input = parse_input(input_str);
    let (mut particles, velocities) = input.into_iter()
        .fold((Vec::new(), Vec::new()),
            |(mut part, mut vel), new| {
                part.push(new.0);
                vel.push(new.1);
                (part, vel)
            });
    for i in 0.. {
        let (start, end) = get_dimensions(&particles);
        if end.1 - start.1 <= max_heigth {
            visualize(&particles);
            return i;
        }
        particles.iter_mut().zip(&velocities)
            .for_each(|(part, vel)| {
                part.0 += vel.0;
                part.1 += vel.1;
            });
    }
    unreachable!();
}


fn main() {
    let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
    println!("The result is {:?}", process(&input_str, 10));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn simple_input() {
        let input_str = "position=< 9,  1> velocity=< 0,  2>\n\
            position=< 7,  0> velocity=<-1,  0>\n\
            position=< 3, -2> velocity=<-1,  1>\n\
            position=< 6, 10> velocity=<-2, -1>\n\
            position=< 2, -4> velocity=< 2,  2>\n\
            position=<-6, 10> velocity=< 2, -2>\n\
            position=< 1,  8> velocity=< 1, -1>\n\
            position=< 1,  7> velocity=< 1,  0>\n\
            position=<-3, 11> velocity=< 1, -2>\n\
            position=< 7,  6> velocity=<-1, -1>\n\
            position=<-2,  3> velocity=< 1,  0>\n\
            position=<-4,  3> velocity=< 2,  0>\n\
            position=<10, -3> velocity=<-1,  1>\n\
            position=< 5, 11> velocity=< 1, -2>\n\
            position=< 4,  7> velocity=< 0, -1>\n\
            position=< 8, -2> velocity=< 0,  1>\n\
            position=<15,  0> velocity=<-2,  0>\n\
            position=< 1,  6> velocity=< 1,  0>\n\
            position=< 8,  9> velocity=< 0, -1>\n\
            position=< 3,  3> velocity=<-1,  1>\n\
            position=< 0,  5> velocity=< 0, -1>\n\
            position=<-2,  2> velocity=< 2,  0>\n\
            position=< 5, -2> velocity=< 1,  2>\n\
            position=< 1,  4> velocity=< 2,  1>\n\
            position=<-2,  7> velocity=< 2, -2>\n\
            position=< 3,  6> velocity=<-1, -1>\n\
            position=< 5,  0> velocity=< 1,  0>\n\
            position=<-6,  0> velocity=< 2,  0>\n\
            position=< 5,  9> velocity=< 1, -2>\n\
            position=<14,  7> velocity=<-2,  0>\n\
            position=<-3,  6> velocity=< 2, -1>";
        assert_eq!(process(input_str, 8), 3);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            assert_eq!(process(&input_str, 10), 10880);
        });
    }
}
