#![feature(bool_to_option)]
#![feature(test)]

use std::collections::{HashMap, HashSet, VecDeque};

use itertools::iproduct;

use aoc2021::*;
use aoc2021::coord::Point;

const DAY: usize = 19;

type Parsed = Vec<Vec<Point<3>>>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    parse_input_nd::<3>(input)
}

fn parse_input_nd<const N: usize>(input: &str) -> Vec<Vec<Point<N>>> {
    input
        .trim()
        .split("\n\n")
        .map(|block| block.lines()
            .skip(1)
            .map(|line| line.split(',')
                .map(|s| s.parse().unwrap())
                .collect::<Vec<_>>()
                .try_into()
                .unwrap()
            )
            .collect()
        )
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let mut queue = VecDeque::new();
    let mut from_scanner_0: HashSet<_> = parsed[0].iter().cloned().collect();
    let mut reference = parsed[0].clone();
    let mut remaining: VecDeque<_> = parsed.iter().skip(1).collect();
    while !remaining.is_empty() {
        println!("#remaining: {}", remaining.len());
        let matched = remaining.iter()
            .enumerate()
            .find_map(|(i, other)| {
                println!("remaining[{i}]");
                (0..24).find_map(|rot| {
                    let rotated: Vec<_> = other.iter()
                        .cloned()
                        .map(|pt| rotate_3d(&pt, rot))
                        .collect();
                    if let Some((offset, _)) = common_beacons(&reference, &rotated, 12) {
                        let shifted: Vec<_> = rotated.into_iter().map(|pt| pt + offset).collect();
                        println!("found scanner at {offset} with rot {rot}");
                        Some((i, shifted))
                    } else {
                        None
                    }
                })
            });
        if let Some((i, beacons)) = matched {
            println!("found match for item {i}");
            queue.push_back(beacons.clone());
            from_scanner_0.extend(beacons);
            remaining.remove(i);
        } else {
            reference = queue.pop_front().unwrap();
        }
    }
    from_scanner_0.len()
}

fn part_2(_parsed: &Parsed) -> usize {
    todo!()
}

fn distance_map<const N: usize>(pts: &[Point<N>]) -> HashMap<Point<N>, HashSet<Point<N>>> {
    pts.iter()
        .map(|a| (*a, pts.iter().filter_map(|b| (b != a).then_some(b - a)).collect()))
        .collect()
}

// Find a combination of indices with the most common offset between the point pairs
// and return that offset + the number.
fn common_beacons<const N: usize>(a: &[Point<N>], b: &[Point<N>], threshold: usize) -> Option<(Point<N>, usize)> {
    // TODO cache a_map between calls in part_1
    iproduct!(
        distance_map(a).iter(),
        distance_map(b).iter()
    )
        .find_map(|((a_pt, a_dist), (b_pt, b_dist))| {
            let common = (a_dist & b_dist).len() + 1; // the origin was stripped
            (common >= threshold).then(|| (a_pt - b_pt, common))
        })
}

fn rotate_3d(pt: &Point<3>, index: usize) -> Point<3> {
    // inspired by https://stackoverflow.com/a/33190472/1327727
    assert!(index < 24, "index too high");
    let axis = index / 8;
    let flip = (index % 8) >= 4;
    let rot = index % 4;
    let mut new_pt = *pt;
    match (axis, flip) {
        (0, false) => (),
        (0, true) => new_pt.rotate_left_3(1, 180),
        (1, false) => new_pt.rotate_left_3(2, 90),
        (1, true) => new_pt.rotate_left_3(2, 270),
        (2, false) => new_pt.rotate_left_3(1, 90),
        (2, true) => new_pt.rotate_left_3(1, 270),
        _ => unreachable!(),
    }
    new_pt.rotate_left_3(axis, rot as i32 * 90);
    new_pt
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    test!(part_1() == 79);
    // test!(part_2() == 0);
    bench_parse!(Vec::len, 30);
    // bench!(part_1() == 0);
    // bench!(part_2() == 0);

    #[test]
    fn find_common_12beacons_2scanners_3d_any_orientation() {
        let parsed: Parsed = parse_input(TEST_INPUT).into_iter().take(2).collect();
        // I don't know the rotation identifier, so I need to check them all
        let reference = &parsed[0];
        let result = (0..24).find_map(|rot| {
            println!("rot: {rot}");
            let rotated: Vec<_> = parsed[1].iter()
                .cloned()
                .map(|pt| rotate_3d(&pt, rot))
                .collect();
            println!("rotated: {rotated:?}");
            let res = common_beacons(reference, &rotated, 12);
            println!("res: {res:?}");
            res
        });
        assert_eq!(result, Some((Point::new([68, -1246, -43]), 12)));
    }

    #[test]
    fn find_common_3beacons_2scanners_2d_same_orientation() {
        let input = "\
            --- scanner 0 ---\n\
            0,2\n\
            4,1\n\
            3,3\n\
            \n\
            --- scanner 1 ---\n\
            -1,-1\n\
            -5,0\n\
            -2,1\n\
            ";
        let parsed = parse_input_nd::<2>(input);
        assert_eq!(
            common_beacons(&parsed[0], &parsed[1], 3),
            Some((Point::new([5, 2]), 3))
        );
    }

    #[test]
    fn find_common_3beacons_2scanners_2d_same_orientation_with_extra() {
        let input = "\
            --- scanner 0 ---\n\
            0,2\n\
            4,1\n\
            -999,0\n\
            3,3\n\
            \n\
            --- scanner 1 ---\n\
            -1,-1\n\
            -5,0\n\
            -2,1\n\
            -3,999\n\
            ";
        let parsed = parse_input_nd::<2>(input);
        assert_eq!(
            common_beacons(&parsed[0], &parsed[1], 3),
            Some((Point::new([5, 2]), 3))
        );
    }

    #[test]
    fn rotate_no_change() {
        let pt = Point::new([8, 0, 7]);
        let new_pt = rotate_3d(&pt, 0);
        assert_eq!(pt, new_pt);
    }

    #[test]
    fn rotate_distinct_results() {
        let pt = Point::new([1, 2, 3]); // only distinct for 3 different non-zero numbers
        let deduped_rotations: HashSet<_> = (0..24).map(|i| rotate_3d(&pt, i)).collect();
        assert_eq!(deduped_rotations.len(), 24);
    }

    #[test]
    #[should_panic]
    fn rotate_panics_for_24() {
        let pt = Point::new([8, 1, 7]);
        rotate_3d(&pt, 24);
    }

    const TEST_INPUT: &str = "\
        --- scanner 0 ---\n\
        404,-588,-901\n\
        528,-643,409\n\
        -838,591,734\n\
        390,-675,-793\n\
        -537,-823,-458\n\
        -485,-357,347\n\
        -345,-311,381\n\
        -661,-816,-575\n\
        -876,649,763\n\
        -618,-824,-621\n\
        553,345,-567\n\
        474,580,667\n\
        -447,-329,318\n\
        -584,868,-557\n\
        544,-627,-890\n\
        564,392,-477\n\
        455,729,728\n\
        -892,524,684\n\
        -689,845,-530\n\
        423,-701,434\n\
        7,-33,-71\n\
        630,319,-379\n\
        443,580,662\n\
        -789,900,-551\n\
        459,-707,401\n\
        \n\
        --- scanner 1 ---\n\
        686,422,578\n\
        605,423,415\n\
        515,917,-361\n\
        -336,658,858\n\
        95,138,22\n\
        -476,619,847\n\
        -340,-569,-846\n\
        567,-361,727\n\
        -460,603,-452\n\
        669,-402,600\n\
        729,430,532\n\
        -500,-761,534\n\
        -322,571,750\n\
        -466,-666,-811\n\
        -429,-592,574\n\
        -355,545,-477\n\
        703,-491,-529\n\
        -328,-685,520\n\
        413,935,-424\n\
        -391,539,-444\n\
        586,-435,557\n\
        -364,-763,-893\n\
        807,-499,-711\n\
        755,-354,-619\n\
        553,889,-390\n\
        \n\
        --- scanner 2 ---\n\
        649,640,665\n\
        682,-795,504\n\
        -784,533,-524\n\
        -644,584,-595\n\
        -588,-843,648\n\
        -30,6,44\n\
        -674,560,763\n\
        500,723,-460\n\
        609,671,-379\n\
        -555,-800,653\n\
        -675,-892,-343\n\
        697,-426,-610\n\
        578,704,681\n\
        493,664,-388\n\
        -671,-858,530\n\
        -667,343,800\n\
        571,-461,-707\n\
        -138,-166,112\n\
        -889,563,-600\n\
        646,-828,498\n\
        640,759,510\n\
        -630,509,768\n\
        -681,-892,-333\n\
        673,-379,-804\n\
        -742,-814,-386\n\
        577,-820,562\n\
        \n\
        --- scanner 3 ---\n\
        -589,542,597\n\
        605,-692,669\n\
        -500,565,-823\n\
        -660,373,557\n\
        -458,-679,-417\n\
        -488,449,543\n\
        -626,468,-788\n\
        338,-750,-386\n\
        528,-832,-391\n\
        562,-778,733\n\
        -938,-730,414\n\
        543,643,-506\n\
        -524,371,-870\n\
        407,773,750\n\
        -104,29,83\n\
        378,-903,-323\n\
        -778,-728,485\n\
        426,699,580\n\
        -438,-605,-362\n\
        -469,-447,-387\n\
        509,732,623\n\
        647,635,-688\n\
        -868,-804,481\n\
        614,-800,639\n\
        595,780,-596\n\
        \n\
        --- scanner 4 ---\n\
        727,592,562\n\
        -293,-554,779\n\
        441,611,-461\n\
        -714,465,-776\n\
        -743,427,-804\n\
        -660,-479,-426\n\
        832,-632,460\n\
        927,-485,-438\n\
        408,393,-506\n\
        466,436,-512\n\
        110,16,151\n\
        -258,-428,682\n\
        -393,719,612\n\
        -211,-452,876\n\
        808,-476,-593\n\
        -575,615,604\n\
        -485,667,467\n\
        -680,325,-822\n\
        -627,-443,-432\n\
        872,-547,-609\n\
        833,512,582\n\
        807,604,487\n\
        839,-516,451\n\
        891,-625,532\n\
        -652,-548,-490\n\
        30,-46,-14\n\
        ";
}

// 3 * 2 * 2 * 2
// primary index, primary flipped(?), others swapped, others flipped
// +++ []
// ++- [2]
// +-- [1,2]
// -++
// --+
// ---
// +-+
