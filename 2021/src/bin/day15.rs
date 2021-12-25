#![feature(test)]
#![feature(map_first_last)]
#![feature(map_try_insert)]

use std::collections::BTreeSet;

use aoc2021::*;
use aoc2021::coord::Point;
use aoc2021::grid2d::Grid2D;
use aoc2021::collections::HashMap;
use itertools::iproduct;

const DAY: usize = 15;

type Parsed = Grid2D<usize>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input.trim().lines()
        .map(|line| line.bytes().map(|b| (b - b'0') as usize))
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    solve(parsed)
}

fn part_2(parsed: &Parsed) -> usize {
    let grid = expand_grid(parsed);
    solve(&grid)
}

fn solve(grid: &Parsed) -> usize {
    // Dijkstra is significantly slower than A* (~40s vs 200ms for part 2).
    // https://www.redblobgames.com/pathfinding/a-star/introduction.html for a recap.
    let mut seen: HashMap<_, usize> = HashMap::default();
    // We use BTreeSet as a priority queue
    // with the priority at the tuple's first position.
    let mut queue: BTreeSet<_> = [(0, 0, Point::new([0, 0]))].into();
    let end = Point::new([grid.size.0 as i32 - 1, grid.size.1 as i32 - 1]);
    let mut least_to_end = usize::MAX;
    while let Some((prio, cost, pt)) = queue.pop_first() {
        if pt == end {
            least_to_end = least_to_end.min(cost);
        } else if prio >= least_to_end {
            continue;
        }
        let mut is_smaller = true;
        seen.entry(pt.clone())
            .and_modify(|s| { is_smaller = cost < *s; *s = (*s).min(cost) })
            .or_insert(cost);
        if is_smaller {
            let successors = grid.contained(pt.direct_neighbors())
                .into_iter()
                .map(|next_pt| {
                    let next_cost = cost + grid.get(&next_pt).unwrap();
                    let next_prio = next_cost + (end - pt).manhattan() as usize;
                    (next_prio, next_cost, next_pt)
                });
            queue.extend(successors);
        }
    }
    least_to_end
}

fn expand_grid(grid: &Parsed) -> Parsed {
    iproduct!(0..5, grid.grid.iter())
        .map(|(i, row)|
            iproduct!(0..5, row.iter())
                .map(move |(j, n)| (n + i + j - 1) % 9 + 1)
        )
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    use aoc2021::grid2d::Size;

    const TEST_INPUT: &str = "\
        1163751742\n\
        1381373672\n\
        2136511328\n\
        3694931569\n\
        7463417111\n\
        1319128137\n\
        1359912421\n\
        3125421639\n\
        1293138521\n\
        2311944581\n\
        ";

    test!(part_1() == 40);
    test!(part_2() == 315);
    bench_parse!(|x: &Parsed| x.size, Size(100, 100));
    bench!(part_1() == 508);
    bench!(part_2() == 2872);

    #[test]
    fn test_expand_grid_1x1() {
        let parsed: Parsed = [[8]].into_iter().collect();
        let expected: Parsed = [
            [8, 9, 1, 2, 3],
            [9, 1, 2, 3, 4],
            [1, 2, 3, 4, 5],
            [2, 3, 4, 5, 6],
            [3, 4, 5, 6, 7],
        ].into_iter().collect();
        assert_eq!(expand_grid(&parsed), expected);
    }

    #[test]
    fn test_expand_grid_2x2() {
        let parsed: Parsed = [[1, 2], [7, 8]].into_iter().collect();
        let expected: Parsed = parse_input("\
            1223344556\n\
            7889911223\n\
            2334455667\n\
            8991122334\n\
            3445566778\n\
            9112233445\n\
            4556677889\n\
            1223344556\n\
            5667788991\n\
            2334455667\n\
        ");
        assert_eq!(expand_grid(&parsed), expected);
    }

    #[test]
    fn test_expand_grid_50x50() {
        let parsed = parse_input(TEST_INPUT);
        let expected = parse_input("\
            11637517422274862853338597396444961841755517295286\n\
            13813736722492484783351359589446246169155735727126\n\
            21365113283247622439435873354154698446526571955763\n\
            36949315694715142671582625378269373648937148475914\n\
            74634171118574528222968563933317967414442817852555\n\
            13191281372421239248353234135946434524615754563572\n\
            13599124212461123532357223464346833457545794456865\n\
            31254216394236532741534764385264587549637569865174\n\
            12931385212314249632342535174345364628545647573965\n\
            23119445813422155692453326671356443778246755488935\n\
            22748628533385973964449618417555172952866628316397\n\
            24924847833513595894462461691557357271266846838237\n\
            32476224394358733541546984465265719557637682166874\n\
            47151426715826253782693736489371484759148259586125\n\
            85745282229685639333179674144428178525553928963666\n\
            24212392483532341359464345246157545635726865674683\n\
            24611235323572234643468334575457944568656815567976\n\
            42365327415347643852645875496375698651748671976285\n\
            23142496323425351743453646285456475739656758684176\n\
            34221556924533266713564437782467554889357866599146\n\
            33859739644496184175551729528666283163977739427418\n\
            35135958944624616915573572712668468382377957949348\n\
            43587335415469844652657195576376821668748793277985\n\
            58262537826937364893714847591482595861259361697236\n\
            96856393331796741444281785255539289636664139174777\n\
            35323413594643452461575456357268656746837976785794\n\
            35722346434683345754579445686568155679767926678187\n\
            53476438526458754963756986517486719762859782187396\n\
            34253517434536462854564757396567586841767869795287\n\
            45332667135644377824675548893578665991468977611257\n\
            44961841755517295286662831639777394274188841538529\n\
            46246169155735727126684683823779579493488168151459\n\
            54698446526571955763768216687487932779859814388196\n\
            69373648937148475914825958612593616972361472718347\n\
            17967414442817852555392896366641391747775241285888\n\
            46434524615754563572686567468379767857948187896815\n\
            46833457545794456865681556797679266781878137789298\n\
            64587549637569865174867197628597821873961893298417\n\
            45364628545647573965675868417678697952878971816398\n\
            56443778246755488935786659914689776112579188722368\n\
            55172952866628316397773942741888415385299952649631\n\
            57357271266846838237795794934881681514599279262561\n\
            65719557637682166874879327798598143881961925499217\n\
            71484759148259586125936169723614727183472583829458\n\
            28178525553928963666413917477752412858886352396999\n\
            57545635726865674683797678579481878968159298917926\n\
            57944568656815567976792667818781377892989248891319\n\
            75698651748671976285978218739618932984172914319528\n\
            56475739656758684176786979528789718163989182927419\n\
            67554889357866599146897761125791887223681299833479\n\
        ");
        assert_eq!(expand_grid(&parsed), expected);
    }
}
