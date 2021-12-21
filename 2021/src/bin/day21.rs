#![feature(explicit_generic_args_with_impl_trait)]
#![feature(iter_advance_by)]
#![feature(test)]

use std::hash::Hash;

use aoc2021::*;
use aoc2021::collections::HashMap;

const DAY: usize = 21;

type Parsed = Vec<usize>;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    input
        .trim()
        .lines()
        .map(|line| line.chars().last().unwrap().to_digit(10).unwrap() as usize)
        .collect()
}

fn part_1(parsed: &Parsed) -> usize {
    let mut players = [
        Player { pos: parsed[0], sum: 0 },
        Player { pos: parsed[1], sum: 0 },
    ];
    let mut die = (1..=100).cycle();
    for i in 0.. {
        let player = &mut players[i % 2];
        let result: usize = next_arr::<usize, 3>(&mut die).unwrap().into_iter().sum();
        player.pos = (player.pos + result - 1) % 10 + 1;
        *&mut player.sum += player.pos;
        if player.sum >= 1000 {
            return players[(i + 1) % 2].sum * (i + 1) * 3;
        }
    };
    unreachable!();
}

fn next_arr<T, const N: usize>(iter: &mut impl Iterator<Item=T>) -> Result<[T; N], usize>
where T: Default + Copy {
    let mut res = [T::default(); N];
    for (i, item) in res.iter_mut().enumerate() {
        *item = iter.next().ok_or(i)?;
    }
    Ok(res)
}

fn part_2(parsed: &Parsed) -> u64 {
    // Count occurances of distinct states,
    // where a state consists of the player positions and their counter;
    // this set is finite over 10**2 * 1000**2 combinations,
    // wheras otherwise we might need to track … a lot.
    // Since we iteratively go over all entries in our state map,
    // this is probably comparable to a breadth-first search.
    let start = State {
        players: [
            Player { pos: parsed[0], sum: 0 },
            Player { pos: parsed[1], sum: 0 },
        ],
        next_player: false,
    };
    let mut all_states = HashMap::default();
    all_states.insert(start, 1);
    let mut wins = [0; 2];
    while let Some((state, old_count)) = drain_some_entry(&mut all_states) {
        for (rolled, times) in QUANTUM_SPLIT.iter() {
            let mut new_state = state.clone();
            let mut player = &mut new_state.players[new_state.next_player as usize];
            player.pos = (player.pos + rolled - 1) % 10 + 1;
            player.sum += player.pos;
            let new_count = old_count * times;
            if player.sum >= 21 {
                *&mut wins[new_state.next_player as usize] += new_count;
            } else {
                *&mut new_state.next_player ^= true;
                *all_states.entry(new_state).or_default() += new_count;
            }
        }
    }
    wins.into_iter().max().unwrap()
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct Player {
    pos: usize,
    sum: usize,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct State {
    players: [Player; 2],
    next_player: bool,
}

fn drain_some_entry<K, V>(map: &mut HashMap<K, V>) -> Option<(K, V)>
where K: Clone + Eq + Hash {
    map.keys().next().cloned().and_then(|key| map.remove_entry(&key))
}

const QUANTUM_SPLIT: [(usize, u64); 7] = [ // 3*3*3=27 total
    (3, 1), // 1 1 1
    (4, 3), // 2 1 1 *3
    (5, 6), // 2 2 1 *3 + 3 1 1 *3
    (6, 7), // 2 2 2 + 1 2 3 *6
    (7, 6), // 3 3 1 *3 + 3 2 2 *3
    (8, 3), // 3 3 2 *3
    (9, 1), // 3 3 3
];

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        Player 1 starting position: 4\n\
        Player 2 starting position: 8\n\
        ";

    test!(part_1() == 739785);
    test!(part_2() == 444356092776315); // occupies 49 bits
    bench_parse!(Vec::len, 2);
    bench!(part_1() == 598416);
    bench!(part_2() == 27674034218179);
}
