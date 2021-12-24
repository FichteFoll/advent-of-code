#![feature(bool_to_option)]
#![feature(test)]

use std::collections::VecDeque;
use std::fmt::Display;
use std::ops::Range;

use aoc2021::*;
use aoc2021::collections::HashMap;

const DAY: usize = 23;

type Rooms = [Vec<char>; 4];
type Parsed = Rooms;

fn main() {
    let input = read_input!();
    let parsed = parse_input(&input);
    println!("Part 1: {}", part_1(&parsed));
    println!("Part 2: {}", part_2(&parsed));
}

fn parse_input(input: &str) -> Parsed {
    let mut rooms: Rooms = [vec![], vec![], vec![], vec![]];
    for line in input.trim().lines().rev() {
        let row = line.chars().filter(|&c| c != ' ' && c != '#' && c != '.');
        for (i, b) in row.enumerate() {
            rooms[i].push(b);
        }
    }
    rooms
}

fn part_1(parsed: &Parsed) -> usize {
    solve(parsed.clone())
}

fn part_2(parsed: &Parsed) -> usize {
    let rooms = rooms_for_part_2(parsed);
    solve(rooms)
}

fn rooms_for_part_2(parsed: &Parsed) -> Rooms {
    let mut rooms = parsed.clone();
    // Push these into the rooms from position 1:
    // #D#C#B#A#
    // #D#B#A#C#
    rooms[0].insert(1, 'D');
    rooms[1].insert(1, 'C');
    rooms[2].insert(1, 'B');
    rooms[3].insert(1, 'A');

    rooms[0].insert(1, 'D');
    rooms[1].insert(1, 'B');
    rooms[2].insert(1, 'A');
    rooms[3].insert(1, 'C');
    rooms
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct State {
    hallway: [Option<char>; 11],
    rooms: Rooms,
    room_depth: usize,
}

fn solve(rooms: Rooms) -> usize {
    let start = State::new(rooms);
    let end = [
        vec!['A'].repeat(start.room_depth),
        vec!['B'].repeat(start.room_depth),
        vec!['C'].repeat(start.room_depth),
        vec!['D'].repeat(start.room_depth),
    ];
    // Track the shortest path to a given state
    let mut seen: HashMap<State, usize> = Default::default();
    let mut queue: VecDeque<(State, usize)> = [(start, 0)].into();
    while let Some((state, steps)) = queue.pop_front() { // back or front is roughly the same speed
        let mut is_smaller = true;
        seen.entry(state.clone())
            .and_modify(|s| { is_smaller = steps < *s; *s = (*s).min(steps) })
            .or_insert(steps);
        if is_smaller {
            queue.extend(state.all_successors(steps));
        }
    }

    *seen.get(&State::new(end)).unwrap()
}

const ROOM_INDEXES: [usize; 4] = [2, 4, 6, 8];

const COSTS: [usize; 4] = [
    1,
    10,
    100,
    1000,
];

impl State {
    fn new(rooms: Rooms) -> Self {
        let room_depth = rooms.iter().map(Vec::len).max().unwrap();
        State {
            hallway: [None; 11],
            rooms,
            room_depth,
        }
    }

    fn all_successors(&self, steps: usize) -> Vec<(Self, usize)> {
        let mut succ = vec![];
        // Try various operations on the state and yield/push any that match
        //
        // Move any from hallway into their room
        succ.extend(
            self.hallway.iter().enumerate()
                .filter_map(|(start, co)| co.and_then(|c| {
                    let kind = amphoid_i(c);
                    let end = ROOM_INDEXES[kind];
                    let can_be_entered = self.rooms[kind].iter().all(|&c2| c2 == c);
                    let range = range_any_dir(start, end);
                    let hallway_is_free = self.hallway[range.clone()].iter().flatten().count() == 0;
                    (can_be_entered && hallway_is_free).then(|| {
                        let add_steps = COSTS[kind]
                            * (range.count() + self.room_depth - self.rooms[kind].len());
                        let mut new_state = self.clone();
                        new_state.hallway[start] = None;
                        new_state.rooms[kind].push(c);
                        (new_state, steps + add_steps)
                    })
                }))
        );

        // Move from room into the hallway
        succ.extend(
            self.rooms.iter().enumerate()
                .flat_map(|(room_i, room)| {
                    if room.iter().all(|&c| amphoid_i(c) == room_i) {
                        return None;
                    }
                    room.last().map(|&c| {
                        let kind = amphoid_i(c);
                        let start = ROOM_INDEXES[room_i];
                        let reachable_right = self.hallway.iter()
                            .enumerate()
                            .skip(start + 1)
                            .map_while(|(i, pos)| pos.is_none().then_some(i))
                            .filter(|n| !ROOM_INDEXES.contains(n));
                        let reachable_left = self.hallway.iter()
                            .enumerate()
                            .rev()
                            .skip(self.hallway.len() - start - 1)
                            .map_while(|(i, pos)| pos.is_none().then_some(i))
                            .filter(|n| !ROOM_INDEXES.contains(n));
                        reachable_right.chain(reachable_left)
                            .map(move |end| {
                                let range = range_any_dir(start, end);
                                let add_steps = COSTS[kind]
                                    * (range.count() + self.room_depth - self.rooms[room_i].len() + 1);
                                let mut new_state = self.clone();
                                new_state.hallway[end] = Some(c);
                                new_state.rooms[room_i].pop();
                                (new_state, steps + add_steps)
                            })
                    })
                })
                .flatten()
        );
        succ
    }
}


fn amphoid_i(c: char) -> usize {
    c as usize - b'A' as usize
}

fn range_any_dir(start: usize, end: usize) -> Range<usize> {
    // end is inclusive, start is exclusive
    (start + 1).min(end)..(end + 1).max(start)
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hallway = self.hallway.iter().map(|h| h.unwrap_or('.')).collect::<String>();
        write!(f, "State {{ \"{hallway}\"; {:?}}}", self.rooms)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    const TEST_INPUT: &str = "\
        #############\n\
        #...........#\n\
        ###B#C#B#D###\n\
          #A#D#C#A#\n\
          #########\n\
        ";

    test!(part_1() == 12521);
    test!(part_2() == 44169);
    bench_parse!(|x| x, &[vec!['C', 'B'], vec!['D', 'A'], vec!['D', 'B'], vec!['A', 'C']]);
    bench!(part_1() == 14510);
    bench!(part_2() == 49180);

    #[test]
    fn part_1_simple() {
        let parsed = [vec!['B'], vec!['A'], vec!['C'], vec!['D']];
        assert_eq!(part_1(&parsed), 2 + 4 * 10 + 4);
    }

    #[test]
    fn test_rooms_for_part_2() {
        let parsed = parse_input(TEST_INPUT);
        let rooms = rooms_for_part_2(&parsed);
        // #############
        // #...........#
        // ###B#C#B#D###
        //   #D#C#B#A#
        //   #D#B#A#C#
        //   #A#D#C#A#
        //   #########
        assert_eq!(rooms, [
            vec!['A', 'D', 'D', 'B'],
            vec!['D', 'B', 'C', 'C'],
            vec!['C', 'A', 'B', 'B'],
            vec!['A', 'C', 'A', 'D'],
        ]);
    }

    mod all_successors {
        use super::*;

        #[test]
        fn from_start_single() {
            let parsed = [vec!['B'], vec!['A'], vec!['C'], vec!['D']];
            let start = State::new(parsed);
            println!("{start}\n");
            let succ = start.all_successors(0);
            for state in succ.iter() {
                println!("{}: {}", state.0, state.1);
            }
            assert_eq!(succ.len(), 2 * 7);

            let mut expected_first = State::new([vec![], vec!['A'], vec!['C'], vec!['D']]);
            expected_first.hallway[3] = Some('B');
            assert_eq!(succ.first().unwrap(), &(expected_first, 20));
            let mut expected_last = State::new([vec!['B'], vec![], vec!['C'], vec!['D']]);
            expected_last.hallway[0] = Some('A');
            assert_eq!(succ.last().unwrap(), &(expected_last, 5));
        }

        #[test]
        fn from_start() {
            let parsed = parse_input(TEST_INPUT);
            let start = State::new(parsed.clone());
            println!("{start}\n");
            let succ = start.all_successors(0);
            for state in succ.iter() {
                println!("{}: {}", state.0, state.1);
            }
            assert_eq!(succ.len(), 4 * 7);

            let mut expected_first = State::new([vec!['A'], vec!['D', 'C'], vec!['C', 'B'], vec!['A', 'D']]);
            expected_first.hallway[3] = Some('B');
            assert_eq!(succ.first().unwrap(), &(expected_first, 20));
            let mut expected_last = State::new([vec!['A', 'B'], vec!['D', 'C'], vec!['C', 'B'], vec!['A']]);
            expected_last.hallway[0] = Some('D');
            assert_eq!(succ.last().unwrap(), &(expected_last, 9000));
        }

        #[test]
        fn moves_into_empty_room() {
            let mut start = State::new([vec![], vec![], vec![], vec![]]);
            start.hallway[1] = Some('A');
            start.room_depth = 1;
            let succ = start.all_successors(0);
            assert_eq!(succ.len(), 1);
            let s = succ.first().unwrap();
            let expected = State::new([vec!['A'], vec![], vec![], vec![]]);
            assert_eq!(s, &(expected, 2));
        }

        #[test]
        fn does_not_move_into_occupied_room() {
            let mut start = State::new([vec!['B'], vec![], vec![], vec![]]);
            start.hallway[1] = Some('A');
            let succ = start.all_successors(0);
            for s in succ.iter() {
                println!("{s:?}");
                assert_eq!(s.0.hallway[1], Some('A'))
            }
        }

        #[test]
        fn no_more_moves() {
            let mut start = State::new([vec!['B'], vec![], vec![], vec![]]);
            start.hallway[1] = Some('A');
            start.hallway[3] = Some('C');
            start.hallway[5] = Some('A');
            let succ = start.all_successors(0);
            assert_eq!(succ, vec![]);
        }

        #[test]
        fn no_more_moves_final() {
            let start = State::new([vec!['A'], vec!['B'], vec!['C'], vec!['D']]);
            let succ = start.all_successors(0);
            assert_eq!(succ, vec![]);
        }
    }

}
