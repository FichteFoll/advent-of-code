#![feature(test)]

use std::cell::RefCell;

use aoc2022::*;
use parse::parse_input;

const DAY: usize = 5;

type Stack = Vec<char>;
type Parsed = (Vec<Stack>, Vec<Instr>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instr {
    pub count: usize,
    pub from: usize,
    pub to: usize,
}

main!();

mod parse {
    use super::*;

    pub fn parse_input(input: &str) -> Parsed {
        let (stacks_block, instr_block) = input.split_once("\n\n").unwrap();
        let stacks = parse_stacks(stacks_block);
        let instrs = parse_instrs(instr_block);
        (stacks, instrs)
    }

    fn parse_stacks(stacks_block: &str) -> Vec<Vec<char>> {
        let mut stacks_block_iter = stacks_block.lines().rev();
        // +1 because of the missing newline
        let num_stacks = stacks_block_iter.next().unwrap().len() / 4 + 1;
        let mut stacks = vec![vec![]; num_stacks];
        for line in stacks_block_iter {
            let lbytes = line.as_bytes();
            (0..num_stacks)
                .flat_map(|i|
                    lbytes.get(i * 4 + 1).map(|&b| (i, b as char))
                )
                .filter(|(_, c)| *c != ' ')
                .for_each(|(i, c)| stacks[i].push(c));
        }
        stacks
    }

    fn parse_instrs(instr_block: &str) -> Vec<Instr> {
        instr_block.lines()
            .map(|line| {
                let words: Vec<_> = line.split_ascii_whitespace().collect();
                Instr {
                    count: words[1].parse().unwrap(),
                    from: words[3].parse::<usize>().unwrap() - 1,
                    to: words[5].parse::<usize>().unwrap() - 1,
                }
            })
            .collect()
    }
}

fn part_1(parsed: &Parsed) -> String {
    operate_crane(parsed, true)
}

fn part_2(parsed: &Parsed) -> String {
    operate_crane(parsed, false)
}

fn operate_crane(parsed: &Parsed, reverse: bool) -> String {
    let stacks: Vec<_> = parsed.0.iter().cloned().map(RefCell::new).collect();
    for instr in parsed.1.iter() {
        let mut from_stack = stacks[instr.from].borrow_mut();
        let mut to_stack = stacks[instr.to].borrow_mut();
        let from_len_after = from_stack.len() - instr.count;
        if reverse {
            to_stack.extend(from_stack.drain(from_len_after..).rev());
        } else {
            to_stack.extend(from_stack.drain(from_len_after..));
        };
    }
    stacks.into_iter()
        .flat_map(|s| s.borrow_mut().pop())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate test;

    // leading whitespace would be stripped
    // if I didn't have it on the first line
    const TEST_INPUT: &str = "    [D]    \n\
        [N] [C]    \n\
        [Z] [M] [P]\n\
         1   2   3 \n\
        \n\
        move 1 from 2 to 1\n\
        move 3 from 1 to 3\n\
        move 2 from 2 to 1\n\
        move 1 from 1 to 2\n\
        ";

    test!(part_1() == "CMZ".to_string());
    test!(part_2() == "MCD".to_string());
    bench_parse!(|p: &Parsed| (p.0.len(), p.1.len()), (9, 501));
    bench!(part_1() == "VRWBSFZWM".to_string());
    bench!(part_2() == "RBTWJWMCF".to_string());

    #[test]
    fn test_parse_example() {
        let expected = (
            vec![vec!['Z', 'N'], vec!['M', 'C', 'D'], vec!['P']],
            vec![
                Instr { count: 1, from: 1, to: 0 },
                Instr { count: 3, from: 0, to: 2 },
                Instr { count: 2, from: 1, to: 0 },
                Instr { count: 1, from: 0, to: 1 },
            ],
        );
        assert_eq!(parse_input(TEST_INPUT), expected);
    }
}
