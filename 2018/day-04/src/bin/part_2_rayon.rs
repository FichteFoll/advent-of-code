#![feature(try_trait,test)]

extern crate chrono;
#[macro_use] extern crate error_chain;
#[macro_use] extern crate lazy_static;
extern crate rayon;
extern crate regex;
extern crate test;

use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::rc::Rc;

use chrono::{Duration, NaiveDate, NaiveDateTime, Timelike};
use rayon::prelude::*;
use regex::Regex;


// TYPES //
type Input = Vec<Rc<RefCell<Duty>>>;
type Output = usize;

type Guard = usize;

#[derive(Debug)]
struct Duty {
    day: NaiveDate,
    guard: Guard,
    sleep_periods: Vec<(u32, u32)>,
}

#[derive(Debug, Eq, PartialEq)]
enum Event {
    Begin(NaiveDateTime, Guard),
    Sleep(NaiveDateTime),
    Wake(NaiveDateTime),
}

impl Event {
    fn time(&self) -> &NaiveDateTime {
        use self::Event::*;
        match self {
            Begin(time, _) => time,
            Sleep(time) => time,
            Wake(time) => time,
        }
    }
}

mod parse_errors {
    error_chain!{
        errors {
            InvalidLine(line: String) {
                description("unexpected line format")
                display("unexpected line format: '{}'", line)
            }
            BadTimestamp(line: String) {
                description("unable to parse timestamp")
                display("unable to parse timestamp: '{}'", line)
            }
        }
        foreign_links {
            ChronoParse(chrono::ParseError);
            Io(std::io::Error);
            ParseInt(std::num::ParseIntError);
        }
    }
}

lazy_static! {
    static ref LINE_RE: Regex = Regex::new(r"\[([^\]]+)\] (?:Guard #(\d+) begins shift|(falls asleep)|(wakes up))").unwrap();
}

impl Event {
    fn parse(line: &str) -> parse_errors::Result<Event> {
        use self::parse_errors::*;

        let caps = LINE_RE.captures(line).expect("unexpected line");
        let time_str = caps.get(1).unwrap().as_str();
        let time = NaiveDateTime::parse_from_str(time_str, "%Y-%m-%d %H:%M") // %F %R
            .chain_err(|| ErrorKind::BadTimestamp(line.to_owned()))?;

        if let Some(guard) = caps.get(2) {
            Ok(Event::Begin(time, guard.as_str().parse::<Guard>()?))
        }
        else if let Some(_) = caps.get(3) {
            Ok(Event::Sleep(time))
        }
        else if let Some(_) = caps.get(4) {
            Ok(Event::Wake(time))
        }
        else {
            bail!(ErrorKind::InvalidLine(line.to_owned()))
        }
    }
}


// PARSE //
fn get_minutes(time: NaiveDateTime) -> u32 {
    // Minutes only count within 0:00 and 1:00 of a day
    match time.hour() {
        0 => time.minute(),
        h if h > 12 => 0,
        _ => 59,
    }
}


fn parse_input(input: &str) -> Input {
    let lines: Vec<&str> = input.trim().lines().collect();
    // major speedup through concurrency here
    let mut events: Vec<Event> = lines.par_iter()
        .map(|line| Event::parse(line).unwrap())
        .collect();

    // but not here
    events.par_sort_unstable_by(|a, b| a.time().cmp(b.time()));
    let mut out: Input= Vec::new();
    let mut current_duty: Option<Rc<RefCell<Duty>>> = None;
    let mut last_start: Option<u32> = None;
    for event in events {
        match event {
            Event::Begin(time, guard) => {
                if let (Some(start), Some(duty_cell)) = (last_start, &current_duty) {
                    duty_cell.borrow_mut().sleep_periods.push((start, 59));
                }
                let day = (time + Duration::hours(12)).date(); // ensure we're on the right day
                last_start = Some(get_minutes(time));
                let duty = Rc::new(RefCell::new(Duty {day, guard, sleep_periods: Vec::new()}));
                current_duty = Some(Rc::clone(&duty));
                out.push(duty);
            },
            Event::Sleep(time) => {
                last_start = Some(get_minutes(time));
            },
            Event::Wake(time) => {
                if let (Some(start), Some(duty_cell)) = (last_start, &current_duty) {
                    duty_cell.borrow_mut().sleep_periods.push((start, get_minutes(time) - 1));
                    last_start = None;
                }
            }
        }
    }
    out
}


// PROCESS //
fn process(input: Input) -> Output {
    let mut guard_time: HashMap<Guard, Vec<u32>> = HashMap::new();
    for duty in input {
        let duty = duty.borrow();
        let minutes = guard_time.entry(duty.guard).or_insert(vec![0u32; 60]);
        for (a, b) in duty.sleep_periods.iter() {
            for i in *a..=*b {
                minutes[i as usize] += 1;
            }
        }
    }

    // this par_iter has almost no effect
    let (sleepy_guard, (minute, _)) = guard_time.par_iter()
        .map(|(k, vs)| (k, vs.iter().enumerate().max_by_key(|(_, v)| *v).unwrap()))
        .max_by_key(|(_, (_, num))| *num).unwrap();

    sleepy_guard * (minute as usize)
}


fn main() {
    let input_str = std::fs::read_to_string("big_input.txt").expect("can’t read file");
    let input = parse_input(&input_str);
    println!("The result is {:?}", process(input));
}


#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    fn test_input() -> &'static str {
        "\
        [1518-11-01 00:00] Guard #10 begins shift\n\
        [1518-11-01 00:05] falls asleep\n\
        [1518-11-01 00:25] wakes up\n\
        [1518-11-01 00:30] falls asleep\n\
        [1518-11-01 00:55] wakes up\n\
        [1518-11-01 23:58] Guard #99 begins shift\n\
        [1518-11-02 00:40] falls asleep\n\
        [1518-11-02 00:50] wakes up\n\
        [1518-11-03 00:05] Guard #10 begins shift\n\
        [1518-11-03 00:24] falls asleep\n\
        [1518-11-03 00:29] wakes up\n\
        [1518-11-04 00:02] Guard #99 begins shift\n\
        [1518-11-04 00:36] falls asleep\n\
        [1518-11-04 00:46] wakes up\n\
        [1518-11-05 00:03] Guard #99 begins shift\n\
        [1518-11-05 00:45] falls asleep
        [1518-11-05 00:55] wakes up"
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(input), 4455);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = std::fs::read_to_string("input.txt").expect("can’t read file");
        b.iter(|| {
            // parsing needs to be included as it's a significant part of the algo
            let input = parse_input(&input_str);
            assert_eq!(process(input), 117061);
        });
    }

    #[bench]
    #[ignore]
    fn bench_big_input(b: &mut Bencher) {
        // https://mega.nz/#!Ro1BkKyS!asDqLa5PRI-JTJJnv6k9Ujt3fjxJ1gTG55vg9E2yHxg
        let input_str = std::fs::read_to_string("big_input.txt").expect("can’t read file");
        b.iter(|| {
            let input = parse_input(&input_str);
            assert_eq!(process(input), 154938);
        });
    }
}
