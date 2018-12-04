#![feature(try_trait,test)]

extern crate chrono;
#[macro_use] extern crate error_chain;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate test;

use std::cell::RefCell;
use std::collections::hash_map::HashMap;
use std::rc::Rc;

use chrono::{Duration, NaiveDate, NaiveDateTime, Timelike};
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
        else if caps.get(3).is_some() {
            Ok(Event::Sleep(time))
        }
        else if caps.get(4).is_some() {
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
    let mut events: Vec<Event> = input.trim().lines()
        .map(|line| Event::parse(line).unwrap())
        .collect();

    events.sort_unstable_by(|a, b| a.time().cmp(b.time()));
    // events.sort_unstable();
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
    let mut guards: HashMap<Guard, Vec<Duty>> = HashMap::new();
    for duty in input {
        // Move duties out of Rc<RefCell> while iterating
        let duty = Rc::try_unwrap(duty).expect("there's a still a reference").into_inner();
        let duties = guards.entry(duty.guard).or_insert_with(Vec::new);
        duties.push(duty)
    }
    let (lazy_guard, _) = guards.iter().max_by_key(|(_, duties)| -> u32 {
        duties.iter().map(|duty| -> u32 {
            duty.sleep_periods.iter().map(|(a, b)| b - a).sum()
        }).sum()
    }).expect("no maximum?");
    let lazy_duties = &guards[lazy_guard];

    let (minute, _) = (0..60u32).map(|minute| -> (u32, usize){
        let count: usize = lazy_duties.iter().map(|duty| {
            duty.sleep_periods.iter().filter(|(a, b)| *a <= minute && minute <= *b).count()
        }).sum();
        (minute, count)
    }).max_by_key(|(_, c)| *c).expect("no maximum?");

    lazy_guard * (minute as usize)
}


fn main() {
    let input_str = include_str!("../../input.txt");
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
    fn parse_events() {
        use super::Event::*;
        let input_str = "\
            [1518-11-01 00:00] Guard #10 begins shift\n\
            [1518-11-01 00:05] falls asleep\n\
            [1518-11-01 00:25] wakes up\n\
            [1518-11-01 00:30] falls asleep\n\
            [1518-11-01 00:55] wakes up";
        let events: Vec<Event> = input_str.trim().lines()
            .map(|line| Event::parse(line).unwrap())
            .collect();
        let expected = vec![
            Begin("1518-11-01T00:00:00".parse::<NaiveDateTime>().unwrap(), 10),
            Sleep("1518-11-01T00:05:00".parse::<NaiveDateTime>().unwrap()),
            Wake("1518-11-01T00:25:00".parse::<NaiveDateTime>().unwrap()),
            Sleep("1518-11-01T00:30:00".parse::<NaiveDateTime>().unwrap()),
            Wake("1518-11-01T00:55:00".parse::<NaiveDateTime>().unwrap()),
        ];
        assert_eq!(events, expected);
    }

    #[test]
    fn with_test_input() {
        let input_str = test_input();
        let input = parse_input(&input_str);
        assert_eq!(process(input), 240);
    }

    #[bench]
    fn bench_real_input(b: &mut Bencher) {
        let input_str = include_str!("../../input.txt");
        b.iter(|| {
            // parsing needs to be included as it's a significant part of the algo
            let input = parse_input(input_str);
            assert_eq!(process(input), 4716);
        });
    }
}
