use std::collections::HashMap;
use std::error::Error;
use std::io::{self, BufRead, Write};
use std::str::FromStr;

use lazy_static::lazy_static;
use regex::{Captures, Regex};

fn read_input() -> Result<Vec<String>, io::Error> {
    io::stdin().lock().lines().collect()
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Timestamp {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum EventType {
    NewGuard(u32),
    Sleep(u8),
    WakeUp(u8),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Event {
    timestamp: Timestamp,
    event: EventType,
}

macro_rules! err {
    ( $($arg:tt)* ) => { Err(Box::<Error>::from(format!($($arg)*))) }
}

fn extract_capture<T>(captures: &Captures, name: &str) -> Result<T, Box<Error>>
where
    T: FromStr,
    T::Err: Error + 'static,
{
    match captures.name(name) {
        Some(capture) => Ok(capture.as_str().parse()?),
        None => err!("unable to extract group: {}", name),
    }
}

impl FromStr for Event {
    type Err = Box<dyn Error>;

    fn from_str(input: &str) -> Result<Event, Self::Err> {
        lazy_static! {
            static ref event_regex: Regex = Regex::new(
                r"(?x) # set insignificant whitespace flag
                \[     # opening bracket
                  (?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})\s+ # date
                  (?P<hour>\d{2}):(?P<minute>\d{2})                  # time
                \]       # closing bracket
                \s+(?P<event_text>.*$) # event text"
            ).unwrap();
            static ref new_guard_regex: Regex =
                Regex::new(r"Guard #(?P<id>\d+) begins shift$").unwrap();
        }

        let captures = match event_regex.captures(input) {
            Some(captures) => captures,
            None => return err!("unable to parse event: {}", input),
        };

        let minute = extract_capture(&captures, "minute")?;

        let timestamp = Timestamp {
            minute,
            hour: extract_capture(&captures, "hour")?,
            day: extract_capture(&captures, "day")?,
            month: extract_capture(&captures, "month")?,
            year: extract_capture(&captures, "year")?,
        };

        let event_text: String = extract_capture(&captures, "event_text")?;

        let event = match new_guard_regex.captures(&event_text) {
            Some(new_guard) => extract_capture(&new_guard, "id").map(EventType::NewGuard)?,
            None => match event_text.as_ref() {
                "wakes up" => EventType::WakeUp(minute),
                "falls asleep" => EventType::Sleep(minute),
                _ => return err!("unable to parse event text: {}", event_text),
            },
        };

        Ok(Event { event, timestamp })
    }
}

fn parse_events(input: &[String]) -> Result<Vec<Event>, Box<Error>> {
    input.iter().map(|s| s.parse()).collect()
}

type Schedule = HashMap<u32, [u8; 60]>;

fn schedule(input: &[Event]) -> Schedule {
    let mut logs = HashMap::new();
    let mut stack = Vec::new();

    for event in input {
        match event.event {
            EventType::NewGuard(id) => {
                if let Some(EventType::NewGuard(current_guard)) = stack.first() {
                    let logs = logs.entry(*current_guard).or_insert([0u8; 60]);
                    let rest = &stack[1..];

                    rest.chunks(2).for_each(|chunk| match chunk {
                        [EventType::Sleep(start), EventType::WakeUp(end)] => {
                            let start = *start as usize;
                            let end = *end as usize;
                            logs[start..end].iter_mut().for_each(|x| *x += 1)
                        }
                        [EventType::Sleep(start)] => {
                            let start = *start as usize;
                            let end = event.timestamp.minute as usize;
                            logs[start..end].iter_mut().for_each(|x| *x += 1)
                        }
                        _ => panic!("Unable to handle chunk: {:?}", chunk),
                    })
                }

                stack.clear();
                stack.push(EventType::NewGuard(id));
            }
            _ => stack.push(event.event.clone()),
        }
    }

    logs
}

fn part1(schedule: &Schedule) -> Option<u32> {
    schedule
        .iter()
        .max_by_key(|(_, v)| v.iter().fold(0u32, |acc, x| acc + u32::from(*x)))
        .and_then(|(k, v)| {
            v.iter()
                .enumerate()
                .max_by_key(|(_, v)| *v)
                .map(|(i, _)| i as u32 * *k)
        })
}

fn part2(schedule: &Schedule) -> Option<u32> {
    schedule
        .iter()
        .max_by_key(|(_, v)| v.iter().max())
        .and_then(|(k, v)| {
            v.iter()
                .enumerate()
                .max_by_key(|(_, v)| *v)
                .map(|(i, _)| i as u32 * *k)
        })
}

fn main() -> Result<(), Box<Error>> {
    let input = read_input()?;
    let mut input = parse_events(&input)?;
    input.sort();
    let schedule = schedule(&input);

    writeln!(io::stdout(), "part 1: {:?}", part1(&schedule))?;
    writeln!(io::stdout(), "part 2: {:?}", part2(&schedule))?;

    Ok(())
}
