#[macro_use] extern crate lazy_static;
extern crate regex;

use std::error::Error;
use std::io::{self, Read, Write};
use std::str::FromStr;

use regex::Regex;

type Result<T> = std::result::Result<T, Box<Error>>;

struct Record {
    id: i32,
    offset_x: usize,
    offset_y: usize,
    width: usize,
    height: usize,
}

impl<'a> Record {
    fn iter(&'a self) -> CoordIterator<'a> {
        CoordIterator{ record: &self, x: self.offset_x, y: self.offset_y }
    }
}

// For str::parse
impl FromStr for Record {
    type Err = Box<Error>;

    fn from_str(input: &str) -> Result<Self> {
        lazy_static! {
            static ref RE: Regex = Regex::new(
                r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
            ).unwrap();
        }

        let captures = match RE.captures(input) {
            Some(captures) => captures,
            None =>  return Err(Box::<Error>::from("no match")),
        };

        let id = captures[1].parse()?;
        let offset_x = captures[2].parse()?;
        let offset_y = captures[3].parse()?;
        let width = captures[4].parse()?;
        let height = captures[5].parse()?;

        Ok(Record { id, offset_x, offset_y, width, height })
    }
}

struct CoordIterator<'a> {
    record: &'a Record,
    x: usize,
    y: usize,
}


impl<'a> Iterator for CoordIterator<'a> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let startx = self.record.offset_x;
        let endx = self.record.offset_x + self.record.width;
        let endy = self.record.offset_y + self.record.height;

        if self.x > endx {
            self.y += 1;
            self.x = startx;
        }

        if self.y > endy {
            return None
        }

        let ret = (self.x, self.y);
        self.x += 1;
        Some(ret)
    }
}

fn part1(input: &[Record]) -> i32 {
    let mut grid = [[0; 1000]; 1000];

    input.iter().for_each(|record| {
        for (x, y) in record.iter() {
            grid[x][y] += 1;
        }
    });

    grid.iter().fold(0, |mut acc, row| {
        row.iter().for_each(|cell| {
            if *cell > 1 { acc += 1 }
        });
        acc
    })
}

fn part2(input: &[Record]) -> Option<i32> {
    let mut grid = [[0; 1000]; 1000];

    input.iter().for_each(|record| {
        for (x, y) in record.iter() {
            grid[x][y] += 1;
        }
    });

    input.iter().find(|record| {
        record.iter().all(|(x, y)| grid[x][y] == 1)
    }).map(|record| record.id)
}

fn main() -> Result<()> {
    let input = read_input()?;

    writeln!(io::stdout(), "{:?}", part1(&input))?;
    writeln!(io::stdout(), "{:?}", part2(&input))?;
    Ok(())
}
