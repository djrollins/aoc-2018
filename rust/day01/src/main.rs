use std::error::Error;
use std::io::{self, Read, Write};
use std::collections::HashSet;

type Result<T> = std::result::Result<T, Box<Error>>;

fn read_input() -> Result<Vec<i32>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let values: std::result::Result<Vec<i32>, _> = input.lines().map(str::parse).collect();
    Ok(values?)
}

fn part1(input: &Vec<i32>) -> i32 {
    input.iter().fold(0, |freq, change| freq + change)
}

fn part2(input: &Vec<i32>) -> i32 {
    let mut existing = HashSet::new();

    input.iter()
         .cycle()
         .scan(0, |acc, value| { *acc += value; Some(*acc) })
         .find(|freq| !existing.insert(*freq))
         .expect("should not happen")
}

fn main() -> Result<()> {
    let input = read_input()?;

    writeln!(io::stdout(), "{}", part1(&input));
    writeln!(io::stdout(), "{}", part2(&input));
    Ok(())
}
