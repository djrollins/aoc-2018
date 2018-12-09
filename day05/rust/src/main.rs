use std::io::{self, Read, Write};
use std::mem;

type Result<T> = std::result::Result<T, Box<std::error::Error>>;

fn read_input() -> Result<String> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    Ok(input)
}

fn will_annihilate(c1: char, c2: char) -> bool {
    c1 != c2 && c1.eq_ignore_ascii_case(&c2)
}

fn run_reaction(input: &str) -> String {
    let mut input: Vec<_> = input.chars().collect();
    let mut output = Vec::new();

    loop {
        let mut stable = true;
        let mut iter = input.iter().peekable();

        while let Some(current) = iter.next() {
            if let Some(next) = iter.peek() {
                if will_annihilate(*current, **next) {
                    stable = false;
                    iter.next();
                    continue;
                }
            }

            output.push(*current);
        }

        if stable {
            break;
        }

        mem::swap(&mut input, &mut output);
        output.clear();
    }

    input.iter().collect()
}

fn part1(input: &str) -> String {
    run_reaction(input)
}

fn part2(input: &str) -> String {
    "abcdefghijklmnopqrstuvwxyz"
        .chars()
        .map(|c| {
            let string: String = input
                .chars()
                .filter(|x| !c.eq_ignore_ascii_case(x))
                .collect();
            run_reaction(&string)
        })
        .min_by_key(String::len)
        .unwrap()
}

fn main() -> Result<()> {
    let input = read_input()?;

    writeln!(io::stdout(), "{}", part1(&input).len() - 1)?;
    writeln!(io::stdout(), "{}", part2(&input).len() - 1)?;
    Ok(())
}
