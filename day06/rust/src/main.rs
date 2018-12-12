use std::error::Error;
use std::io::{self, Read, Write};
use std::iter::repeat;

type Result<T> = std::result::Result<T, Box<Error>>;

macro_rules! err {
    ( $($arg:tt)* ) => { Err(Box::<Error>::from(format!($($arg)*))) }
}

fn read_input() -> Result<Vec<(usize, usize)>> {
    let mut contents = String::new();
    io::stdin().read_to_string(&mut contents)?;

    contents
        .lines()
        .map(|coord| {
            let split: Vec<&str> = coord.split(',').collect();
            match &split[..] {
                [x, y] => Ok((x.trim().parse()?, y.trim().parse()?)),
                _ => return err!("unable to parse coordinate: {}", coord),
            }
        })
        .collect()
}

fn distance((x1, y1): (usize, usize), (x2, y2): (usize, usize)) -> usize {
    let diff = |x: usize, y: usize| x.checked_sub(y).unwrap_or(y - x);

    diff(x1, x2) + diff(y1, y2)
}

fn find_nearest(coords: &[(usize, usize)], x: usize, y: usize) -> Option<usize> {
    let distances: Vec<_> = coords
        .iter()
        .map(|&coord| distance(coord, (x, y)))
        .collect();

    let min = distances.iter().min().unwrap();

    let smallest: Vec<_> = distances
        .iter()
        .enumerate()
        .filter(|(_, x)| *x == min)
        .collect();

    match smallest.len() {
        1 => Some(smallest[0].0),
        _ => None,
    }
}

enum Area {
    Finite(usize),
    Infinite,
}

type Grid = Vec<Vec<Option<usize>>>;

fn on_boundary(grid: &Grid, max_x: usize, max_y: usize, i: usize) -> bool {
    ((0..max_x).zip(repeat(0)))
        .chain((0..max_x).zip(repeat(max_y - 1)))
        .chain(repeat(0).zip(0..max_y))
        .chain(repeat(max_x - 1).zip(0..max_y))
        .any(|(x, y)| grid[x][y] == Some(i))
}

fn calculate_area(grid: &Grid, max_x: usize, max_y: usize, i: usize) -> Area {
    if on_boundary(grid, max_x, max_y, i) {
        Area::Infinite
    } else {
        Area::Finite(
            grid.iter()
                .flat_map(|row| row.iter())
                .filter(|x| x.map(|x| x == i).unwrap_or(false))
                .count(),
        )
    }
}

fn part1(coords: &[(usize, usize)]) -> usize {
    let max_x = coords.iter().map(|c| c.0).max().unwrap() + 1;
    let max_y = coords.iter().map(|c| c.1).max().unwrap() + 1;

    let mut grid = vec![vec![None; max_y]; max_x];

    for (x, row) in grid.iter_mut().enumerate() {
        for (y, cell) in row.iter_mut().enumerate() {
            *cell = find_nearest(&coords, x, y);
        }
    }

    coords
        .iter()
        .enumerate()
        .map(|(i, _)| calculate_area(&grid, max_x, max_y, i))
        .filter_map(|area| match area {
            Area::Infinite => None,
            Area::Finite(area) => Some(area),
        })
        .max()
        .unwrap()
}

fn part2(coords: &[(usize, usize)]) -> usize {
    let max_x = coords.iter().map(|c| c.0).max().unwrap() + 1;
    let max_y = coords.iter().map(|c| c.1).max().unwrap() + 1;

    let mut grid = vec![vec![std::usize::MAX; max_y]; max_x];

    for (x, row) in grid.iter_mut().enumerate() {
        for (y, cell) in row.iter_mut().enumerate() {
            *cell = repeat((x, y))
                .zip(coords)
                .map(|(x, y)| distance(x, *y))
                .sum()
        }
    }

    grid.iter()
        .flat_map(|it| it.iter())
        .filter(|x| **x < 10000)
        .count()
}

fn main() -> Result<()> {
    let input = read_input()?;

    writeln!(io::stdout(), "{:?}", part1(&input))?;
    writeln!(io::stdout(), "{:?}", part2(&input))?;
    Ok(())
}
