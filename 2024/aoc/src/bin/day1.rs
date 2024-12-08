use std::cmp::{max, min};
use std::collections::HashMap;
use aoc::utils::parse_input;

fn main() {
    aoc2024_day1_part2()
}

fn aoc2024_day1_part1() {
    let (mut list_a, mut list_b): (Vec<usize>, Vec<usize>) = parse_input("input_day1.txt", parse_line).unzip();
    list_a.sort();
    list_b.sort();
    let iterable = list_a.iter().zip(list_b.iter());
    let result = iterable
        .map(|(a, b)| max(a, b) - min(a, b) )
        .sum::<usize>()
    ;
    println!("result: {}", result);
}

fn aoc2024_day1_part2() {
    let (mut list_a, mut list_b): (Vec<usize>, Vec<usize>) = parse_input("input_day1.txt", parse_line).unzip();
    let list_b = list_b.iter().fold(HashMap::new(), |mut acc, val| {
        *acc.entry(*val).or_insert(0) += 1;
        acc
    });
    let result = list_a.iter()
        .map(|x| match list_b.get(&x) {
            None => 0,
            Some(y) => x * y,
        })
        .sum::<usize>()
        ;
    println!("result: {}", result);
}


fn parse_line(line: String) -> (usize, usize) {
    let a = &line[0..5];
    let a = a.parse::<usize>().expect("failed to parse first number");
    let b = &line[8..];
    let b = b.parse::<usize>().expect("failed to parse second number");
    (a, b)
}