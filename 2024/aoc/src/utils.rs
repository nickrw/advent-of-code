use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn parse_input_lines<T>(filename: &str, line_parser: fn(String) -> T) -> impl Iterator<Item=T> {
    let lines: Vec<T> = std::fs::read_to_string(filename)
        .expect("should be able to read from file")
        .lines()
        .map(String::from)
        .map(line_parser)
        .collect();
    lines.into_iter()
}

pub fn parse_input_lines_stream<T: 'static>(filename: &str, line_parser: fn(String) -> T) -> Result<impl Iterator<Item=Result<T, std::io::Error>>, std::io::Error> {
    let fh = File::open(filename)?;
    let iterator =  BufReader::new(fh)
        .lines()
        .map(move |line_result| line_result.map(line_parser));
    Ok(iterator)
}