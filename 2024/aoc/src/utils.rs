
pub fn parse_input<T>(filename: &str, line_parser: fn(String) -> T) -> impl Iterator<Item=T> {
    let lines: Vec<T> = std::fs::read_to_string(filename)
        .expect("should be able to read from file")
        .lines()
        .map(String::from)
        .map(line_parser)
        .collect();
    lines.into_iter()
}