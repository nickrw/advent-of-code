use aoc::utils::{parse_input_lines, parse_input_lines_stream};

fn main() {
    aoc2024_day2_part1()
}

struct Report {
    raw: Vec<isize>
}

enum Direction {
    NoneYet,
    Increasing,
    Decreasing,
}

impl Report {
    fn is_valid(&self) -> bool {
        let mut direction = Direction::NoneYet;
        for i in 1..self.raw.len() {
            let prev = *self.raw.get(i-1).unwrap();
            let this = *self.raw.get(i).unwrap();
            match direction {
                Direction::NoneYet => {
                    if this > prev && this < prev + 4 {
                        direction = Direction::Increasing;
                    } else if this < prev && this > prev - 4 {
                        direction = Direction::Decreasing;
                    } else {
                        return false;
                    }
                },
                Direction::Increasing => {
                    if this <= prev {
                        return false
                    }
                    if this >= prev + 4 {
                        return false
                    }
                }
                Direction::Decreasing => {
                    if this >= prev {
                        return false
                    }
                    if this <= prev - 4 {
                        return false
                    }
                }
            }
        };
        return true
    }

}

fn aoc2024_day2_part1() {
    let input = parse_input_lines_stream("input_day2.txt", line_parser)
        .expect("input parse fail")
        .filter_map(|v| v.ok() );
    let result = input.filter(|rep| rep.is_valid()).count();
    println!("result: {}", result);
}

fn line_parser(line: String) -> Report {
    let raw = line.split_ascii_whitespace()
        .map(|text| text.parse::<isize>().unwrap())
        .collect();
    Report{ raw }
}
