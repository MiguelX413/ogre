use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

fn main() {
    let args = args().collect::<Vec<_>>();
    let path = Path::new(&args[1]);
    let file = File::open(path).unwrap();
    let buf = BufReader::new(file);
    let lines = buf.lines();
    lines.for_each(|f| f.unwrap().split_whitespace().for_each(|x| println!("{x}")));
}
